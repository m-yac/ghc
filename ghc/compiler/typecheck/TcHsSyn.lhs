%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcHsSyn]{Specialisations of the @HsSyn@ syntax for the typechecker}

This module is an extension of @HsSyn@ syntax, for use in the type
checker.

\begin{code}
module TcHsSyn (
	mkHsTyApp, mkHsDictApp, mkHsConApp,
	mkHsTyLam, mkHsDictLam, mkHsLet, mkHsApp,
	hsLitType, hsPatType, mkHsAppTy, mkSimpleHsAlt,
	nlHsIntLit, glueBindsOnGRHSs,
	

	-- Coercions
	Coercion, ExprCoFn, PatCoFn, 
	(<$>), (<.>), mkCoercion, 
	idCoercion, isIdCoercion,

	-- re-exported from TcMonad
	TcId, TcIdSet, TcDictBinds,

	zonkTopDecls, zonkTopExpr, zonkTopLExpr,
	zonkId, zonkTopBndrs
  ) where

#include "HsVersions.h"

-- friends:
import HsSyn	-- oodles of it

-- others:
import Id	( idType, setIdType, Id )

import TcRnMonad
import Type	  ( Type )
import TcType	  ( TcType, TcTyVar, mkTyVarTy, mkTyConApp, isImmutableTyVar, tcGetTyVar )
import Kind	  ( isLiftedTypeKind, liftedTypeKind, isSubKind )
import qualified  Type
import TcMType	  ( zonkQuantifiedTyVar, zonkType, zonkTcType, zonkTcTyVars, putMetaTyVar )
import TysPrim	  ( charPrimTy, intPrimTy, floatPrimTy,
		    doublePrimTy, addrPrimTy
		  )
import TysWiredIn ( charTy, stringTy, intTy, 
		    mkListTy, mkPArrTy, mkTupleTy, unitTy,
		    voidTy, listTyCon, tupleTyCon )
import TyCon	  ( mkPrimTyCon, tyConKind, PrimRep(..) )
import Kind	  ( splitKindFunTys )
import Name	  ( getOccName, mkInternalName, mkDerivedTyConOcc )
import Var	  ( Var, isId, isLocalVar, tyVarKind )
import VarSet
import VarEnv
import BasicTypes ( Boxity(..), IPName(..), ipNameName, mapIPName )
import Maybes	  ( orElse )
import Maybe  	  ( isNothing )
import Unique	  ( Uniquable(..) )
import SrcLoc	  ( noSrcLoc, noLoc, Located(..), unLoc )
import Bag
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Note: If @hsPatType@ doesn't bear a strong resemblance to @exprType@,
then something is wrong.
\begin{code}
hsPatType :: OutPat Id -> Type
hsPatType pat = pat_type (unLoc pat)

pat_type (ParPat pat)		   = hsPatType pat
pat_type (WildPat ty)		   = ty
pat_type (VarPat var)		   = idType var
pat_type (VarPatOut var _)	   = idType var
pat_type (LazyPat pat)		   = hsPatType pat
pat_type (LitPat lit)		   = hsLitType lit
pat_type (AsPat var pat)	   = idType (unLoc var)
pat_type (ListPat _ ty)		   = mkListTy ty
pat_type (PArrPat _ ty)		   = mkPArrTy ty
pat_type (TuplePat pats box)	   = mkTupleTy box (length pats) (map hsPatType pats)
pat_type (ConPatOut _ _ _ _ _ ty)  = ty
pat_type (SigPatOut pat ty)	   = ty
pat_type (NPatOut lit ty _)	   = ty
pat_type (NPlusKPatOut id _ _ _)   = idType (unLoc id)
pat_type (DictPat ds ms)           = case (ds ++ ms) of
				       []  -> unitTy
				       [d] -> idType d
				       ds  -> mkTupleTy Boxed (length ds) (map idType ds)


hsLitType :: HsLit -> TcType
hsLitType (HsChar c)       = charTy
hsLitType (HsCharPrim c)   = charPrimTy
hsLitType (HsString str)   = stringTy
hsLitType (HsStringPrim s) = addrPrimTy
hsLitType (HsInt i)	   = intTy
hsLitType (HsIntPrim i)    = intPrimTy
hsLitType (HsInteger i ty) = ty
hsLitType (HsRat _ ty)	   = ty
hsLitType (HsFloatPrim f)  = floatPrimTy
hsLitType (HsDoublePrim d) = doublePrimTy
\end{code}

%************************************************************************
%*									*
\subsection{Coercion functions}
%*									*
%************************************************************************

\begin{code}
type Coercion a = Maybe (a -> a)
	-- Nothing => identity fn

type ExprCoFn = Coercion (HsExpr TcId)
type PatCoFn  = Coercion (Pat    TcId)

(<.>) :: Coercion a -> Coercion a -> Coercion a	-- Composition
Nothing <.> Nothing = Nothing
Nothing <.> Just f  = Just f
Just f  <.> Nothing = Just f
Just f1 <.> Just f2 = Just (f1 . f2)

(<$>) :: Coercion a -> a -> a
Just f  <$> e = f e
Nothing <$> e = e

mkCoercion :: (a -> a) -> Coercion a
mkCoercion f = Just f

idCoercion :: Coercion a
idCoercion = Nothing

isIdCoercion :: Coercion a -> Bool
isIdCoercion = isNothing
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

\begin{code}
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = zonkTcType (idType id) `thenM` \ ty' ->
    returnM (setIdType id ty')
\end{code}

The rest of the zonking is done *after* typechecking.
The main zonking pass runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void
 c) convert each TcId to an Id by zonking its type

The type variables are converted by binding mutable tyvars to immutable ones
and then zonking as normal.

The Ids are converted by binding them in the normal Tc envt; that
way we maintain sharing; eg an Id is zonked at its binding site and they
all occurrences of that Id point to the common zonked copy

It's all pretty boring stuff, because HsSyn is such a large type, and 
the environment manipulation is tiresome.

\begin{code}
data ZonkEnv = ZonkEnv	(TcType -> TcM Type) 	-- How to zonk a type
			(IdEnv Id)		-- What variables are in scope
	-- Maps an Id to its zonked version; both have the same Name
	-- Is only consulted lazily; hence knot-tying

emptyZonkEnv = ZonkEnv zonkTypeZapping emptyVarEnv

extendZonkEnv :: ZonkEnv -> [Id] -> ZonkEnv
extendZonkEnv (ZonkEnv zonk_ty env) ids 
  = ZonkEnv zonk_ty (extendVarEnvList env [(id,id) | id <- ids])

extendZonkEnv1 :: ZonkEnv -> Id -> ZonkEnv
extendZonkEnv1 (ZonkEnv zonk_ty env) id 
  = ZonkEnv zonk_ty (extendVarEnv env id id)

setZonkType :: ZonkEnv -> (TcType -> TcM Type) -> ZonkEnv
setZonkType (ZonkEnv _ env) zonk_ty = ZonkEnv zonk_ty env

zonkEnvIds :: ZonkEnv -> [Id]
zonkEnvIds (ZonkEnv _ env) = varEnvElts env

zonkIdOcc :: ZonkEnv -> TcId -> Id
-- Ids defined in this module should be in the envt; 
-- ignore others.  (Actually, data constructors are also
-- not LocalVars, even when locally defined, but that is fine.)
--
-- Actually, Template Haskell works in 'chunks' of declarations, and
-- an earlier chunk won't be in the 'env' that the zonking phase 
-- carries around.  Instead it'll be in the tcg_gbl_env, already fully
-- zonked.  There's no point in looking it up there (except for error 
-- checking), and it's not conveniently to hand; hence the simple
-- 'orElse' case in the LocalVar branch.
--
-- Even without template splices, in module Main, the checking of
-- 'main' is done as a separte chunk.
zonkIdOcc (ZonkEnv zonk_ty env) id 
  | isLocalVar id = lookupVarEnv env id `orElse` id
  | otherwise	  = id

zonkIdOccs env ids = map (zonkIdOcc env) ids

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give 
zonkIdBndr :: ZonkEnv -> TcId -> TcM Id
zonkIdBndr env id
  = zonkTcTypeToType env (idType id)	`thenM` \ ty' ->
    returnM (setIdType id ty')

zonkIdBndrs :: ZonkEnv -> [TcId] -> TcM [Id]
zonkIdBndrs env ids = mappM (zonkIdBndr env) ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = zonkIdBndrs emptyZonkEnv ids
\end{code}


\begin{code}
zonkTopExpr :: HsExpr TcId -> TcM (HsExpr Id)
zonkTopExpr e = zonkExpr emptyZonkEnv e

zonkTopLExpr :: LHsExpr TcId -> TcM (LHsExpr Id)
zonkTopLExpr e = zonkLExpr emptyZonkEnv e

zonkTopDecls :: LHsBinds TcId -> [LRuleDecl TcId] -> [LForeignDecl TcId]
	     -> TcM ([Id], 
		     Bag (LHsBind  Id),
		     [LForeignDecl Id],
		     [LRuleDecl    Id])
zonkTopDecls binds rules fords
  = do	{ (env, binds') <- zonkRecMonoBinds emptyZonkEnv binds
			-- Top level is implicitly recursive
	; rules' <- zonkRules env rules
	; fords' <- zonkForeignExports env fords
	; return (zonkEnvIds env, binds', fords', rules') }

---------------------------------------------
zonkGroup :: ZonkEnv -> HsBindGroup TcId -> TcM (ZonkEnv, HsBindGroup Id)
zonkGroup env (HsBindGroup bs sigs is_rec)
  = ASSERT( null sigs )
    do  { (env1, bs') <- zonkRecMonoBinds env bs
        ; return (env1, HsBindGroup bs' [] is_rec) }
 
zonkGroup env (HsIPBinds binds)
  = mappM (wrapLocM zonk_ip_bind) binds	`thenM` \ new_binds ->
    let
	env1 = extendZonkEnv env [ipNameName n | L _ (IPBind n _) <- new_binds]
    in
    returnM (env1, HsIPBinds new_binds)
  where
    zonk_ip_bind (IPBind n e)
	= mapIPNameTc (zonkIdBndr env) n	`thenM` \ n' ->
	  zonkLExpr env e			`thenM` \ e' ->
	  returnM (IPBind n' e')

---------------------------------------------
zonkNestedBinds :: ZonkEnv -> [HsBindGroup TcId] -> TcM (ZonkEnv, [HsBindGroup Id])
zonkNestedBinds env []     = return (env, [])
zonkNestedBinds env (b:bs) = do	{ (env1, b') <- zonkGroup env b
				; (env2, bs') <- zonkNestedBinds env1 bs
				; return (env2, b':bs') }

---------------------------------------------
zonkRecMonoBinds :: ZonkEnv -> LHsBinds TcId -> TcM (ZonkEnv, LHsBinds Id)
zonkRecMonoBinds env binds 
 = fixM (\ ~(_, new_binds) -> do 
	{ let env1 = extendZonkEnv env (collectHsBindBinders new_binds)
        ; binds' <- zonkMonoBinds env1 binds
        ; return (env1, binds') })

zonkMonoBinds :: ZonkEnv -> LHsBinds TcId -> TcM (LHsBinds Id)
zonkMonoBinds env binds = mapBagM (wrapLocM (zonk_bind env)) binds

zonk_bind :: ZonkEnv -> HsBind TcId -> TcM (HsBind Id)
zonk_bind env (PatBind pat grhss ty)
  = do	{ (_env, new_pat) <- zonkPat env pat		-- Env already extended
	; new_grhss <- zonkGRHSs env grhss
	; new_ty    <- zonkTcTypeToType env ty
	; return (PatBind new_pat new_grhss new_ty) }

zonk_bind env (VarBind var expr)
  = zonkIdBndr env var 			`thenM` \ new_var ->
    zonkLExpr env expr			`thenM` \ new_expr ->
    returnM (VarBind new_var new_expr)

zonk_bind env (FunBind var inf ms)
  = wrapLocM (zonkIdBndr env) var	`thenM` \ new_var ->
    zonkMatchGroup env ms		`thenM` \ new_ms ->
    returnM (FunBind new_var inf new_ms)

zonk_bind env (AbsBinds tyvars dicts exports inlines val_binds)
  = ASSERT( all isImmutableTyVar tyvars )
    zonkIdBndrs env dicts		`thenM` \ new_dicts ->
    fixM (\ ~(new_val_binds, _) ->
	let
	  env1 = extendZonkEnv (extendZonkEnv env new_dicts) 
			       (collectHsBindBinders new_val_binds)
	in
	zonkMonoBinds env1 val_binds 		`thenM` \ new_val_binds ->
        mappM (zonkExport env1) exports		`thenM` \ new_exports ->
	returnM (new_val_binds, new_exports)
    )						`thenM` \ (new_val_bind, new_exports) ->
    returnM (AbsBinds tyvars new_dicts new_exports inlines new_val_bind)
  where
    zonkExport env (tyvars, global, local)
	= zonkTcTyVars tyvars		`thenM` \ tys ->
	  let
		new_tyvars = map (tcGetTyVar "zonkExport") tys
		-- This isn't the binding occurrence of these tyvars
		-- but they should *be* tyvars.  Hence tcGetTyVar.
	  in
	  zonkIdBndr env global		`thenM` \ new_global ->
	  returnM (new_tyvars, new_global, zonkIdOcc env local)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
%*									*
%************************************************************************

\begin{code}
zonkMatchGroup :: ZonkEnv -> MatchGroup TcId-> TcM (MatchGroup Id)
zonkMatchGroup env (MatchGroup ms ty) 
  = do	{ ms' <- mapM (zonkMatch env) ms
	; ty' <- zonkTcTypeToType env ty
	; return (MatchGroup ms' ty') }

zonkMatch :: ZonkEnv -> LMatch TcId-> TcM (LMatch Id)
zonkMatch env (L loc (Match pats _ grhss))
  = do	{ (env1, new_pats) <- zonkPats env pats
	; new_grhss <- zonkGRHSs env1 grhss
	; return (L loc (Match new_pats Nothing new_grhss)) }

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv -> GRHSs TcId -> TcM (GRHSs Id)

zonkGRHSs env (GRHSs grhss binds)
  = zonkNestedBinds env binds   	`thenM` \ (new_env, new_binds) ->
    let
	zonk_grhs (GRHS guarded)
	  = zonkStmts new_env guarded	`thenM` \ new_guarded ->
	    returnM (GRHS new_guarded)
    in
    mappM (wrapLocM zonk_grhs) grhss 	`thenM` \ new_grhss ->
    returnM (GRHSs new_grhss new_binds)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

\begin{code}
zonkLExprs :: ZonkEnv -> [LHsExpr TcId] -> TcM [LHsExpr Id]
zonkLExpr  :: ZonkEnv -> LHsExpr TcId   -> TcM (LHsExpr Id)
zonkExpr   :: ZonkEnv -> HsExpr TcId    -> TcM (HsExpr Id)

zonkLExprs env exprs = mappM (zonkLExpr env) exprs
zonkLExpr  env expr  = wrapLocM (zonkExpr env) expr

zonkExpr env (HsVar id)
  = returnM (HsVar (zonkIdOcc env id))

zonkExpr env (HsIPVar id)
  = returnM (HsIPVar (mapIPName (zonkIdOcc env) id))

zonkExpr env (HsLit (HsRat f ty))
  = zonkTcTypeToType env ty	   `thenM` \ new_ty  ->
    returnM (HsLit (HsRat f new_ty))

zonkExpr env (HsLit lit)
  = returnM (HsLit lit)
-- HsOverLit doesn't appear in typechecker output

zonkExpr env (HsLam matches)
  = zonkMatchGroup env matches	`thenM` \ new_matches ->
    returnM (HsLam new_matches)

zonkExpr env (HsApp e1 e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (HsApp new_e1 new_e2)

zonkExpr env (HsBracketOut body bs) 
  = mappM zonk_b bs	`thenM` \ bs' ->
    returnM (HsBracketOut body bs')
  where
    zonk_b (n,e) = zonkLExpr env e	`thenM` \ e' ->
		   returnM (n,e')

zonkExpr env (HsSpliceE s) = WARN( True, ppr s )	-- Should not happen
			     returnM (HsSpliceE s)

zonkExpr env (OpApp e1 op fixity e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env op	`thenM` \ new_op ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (OpApp new_e1 new_op fixity new_e2)

zonkExpr env (NegApp _ _) = panic "zonkExpr env: NegApp"

zonkExpr env (HsPar e)    
  = zonkLExpr env e	`thenM` \new_e ->
    returnM (HsPar new_e)

zonkExpr env (SectionL expr op)
  = zonkLExpr env expr	`thenM` \ new_expr ->
    zonkLExpr env op		`thenM` \ new_op ->
    returnM (SectionL new_expr new_op)

zonkExpr env (SectionR op expr)
  = zonkLExpr env op		`thenM` \ new_op ->
    zonkLExpr env expr		`thenM` \ new_expr ->
    returnM (SectionR new_op new_expr)

-- gaw 2004
zonkExpr env (HsCase expr ms)
  = zonkLExpr env expr    	`thenM` \ new_expr ->
    zonkMatchGroup env ms	`thenM` \ new_ms ->
    returnM (HsCase new_expr new_ms)

zonkExpr env (HsIf e1 e2 e3)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    zonkLExpr env e3	`thenM` \ new_e3 ->
    returnM (HsIf new_e1 new_e2 new_e3)

zonkExpr env (HsLet binds expr)
  = zonkNestedBinds env binds	`thenM` \ (new_env, new_binds) ->
    zonkLExpr new_env expr	`thenM` \ new_expr ->
    returnM (HsLet new_binds new_expr)

zonkExpr env (HsDo do_or_lc stmts ids ty)
  = zonkStmts env stmts 	`thenM` \ new_stmts ->
    zonkTcTypeToType env ty	`thenM` \ new_ty   ->
    zonkReboundNames env ids	`thenM` \ new_ids ->
    returnM (HsDo do_or_lc new_stmts new_ids new_ty)

zonkExpr env (ExplicitList ty exprs)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkLExprs env exprs	`thenM` \ new_exprs ->
    returnM (ExplicitList new_ty new_exprs)

zonkExpr env (ExplicitPArr ty exprs)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkLExprs env exprs	`thenM` \ new_exprs ->
    returnM (ExplicitPArr new_ty new_exprs)

zonkExpr env (ExplicitTuple exprs boxed)
  = zonkLExprs env exprs  	`thenM` \ new_exprs ->
    returnM (ExplicitTuple new_exprs boxed)

zonkExpr env (RecordConOut data_con con_expr rbinds)
  = zonkLExpr env con_expr	`thenM` \ new_con_expr ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordConOut data_con new_con_expr new_rbinds)

zonkExpr env (RecordUpd _ _) = panic "zonkExpr env:RecordUpd"

zonkExpr env (RecordUpdOut expr in_ty out_ty rbinds)
  = zonkLExpr env expr		`thenM` \ new_expr ->
    zonkTcTypeToType env in_ty	`thenM` \ new_in_ty ->
    zonkTcTypeToType env out_ty	`thenM` \ new_out_ty ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordUpdOut new_expr new_in_ty new_out_ty new_rbinds)

zonkExpr env (ExprWithTySigOut e ty) 
  = do { e' <- zonkLExpr env e
       ; return (ExprWithTySigOut e' ty) }

zonkExpr env (ExprWithTySig _ _) = panic "zonkExpr env:ExprWithTySig"
zonkExpr env (ArithSeqIn _)      = panic "zonkExpr env:ArithSeqIn"
zonkExpr env (PArrSeqIn _)       = panic "zonkExpr env:PArrSeqIn"

zonkExpr env (ArithSeqOut expr info)
  = zonkLExpr env expr		`thenM` \ new_expr ->
    zonkArithSeq env info	`thenM` \ new_info ->
    returnM (ArithSeqOut new_expr new_info)

zonkExpr env (PArrSeqOut expr info)
  = zonkLExpr env expr		`thenM` \ new_expr ->
    zonkArithSeq env info	`thenM` \ new_info ->
    returnM (PArrSeqOut new_expr new_info)

zonkExpr env (HsSCC lbl expr)
  = zonkLExpr env expr	`thenM` \ new_expr ->
    returnM (HsSCC lbl new_expr)

-- hdaume: core annotations
zonkExpr env (HsCoreAnn lbl expr)
  = zonkLExpr env expr   `thenM` \ new_expr ->
    returnM (HsCoreAnn lbl new_expr)

zonkExpr env (TyLam tyvars expr)
  = ASSERT( all isImmutableTyVar tyvars )
    zonkLExpr env expr			`thenM` \ new_expr ->
    returnM (TyLam tyvars new_expr)

zonkExpr env (TyApp expr tys)
  = zonkLExpr env expr    	 	`thenM` \ new_expr ->
    mappM (zonkTcTypeToType env) tys	`thenM` \ new_tys ->
    returnM (TyApp new_expr new_tys)

zonkExpr env (DictLam dicts expr)
  = zonkIdBndrs env dicts	`thenM` \ new_dicts ->
    let
	env1 = extendZonkEnv env new_dicts
    in
    zonkLExpr env1 expr  	`thenM` \ new_expr ->
    returnM (DictLam new_dicts new_expr)

zonkExpr env (DictApp expr dicts)
  = zonkLExpr env expr    	    	`thenM` \ new_expr ->
    returnM (DictApp new_expr (zonkIdOccs env dicts))

-- arrow notation extensions
zonkExpr env (HsProc pat body)
  = do	{ (env1, new_pat) <- zonkPat env pat
	; new_body <- zonkCmdTop env1 body
	; return (HsProc new_pat new_body) }

zonkExpr env (HsArrApp e1 e2 ty ho rl)
  = zonkLExpr env e1	    	    	`thenM` \ new_e1 ->
    zonkLExpr env e2	    	    	`thenM` \ new_e2 ->
    zonkTcTypeToType env ty 		`thenM` \ new_ty ->
    returnM (HsArrApp new_e1 new_e2 new_ty ho rl)

zonkExpr env (HsArrForm op fixity args)
  = zonkLExpr env op	    	    	`thenM` \ new_op ->
    mappM (zonkCmdTop env) args		`thenM` \ new_args ->
    returnM (HsArrForm new_op fixity new_args)

zonkCmdTop :: ZonkEnv -> LHsCmdTop TcId -> TcM (LHsCmdTop Id)
zonkCmdTop env cmd = wrapLocM (zonk_cmd_top env) cmd

zonk_cmd_top env (HsCmdTop cmd stack_tys ty ids)
  = zonkLExpr env cmd	    	    	`thenM` \ new_cmd ->
    mappM (zonkTcTypeToType env) stack_tys
			 		`thenM` \ new_stack_tys ->
    zonkTcTypeToType env ty 		`thenM` \ new_ty ->
    zonkReboundNames env ids		`thenM` \ new_ids ->
    returnM (HsCmdTop new_cmd new_stack_tys new_ty new_ids)

-------------------------------------------------------------------------
zonkReboundNames :: ZonkEnv -> ReboundNames Id -> TcM (ReboundNames Id)
zonkReboundNames env prs 
  = mapM zonk prs
  where
    zonk (n, e) = zonkExpr env e `thenM` \ new_e ->
		  returnM (n, new_e)


-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> ArithSeqInfo TcId -> TcM (ArithSeqInfo Id)

zonkArithSeq env (From e)
  = zonkLExpr env e		`thenM` \ new_e ->
    returnM (From new_e)

zonkArithSeq env (FromThen e1 e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (FromThen new_e1 new_e2)

zonkArithSeq env (FromTo e1 e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (FromTo new_e1 new_e2)

zonkArithSeq env (FromThenTo e1 e2 e3)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    zonkLExpr env e3	`thenM` \ new_e3 ->
    returnM (FromThenTo new_e1 new_e2 new_e3)


-------------------------------------------------------------------------
zonkStmts  :: ZonkEnv -> [LStmt TcId] -> TcM [LStmt Id]

zonkStmts env stmts = zonk_stmts env stmts	`thenM` \ (_, stmts) ->
		      returnM stmts

zonk_stmts :: ZonkEnv -> [LStmt TcId] -> TcM (ZonkEnv, [LStmt Id])
zonk_stmts env []     = return (env, [])
zonk_stmts env (s:ss) = do { (env1, s')  <- wrapLocSndM (zonkStmt env) s
			   ; (env2, ss') <- zonk_stmts env1 ss
			   ; return (env2, s' : ss') }

zonkStmt :: ZonkEnv -> Stmt TcId -> TcM (ZonkEnv, Stmt Id)
zonkStmt env (ParStmt stmts_w_bndrs)
  = mappM zonk_branch stmts_w_bndrs	`thenM` \ new_stmts_w_bndrs ->
    let 
	new_binders = concat (map snd new_stmts_w_bndrs)
	env1 = extendZonkEnv env new_binders
    in
    return (env1, ParStmt new_stmts_w_bndrs)
  where
    zonk_branch (stmts, bndrs) = zonk_stmts env stmts	`thenM` \ (env1, new_stmts) ->
				 returnM (new_stmts, zonkIdOccs env1 bndrs)

zonkStmt env (RecStmt segStmts lvs rvs rets)
  = zonkIdBndrs env rvs		`thenM` \ new_rvs ->
    let
	env1 = extendZonkEnv env new_rvs
    in
    zonk_stmts env1 segStmts	`thenM` \ (env2, new_segStmts) ->
	-- Zonk the ret-expressions in an envt that 
	-- has the polymorphic bindings in the envt
    zonkLExprs env2 rets	`thenM` \ new_rets ->
    let
	new_lvs = zonkIdOccs env2 lvs
	env3 = extendZonkEnv env new_lvs	-- Only the lvs are needed
    in
    returnM (env3, RecStmt new_segStmts new_lvs new_rvs new_rets)

zonkStmt env (ResultStmt expr)
  = zonkLExpr env expr	`thenM` \ new_expr ->
    returnM (env, ResultStmt new_expr)

zonkStmt env (ExprStmt expr ty)
  = zonkLExpr env expr		`thenM` \ new_expr ->
    zonkTcTypeToType env ty	`thenM` \ new_ty ->
    returnM (env, ExprStmt new_expr new_ty)

zonkStmt env (LetStmt binds)
  = zonkNestedBinds env binds	`thenM` \ (env1, new_binds) ->
    returnM (env1, LetStmt new_binds)

zonkStmt env (BindStmt pat expr)
  = do	{ new_expr <- zonkLExpr env expr
	; (env1, new_pat) <- zonkPat env pat
	; return (env1, BindStmt new_pat new_expr) }


-------------------------------------------------------------------------
zonkRbinds :: ZonkEnv -> HsRecordBinds TcId -> TcM (HsRecordBinds Id)

zonkRbinds env rbinds
  = mappM zonk_rbind rbinds
  where
    zonk_rbind (field, expr)
      = zonkLExpr env expr	`thenM` \ new_expr ->
	returnM (fmap (zonkIdOcc env) field, new_expr)

-------------------------------------------------------------------------
mapIPNameTc :: (a -> TcM b) -> IPName a -> TcM (IPName b)
mapIPNameTc f (Dupable n) = f n  `thenM` \ r -> returnM (Dupable r)
mapIPNameTc f (Linear  n) = f n  `thenM` \ r -> returnM (Linear r)
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: ZonkEnv -> OutPat TcId -> TcM (ZonkEnv, OutPat Id)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat env pat = wrapLocSndM (zonk_pat env) pat

zonk_pat env (ParPat p)
  = do	{ (env', p') <- zonkPat env p
  	; return (env', ParPat p') }

zonk_pat env (WildPat ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; return (env, WildPat ty') }

zonk_pat env (VarPat v)
  = do	{ v' <- zonkIdBndr env v
	; return (extendZonkEnv1 env v', VarPat v') }

zonk_pat env (VarPatOut v binds)
  = do	{ v' <- zonkIdBndr env v
	; (env', binds') <- zonkRecMonoBinds (extendZonkEnv1 env v') binds
  	; returnM (env', VarPatOut v' binds') }

zonk_pat env (LazyPat pat)
  = do	{ (env', pat') <- zonkPat env pat
	; return (env',  LazyPat pat') }

zonk_pat env (AsPat (L loc v) pat)
  = do	{ v' <- zonkIdBndr env v
	; (env', pat') <- zonkPat (extendZonkEnv1 env v') pat
 	; return (env', AsPat (L loc v') pat') }

zonk_pat env (ListPat pats ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pats') <- zonkPats env pats
	; return (env', ListPat pats' ty') }

zonk_pat env (PArrPat pats ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pats') <- zonkPats env pats
	; return (env', PArrPat pats' ty') }

zonk_pat env (TuplePat pats boxed)
  = do	{ (env', pats') <- zonkPats env pats
	; return (env', TuplePat pats' boxed) }

zonk_pat env (ConPatOut n tvs dicts binds stuff ty)
  = ASSERT( all isImmutableTyVar tvs )
    do	{ new_ty <- zonkTcTypeToType env ty
	; new_dicts <- zonkIdBndrs env dicts
	; let env1 = extendZonkEnv env new_dicts
	; (env2, new_binds) <- zonkRecMonoBinds env1 binds
	; (env', new_stuff) <- zonkConStuff env2 stuff
	; returnM (env', ConPatOut n tvs new_dicts new_binds new_stuff new_ty) }

zonk_pat env (LitPat lit) = return (env, LitPat lit)

zonk_pat env (SigPatOut pat ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pat') <- zonkPat env pat
	; return (env', SigPatOut pat' ty') }

zonk_pat env (NPatOut lit ty expr)
  = do	{ ty' <- zonkTcTypeToType env ty
 	; expr' <- zonkExpr env expr
	; return (env, NPatOut lit ty' expr') }

zonk_pat env (NPlusKPatOut (L loc n) k e1 e2)
  = do	{ n' <- zonkIdBndr env n
	; e1' <- zonkExpr env e1
	; e2' <- zonkExpr env e2
	; return (extendZonkEnv1 env n', NPlusKPatOut (L loc n') k e1' e2') }

zonk_pat env (DictPat ds ms)
  = do	{ ds' <- zonkIdBndrs env ds
	; ms' <- zonkIdBndrs env ms
	; return (extendZonkEnv env (ds' ++ ms'), DictPat ds' ms') }

---------------------------
zonkConStuff env (PrefixCon pats)
  = do	{ (env', pats') <- zonkPats env pats
	; return (env', PrefixCon pats') }

zonkConStuff env (InfixCon p1 p2)
  = do	{ (env1, p1') <- zonkPat env  p1
	; (env', p2') <- zonkPat env1 p2
	; return (env', InfixCon p1' p2') }

zonkConStuff env (RecCon rpats)
  = do	{ (env', pats') <- zonkPats env pats
	; returnM (env', RecCon (fields `zip` pats')) }
  where
    (fields, pats) = unzip rpats

---------------------------
zonkPats env []		= return (env, [])
zonkPats env (pat:pats) = do { (env1, pat') <- zonkPat env pat
			     ; (env', pats') <- zonkPats env1 pats
			     ; return (env', pat':pats') }
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************


\begin{code}
zonkForeignExports :: ZonkEnv -> [LForeignDecl TcId] -> TcM [LForeignDecl Id]
zonkForeignExports env ls = mappM (wrapLocM (zonkForeignExport env)) ls

zonkForeignExport :: ZonkEnv -> ForeignDecl TcId -> TcM (ForeignDecl Id)
zonkForeignExport env (ForeignExport i hs_ty spec isDeprec) =
   returnM (ForeignExport (fmap (zonkIdOcc env) i) undefined spec isDeprec)
zonkForeignExport env for_imp 
  = returnM for_imp	-- Foreign imports don't need zonking
\end{code}

\begin{code}
zonkRules :: ZonkEnv -> [LRuleDecl TcId] -> TcM [LRuleDecl Id]
zonkRules env rs = mappM (wrapLocM (zonkRule env)) rs

zonkRule :: ZonkEnv -> RuleDecl TcId -> TcM (RuleDecl Id)
zonkRule env (HsRule name act (vars::[RuleBndr TcId]) lhs rhs)
  = mappM zonk_bndr vars		`thenM` \ new_bndrs ->
    newMutVar emptyVarSet		`thenM` \ unbound_tv_set ->
    let
	env_rhs = extendZonkEnv env [id | b <- new_bndrs, let id = unLoc b, isId id]
	-- Type variables don't need an envt
	-- They are bound through the mutable mechanism

	env_lhs = setZonkType env_rhs (zonkTypeCollecting unbound_tv_set)
	-- We need to gather the type variables mentioned on the LHS so we can 
	-- quantify over them.  Example:
	--   data T a = C
	-- 
	--   foo :: T a -> Int
	--   foo C = 1
	--
	--   {-# RULES "myrule"  foo C = 1 #-}
	-- 
	-- After type checking the LHS becomes (foo a (C a))
	-- and we do not want to zap the unbound tyvar 'a' to (), because
	-- that limits the applicability of the rule.  Instead, we
	-- want to quantify over it!  
	--
	-- It's easiest to find the free tyvars here. Attempts to do so earlier
	-- are tiresome, because (a) the data type is big and (b) finding the 
	-- free type vars of an expression is necessarily monadic operation.
	--	(consider /\a -> f @ b, where b is side-effected to a)
    in
    zonkLExpr env_lhs lhs		`thenM` \ new_lhs ->
    zonkLExpr env_rhs rhs		`thenM` \ new_rhs ->

    readMutVar unbound_tv_set		`thenM` \ unbound_tvs ->
    let
	final_bndrs :: [Located Var]
	final_bndrs = map noLoc (varSetElems unbound_tvs) ++ new_bndrs
    in
    returnM (HsRule name act (map RuleBndr final_bndrs) new_lhs new_rhs)
		-- I hate this map RuleBndr stuff
  where
   zonk_bndr (RuleBndr v) 
	| isId (unLoc v) = wrapLocM (zonkIdBndr env)   v
	| otherwise      = ASSERT( isImmutableTyVar (unLoc v) )
			   return v
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************

\begin{code}
zonkTcTypeToType :: ZonkEnv -> TcType -> TcM Type
zonkTcTypeToType (ZonkEnv zonk_ty _) ty = zonk_ty ty

zonkTypeCollecting :: TcRef TyVarSet -> TcType -> TcM Type
-- This variant collects unbound type variables in a mutable variable
zonkTypeCollecting unbound_tv_set
  = zonkType zonk_unbound_tyvar True
  where
    zonk_unbound_tyvar tv 
	= zonkQuantifiedTyVar tv				`thenM` \ tv' ->
	  readMutVar unbound_tv_set				`thenM` \ tv_set ->
	  writeMutVar unbound_tv_set (extendVarSet tv_set tv')	`thenM_`
	  return (mkTyVarTy tv')

zonkTypeZapping :: TcType -> TcM Type
-- This variant is used for everything except the LHS of rules
-- It zaps unbound type variables to (), or some other arbitrary type
zonkTypeZapping ty 
  = zonkType zonk_unbound_tyvar True ty 
  where
	-- Zonk a mutable but unbound type variable to an arbitrary type
	-- We know it's unbound even though we don't carry an environment,
	-- because at the binding site for a type variable we bind the
	-- mutable tyvar to a fresh immutable one.  So the mutable store
	-- plays the role of an environment.  If we come across a mutable
	-- type variable that isn't so bound, it must be completely free.
    zonk_unbound_tyvar tv = do { putMetaTyVar tv ty; return ty }
			  where 
			    ty = mkArbitraryType tv


-- When the type checker finds a type variable with no binding,
-- which means it can be instantiated with an arbitrary type, it
-- usually instantiates it to Void.  Eg.
-- 
-- 	length []
-- ===>
-- 	length Void (Nil Void)
-- 
-- But in really obscure programs, the type variable might have
-- a kind other than *, so we need to invent a suitably-kinded type.
-- 
-- This commit uses
-- 	Void for kind *
-- 	List for kind *->*
-- 	Tuple for kind *->...*->*
-- 
-- which deals with most cases.  (Previously, it only dealt with
-- kind *.)   
-- 
-- In the other cases, it just makes up a TyCon with a suitable
-- kind.  If this gets into an interface file, anyone reading that
-- file won't understand it.  This is fixable (by making the client
-- of the interface file make up a TyCon too) but it is tiresome and
-- never happens, so I am leaving it 

mkArbitraryType :: TcTyVar -> Type
-- Make up an arbitrary type whose kind is the same as the tyvar.
-- We'll use this to instantiate the (unbound) tyvar.
mkArbitraryType tv 
  | liftedTypeKind `isSubKind` kind = voidTy		-- The vastly common case
  | otherwise			    = mkTyConApp tycon []
  where
    kind       = tyVarKind tv
    (args,res) = splitKindFunTys kind

    tycon | kind == tyConKind listTyCon 	--  *->*
	  = listTyCon				-- No tuples this size

	  | all isLiftedTypeKind args && isLiftedTypeKind res
	  = tupleTyCon Boxed (length args)	--  *-> ... ->*->*

	  | otherwise
	  = pprTrace "Urk! Inventing strangely-kinded void TyCon:" (ppr tc_name $$ ppr kind) $
	    mkPrimTyCon tc_name kind 0 [] VoidRep
		-- Same name as the tyvar, apart from making it start with a colon (sigh)
		-- I dread to think what will happen if this gets out into an 
		-- interface file.  Catastrophe likely.  Major sigh.

    tc_name = mkInternalName (getUnique tv) (mkDerivedTyConOcc (getOccName tv)) noSrcLoc
\end{code}
