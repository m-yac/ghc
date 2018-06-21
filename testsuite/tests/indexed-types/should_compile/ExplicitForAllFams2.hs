{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module ExplicitForAllFams2 where

-- Even more tests

type family CF a b where
  forall x y.      CF [x] (Maybe y) = Double
  forall (z :: *). CF z   z         = Bool
  forall.          CF _   _         = ()

type family OF a
type instance forall a b. OF (Maybe a, Either a b) = Bool

data family DF a
data instance forall a b. DF (Maybe a, Either a b) = DF a a b

data family NF a
newtype instance forall a b. NF (Maybe a, Either a b) = NF { unNF :: Bool }

class Cl a where
  type AT a b
  data AD a b
instance forall a. Cl (Maybe a) where
  type forall b. AT (Maybe a) b = Int
  data forall b. AD (Maybe a) b = AD b
