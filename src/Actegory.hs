{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Actegory where
import Data.Functor.Contravariant (Contravariant, (>$))
import Data.Void (absurd, Void)

class MonoidalCategory c p i | c -> p, c -> i where
  none :: c i
  (|.|) :: c a -> c b -> c (p a b)
  infixr 1 |.|

constant :: forall a f p i . (Functor f, MonoidalCategory f p i) => a -> f a -- @forall@ for convenient use of TypeApplications extension
constant a = a <$ none

null :: forall a f p i . (Contravariant f, MonoidalCategory f p i) => i -> f a -- @forall@ for convenient use of TypeApplications extension
null a = a >$ none

empty :: forall a f p i. (Functor f, MonoidalCategory f p Void) => f a -- @forall@ for convenient use of TypeApplications extension
empty = absurd <$> none

class MonoidalCategory m p i => Actegory m p i f where
  (|>|) :: m a -> f b -> f (p a b)
  infixr 1 |>|
