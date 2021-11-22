module Quartet
  ( CollapseP2P (nothing, (|&|))
  , constant
  , null
  , CollapseP2S (never, (|||))
  , empty
  , ExpandS2P (foo, expand)
  , Static (..)
  ) where

import           Data.Functor.Contravariant
import           Data.Void
import           Prelude                    hiding (null)

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- laws:
-- u |&| nothing ~= u
-- nothing |&| u ~= u
-- (u |&| w) |&| z ~= u |&| (w |&| z)
class CollapseP2P f where
  nothing :: f ()
  (|&|) :: f a -> f b -> f (a, b)
  infixr 1 |&|

constant :: (Functor f, CollapseP2P f) => a -> f a
constant a = a <$ nothing

null :: (Contravariant f, CollapseP2P f) => f a
null = () >$ nothing

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- | laws:
-- | u ||| never ~= u
-- | never ||| u ~= u
-- | (u ||| w) ||| z ~= u ||| (w ||| z)
class CollapseP2S f where
  never :: f Void
  (|||) :: f a -> f b -> f (Either a b)
  infixr 1 |||

empty :: (Functor f, CollapseP2S f) => f a
empty = absurd <$> never

class ExpandS2P f where
  foo :: f Void
  expand :: f (Either a b) -> (f a, f b)

empty' :: (ExpandS2P f, Functor f) => f a
empty' = absurd <$> foo

--

newtype Static f p a = Static { runStatic :: f (p a) }

instance (Functor p, Applicative f) => Functor (Static f p) where
  fmap f s = Static $ fmap f <$> runStatic s

instance (Contravariant p, Applicative f) => Contravariant (Static f p) where
  contramap f s = Static $ contramap f <$> runStatic s

instance (CollapseP2P p, Applicative f) => CollapseP2P (Static f p) where
  nothing = Static $ pure nothing
  sa |&| sb = Static $ (|&|) <$> runStatic sa <*> runStatic sb

instance (CollapseP2S p, Applicative f) => CollapseP2S (Static f p) where
  never = Static $ pure never
  sa ||| sb = Static $ (|||) <$> runStatic sa <*> runStatic sb

instance (ExpandS2P p, Applicative f) => ExpandS2P (Static f p) where
  foo = Static $ pure foo
  expand s = (Static $ fst . expand <$> runStatic s, Static $ snd . expand <$> runStatic s)
