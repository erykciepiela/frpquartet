module Quartet
  ( P2P (nothing, (|&|))
  , constant
  , null
  , P2S (never, (|||))
  , empty
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
class P2P f where
  nothing :: f ()
  (|&|) :: f a -> f b -> f (a, b)
  infixr 1 |&|

constant :: (Functor f, P2P f) => a -> f a
constant a = a <$ nothing

null :: (Contravariant f, P2P f) => f a
null = () >$ nothing

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- | laws:
-- | u ||| never ~= u
-- | never ||| u ~= u
-- | (u ||| w) ||| z ~= u ||| (w ||| z)
class P2S f where
  never :: f Void
  (|||) :: f a -> f b -> f (Either a b)
  infixr 1 |||

empty :: (Functor f, P2S f) => f a
empty = absurd <$> never

newtype Static f p a = Static { runStatic :: f (p a) }

instance (Functor p, Applicative f) => Functor (Static f p) where
  fmap f s = Static $ fmap f <$> runStatic s

instance (Contravariant p, Applicative f) => Contravariant (Static f p) where
  contramap f s = Static $ contramap f <$> runStatic s

instance (P2P p, Applicative f) => P2P (Static f p) where
  nothing = Static $ pure nothing
  sa |&| sb = Static $ (|&|) <$> runStatic sa <*> runStatic sb

instance (P2S p, Applicative f) => P2S (Static f p) where
  never = Static $ pure never
  sa ||| sb = Static $ (|||) <$> runStatic sa <*> runStatic sb
