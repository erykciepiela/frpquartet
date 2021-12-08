{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Quartet
  ( ExpandS2P (expand)
  , Static (..)
  ) where

import           Data.Functor.Contravariant
import           Data.Functor.Invariant
import           Data.Void
import           Prelude                    hiding (null)
import Actegory (MonoidalCategory (none), (|.|), Actegory, (|>|))


-- | Notice no @Functor f@ nor @Contravariant f@ constraint
class ExpandS2P f where
  expand :: f (Either a b) -> (f a, f b)

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- laws:
--   u |&| nothing ~= u
--   nothing |&| u ~= u
--   (u |&| w) |&| z ~= u |&| (w |&| z)
-- additionally if @ExpandS2P f@ then:
--   |&| . expand = nothing
class CollapseP2P f where
  nothing :: f ()
  (|&|) :: f a -> f b -> f (a, b)
  infixr 1 |&|

-- | Notice no @Functor f@ nor @Contravariant f@ constraint
-- laws:
--   u ||| never ~= u
--   never ||| u ~= u
--   (u ||| w) ||| z ~= u ||| (w ||| z)
-- additionally if @ExpandS2P f@ then:
--   ||| . expand = id
class CollapseP2S f where
  never :: f Void
  (|||) :: f a -> f b -> f (Either a b)
  infixr 1 |||

--

newtype Static f p a = Static { runStatic :: f (p a) }

instance (Invariant p, Applicative f) => Invariant (Static f p) where
  invmap f g s = Static $ invmap f g <$> runStatic s

instance (Functor p, Applicative f) => Functor (Static f p) where
  fmap f s = Static $ fmap f <$> runStatic s

instance (Applicative p, Applicative f) => Applicative (Static f p) where
  pure a = Static $ pure $ pure a
  sfpf <*> sfpa = Static $ (<*>) <$> runStatic sfpf <*> runStatic sfpa

instance (Contravariant p, Applicative f) => Contravariant (Static f p) where
  contramap f s = Static $ contramap f <$> runStatic s

instance (CollapseP2P p, Applicative f) => CollapseP2P (Static f p) where
  nothing = Static $ pure nothing
  sa |&| sb = Static $ (|&|) <$> runStatic sa <*> runStatic sb

instance (CollapseP2S p, Applicative f) => CollapseP2S (Static f p) where
  never = Static $ pure never
  sa ||| sb = Static $ (|||) <$> runStatic sa <*> runStatic sb

instance (MonoidalCategory c p i, Applicative f) => MonoidalCategory (Static f c) p i  where
  none = Static $ pure none
  sa |.| sb = Static $ (|.|) <$> runStatic sa <*> runStatic sb

instance (Actegory c p i d, Applicative f) => Actegory (Static f c) p i (Static f d) where
  sa |>| sb = Static $ (|>|) <$> runStatic sa <*> runStatic sb

instance (ExpandS2P p, Applicative f) => ExpandS2P (Static f p) where
  expand s = (Static $ fst . expand <$> runStatic s, Static $ snd . expand <$> runStatic s)
