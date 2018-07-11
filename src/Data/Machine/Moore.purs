module Data.Machine.Moore where

import Prelude

import Control.Comonad (class Comonad, extend)
import Control.Extend (class Extend)
import Data.Functor.Pairing (type (⋈))
import Data.Tuple (Tuple(..))


data Moore i a = Moore (Tuple a (i → Moore i a))

buildMoore :: ∀ s i a. (s → Tuple a (i → s)) → s → Moore i a
buildMoore next state = Moore $ Tuple a (\i → buildMoore next $ transition i)
  where
    Tuple a transition = next state

instance functorMoore :: Functor (Moore i) where
  map f (Moore (Tuple a t)) = Moore $ Tuple (f a) (\i -> map f $ t i)

instance extendMoore :: Extend (Moore i) where
  extend f w@(Moore (Tuple _ t)) = Moore $ Tuple (f w) (\i -> extend f $ t i)

instance comonadMoore :: Comonad (Moore i) where
  extract (Moore (Tuple a _)) = a
 
data Actions i a = NoAction a | Action (Tuple i (Actions i a))

instance functorActions :: Functor (Actions i) where
  map f (NoAction a) = NoAction (f a)
  map f (Action (Tuple i as)) = Action (Tuple i (map f as))

instance applyActions :: Apply (Actions i) where
  apply (NoAction ab) fa = map ab fa
  apply (Action (Tuple i as)) fa = Action (Tuple i (apply as fa))
  
instance applicativeActions :: Applicative (Actions i) where
  pure = NoAction

instance binddActions :: Bind (Actions i) where
  bind (NoAction a) f = f a
  bind (Action (Tuple i as)) f = Action (Tuple i (bind as f))

instance monadActions :: Monad (Actions i)

action :: ∀ i. i → Actions i Unit
action input = Action $ Tuple input (pure unit)

actionsMoore :: ∀ i. (Actions i) ⋈ (Moore i)
actionsMoore f (NoAction a) (Moore (Tuple b _)) =  f a b
actionsMoore f (Action (Tuple i as)) (Moore (Tuple _ t)) =  actionsMoore f as (t i)
