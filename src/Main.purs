module Main where

import Prelude hiding (identity)

import Control.Comonad (class Comonad, duplicate, extract)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Traced (Traced, traced)
import Control.Monad.State (State, modify_)
import Control.Monad.Writer (Writer, tell)
import Data.Functor.Pairing (type (⋈), identity, stateStore, writerTraced)
import Data.Monoid.Additive (Additive(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic (JSX, ReactComponent, react)
import React.Basic.DOM as R

type UI m = (m Unit -> Effect Unit) -> JSX
type Component w m = w (UI m)

move :: forall w m a b. Comonad w => Monad m => m ⋈ w -> m a -> w b -> w b
move pairing movement space = pairing (\_ newspace -> newspace) movement (duplicate space)

explore :: forall w m
  .  Comonad w 
  => Monad m 
  => String -> m ⋈ w -> Component w m -> ReactComponent {}
explore displayName pairing component = react
  { displayName
  , initialState
  , receiveProps
  , render
  }
  where
    initialState = { space: component }
    receiveProps _ _ _ = pure unit
    render {} { space } setState = 
      let 
        send action = setState \s -> { space: move pairing action s.space }
      in
        extract space send

tracedExample :: Component (Traced (Additive Int)) (Writer (Additive Int))
tracedExample = traced render where
  render :: Additive Int -> UI (Writer (Additive Int))
  render (Additive count) send =
    R.button
      { onClick: mkEffectFn1 \_ -> send $ tell (Additive 1)
      , children: [ R.text ("Increment: " <> show count) ]
      }

tracedReactComponent :: ReactComponent {}
tracedReactComponent = explore "TracedExample" (writerTraced identity) tracedExample

storeExample :: Component (Store Int) (State Int)
storeExample = store render 0 where
  render :: Int -> UI (State Int)
  render count send = 
    R.button
      { onClick: mkEffectFn1 \_ -> send $ modify_ (add 1)
      , children: [ R.text ("Increment: " <> show count) ]
      }

storeReactComponent :: ReactComponent {}
storeReactComponent = explore "StoreExample" (stateStore identity) storeExample
