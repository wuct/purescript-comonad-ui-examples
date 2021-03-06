module Main where

import Prelude hiding (identity)

import Control.Comonad (class Comonad, duplicate, extract)
import Control.Comonad.Env (Env, env)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Traced (Traced, traced)
import Control.Monad.Reader (Reader)
import Control.Monad.State (State, modify_)
import Control.Monad.Writer (Writer, tell)
import Data.Functor.Pairing (type (⋈), identity, readerEnv, stateStore, writerTraced)
import Data.Machine.Moore (Actions, Moore, action, actionsMoore, buildMoore)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic (JSX, ReactComponent, react)
import React.Basic.DOM as R

type UI m = (m Unit -> Effect Unit) -> JSX
type Component w m = w (UI m)

move :: forall w m a b. Comonad w => Monad m => m ⋈ w -> m a -> w b -> w b
move pairing movement space = pairing (\_ newspace -> newspace) movement (duplicate space)

select :: forall w m a b. Comonad w => Monad m => m ⋈ w -> m a -> w b -> a
select pairing movement space = pairing (\result _ -> result) movement space

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

data Input = Increment

mooreExample :: Component (Moore Input) (Actions Input)
mooreExample = buildMoore (\count -> Tuple (render count) (update count)) 0
  where
    update count Increment = count + 1
    render count send = 
      R.button
        { onClick: mkEffectFn1 \_ -> send $ action Increment
        , children: [ R.text ("Increment: " <> show count) ]
        }

mooreReactComponent :: ReactComponent {}
mooreReactComponent = explore "MooreExample" actionsMoore mooreExample

-- The state indside Env comonads can only be consumed by consumers (via `select`.)
envExample :: Component (Env Int) (Reader Int)
envExample = env 0 ui where
  ui :: UI (Reader Int)
  ui send =
    R.button
        -- We can't update the read-only state here.
        -- Actually, we can't read it either.
        { onClick: mkEffectFn1 \_ -> send $ pure unit
        , children: [ R.text ("We can do nothing here.") ]
        }

envReactComponent :: ReactComponent {}
envReactComponent = explore "EnvExample" (readerEnv identity) envExample
