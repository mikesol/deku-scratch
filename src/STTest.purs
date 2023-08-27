module STTest where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect, whileE)
import Effect.Console (log)
import Foreign.Object as Object
import Foreign.Object.ST as STObject
import Foreign.Object.ST.Unsafe as STObjectUnsafe

newtype Event a = Event ((a -> Effect Unit) -> ST Global (ST Global Unit))

subscribe :: forall a. Event a -> (a -> Effect Unit) -> ST Global (ST Global Unit)
subscribe (Event a) = a

create :: forall a. ST Global { push :: a -> Effect Unit, event :: Event a }
create = do
  idx <- STRef.new 0
  n <- STArray.new
  d <- STObject.new
  pure
    { event:
        Event \k -> do
          ix <- STRef.read idx
          let i = show ix
          void $ STArray.push (Tuple i k) n
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STObject.poke i unit d
    , push:
        \a -> do
          o <- liftST $ STRef.new (pure unit)
          nn <- liftST $ STArray.new
          let
            hasStuff = ado
              ln <- STArray.length n
              in ln > 0
          whileE (liftST hasStuff) do
            e <- liftST $ STArray.pop n
            case e of
              Nothing -> pure unit
              Just (Tuple x y) -> do
                d' <- liftST $ STObjectUnsafe.unsafeFreeze d
                let dm = Object.member x d'
                if dm then void $ liftST $ STObject.delete x d
                else liftST do
                  void $ STRef.modify (y a *> _) o
                  void $ STArray.push (Tuple x y) nn
          g <- liftST $ STArray.unsafeFreeze nn
          liftST $ void $ STArray.pushAll g n
          join $ (liftST $ STRef.read o)
    }

main :: Effect Unit
main = do
  { push, event } <- liftST create
  void $ liftST $ subscribe event \a -> do
    log a
  void $ liftST $ subscribe event \a -> do
    log a
  push "1"
  push "2"
  push "3"
