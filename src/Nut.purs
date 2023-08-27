module Nut where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Data.Array.ST as STArray
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Deku.DOM (s)
import Effect (Effect, foreachE)
import Effect.Random (random, randomInt)
import FRP.Event (bindToEffect, bindToST, makeEvent, makeLemmingEvent, subscribe)
import FRP.Poll (Poll, dredge, poll, sampleBy)
import Unsafe.Coerce (unsafeCoerce)

-- type MakeElement = { id :: String }
-- type AssociateWithParent = { id :: String, parent :: String }
-- type SetAttribute = { id :: String, key :: String, value :: String }

-- newtype Attribute = Attribute { key :: String, value :: String }

-- class Korok payload where
--   makeElement :: MakeElement -> payload
--   associateWithParent :: AssociateWithParent -> payload
--   setAttribute :: SetAttribute -> payload

-- data NutF a = 
--      NSelf String
--    | NPure a
--    | NMany (Array a)
--    | NST (ST Global a)
--    | NManyST (ST Global (Array a))
--    | NEffect (Effect a)
--    | NManyEffect (Effect (Array a))
--    | NPoll (Poll a)
--    | NResume (ST Global (NutF a))

-- -- fromEffect :: forall a. Effect (NutF a) -> NutF a
-- -- fromEffect e = NEffect $ e >>= case _ of
-- --   NSelf s -> ?hole -- fromEffect e
-- --   NPure a -> pure a
-- --   NMany a -> ?hole
-- --   NST a -> ?hole
-- --   NManyST a -> ?hole
-- --   NEffect a -> ?hole
-- --   NManyEffect a -> ?hole
-- --   NPoll a -> ?hole
  
-- instance Functor NutF where
--   map f (NSelf s) = ?hole -- NResume (NEffect (pure $ NSelf s)) -- NEffect $ fromEffect (map (map f) (pure (NSelf s) :: Effect (NutF _)))
--   map f (NPure a) = NPure (f a)
--   map f (NMany a) = NMany (f <$> a)
--   map f (NST a) = NST (f <$> a)
--   map f (NManyST a) = NManyST (map (map f) a)
--   map f (NEffect a) = NEffect (f <$> a)
--   map f (NManyEffect a) = NManyEffect (map (map f) a)
--   map f (NPoll a) = NPoll (f <$> a)
--   -- ugggh hairy
--   map f (NResume n) = NST $ go n
--     where go x = do
--                   x' <- x
--                   case x' of
--                     NResume n'' -> go n''
--                     NSelf n'' -> go (pure (NSelf n''))
--                     NPure a ->  pure $ f a

      


-- -- traverseE :: forall a b. Array a -> (a -> Effect b) -> Effect (Array b)
-- -- traverseE a e = do
-- --   arr <- liftST STArray.new
-- --   foreachE a \a' -> do
-- --     a'' <- e a'
-- --     void $ liftST $ STArray.push  a'' arr
-- --   liftST $ STArray.freeze arr

-- -- traverseST :: forall r a b. Array a -> (a -> ST r b) -> ST r (Array b)
-- -- traverseST = unsafeCoerce traverseE

-- -- foreachST :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
-- -- foreachST = unsafeCoerce foreachE

-- -- instance Apply NutF where
-- --   apply (NPure f) (NPure a) = NPure (f a)
-- --   apply (NPure f) (NMany a) = NMany (f <$> a)
-- --   apply (NPure f) (NST a) = NST (f <$> a)
-- --   apply (NPure f) (NManyST a) = NManyST (map (map f) a)
-- --   apply (NPure f) (NEffect a) = NEffect (f <$> a)
-- --   apply (NPure f) (NManyEffect a) = NManyEffect (map (map f) a)
-- --   apply (NPure f) (NPoll a) = NPoll (f <$> a)
-- --   -------
-- --   apply (NMany f) (NPure a) = NMany (f <@> a)
-- --   apply (NMany f) (NMany a) = NMany (f <*> a)
-- --   apply (NMany f) (NST a) = NManyST $ traverseST f (_ <$> a)
-- --   apply (NMany f) (NManyST a) = NManyST $ map join $ traverseST f (\f' -> map (map f') a)
-- --   apply (NMany f) (NEffect a) = NManyEffect $ traverseE f (_ <$> a)
-- --   apply (NMany f) (NManyEffect a) = NManyEffect $ map join $ traverseE f (\f' -> map (map f') a)
-- --   apply (NMany f) (NPoll a) = NPoll $ poll \e -> makeLemmingEvent \s k -> s (sampleBy Tuple a e) \(Tuple a' f'') -> foreachST f \f' -> do
-- --      k $ f'' $ f' a'
-- --   -------
-- --   apply (NManyST f) (NPure a) = NManyST ado
-- --     f' <- f
-- --     in f' <@> a
-- --   apply (NManyST f) (NMany a) = NManyST (f <#> (_ <*> a))
-- --   apply (NManyST f) (NST a) = NManyST $ do
-- --       z <- f
-- --       traverseST z (_ <$> a)
-- --   apply (NManyST f) (NManyST a) = NManyST $ do
-- --       z <- f
-- --       map join $ traverseST z (\f' -> map (map f') a)
-- --   apply (NManyST f) (NEffect a) = NManyEffect do
-- --       z <- liftST f
-- --       traverseE z (_ <$> a)
-- --   apply (NManyST f) (NManyEffect a) = NManyEffect do
-- --       z <- liftST f
-- --       map join $ traverseE z (\f' -> map (map f') a)
-- --   apply (NManyST f) (NPoll a) = NPoll $ poll \e -> makeLemmingEvent \s k -> s (sampleBy Tuple a e) \(Tuple a' f'') ->do
-- --      z <- f
-- --      foreachST z \f' -> do
-- --        k $ f'' $ f' a'
-- --   -------
-- --   apply (NManyEffect f) (NPure a) = NManyEffect ado
-- --     f' <- f
-- --     in f' <@> a
-- --   apply (NManyEffect f) (NMany a) = NManyEffect (f <#> (_ <*> a))
-- --   apply (NManyEffect f) (NST a) = NManyEffect $ do
-- --       z <- f
-- --       traverseE z (_ <$> liftST a)
-- --   apply (NManyEffect f) (NManyST a) = NManyEffect $ do
-- --       z <- f
-- --       map join $ traverseE z (\f' -> map (map f') $ liftST a)
-- --   apply (NManyEffect f) (NEffect a) = NManyEffect do
-- --       z <- f
-- --       traverseE z (_ <$> a)
-- --   apply (NManyEffect f) (NManyEffect a) = NManyEffect do
-- --       z <- f
-- --       map join $ traverseE z (\f' -> map (map f') a)
-- --   apply (NManyEffect f) (NPoll a) = NPoll $ poll \e -> makeEvent \k -> subscribe (sampleBy Tuple a e) \(Tuple a' f'') ->do
-- --      z <- f
-- --      foreachE z \f' -> do
-- --        k $ f'' $ f' a'
-- --   -------
-- --   apply (NST f) (NPure a) = NST (f <@> a)
-- --   apply (NST f) (NMany a) = NManyST $ traverseST a (f <@> _)
-- --   apply (NST f) (NST a) = NST (f <*> a)
-- --   apply (NST f) (NManyST a) = NManyST ado
-- --       x <- f
-- --       y <- a
-- --       in x <$> y
-- --   apply (NST f) (NEffect a) = NEffect (liftST f <*> a)
-- --   apply (NST f) (NManyEffect a) = NManyEffect ado
-- --       x <- liftST f
-- --       y <- a
-- --       in x <$> y
-- --   apply (NST f) (NPoll a) = NPoll $ dredge b2e a
-- --     where
-- --     b2e ev = bindToST ev \a' -> f <@> a'
-- --   -------
-- --   apply (NEffect f) (NPure a) = NEffect (f <@> a)
-- --   apply (NEffect f) (NMany a) = NManyEffect $ traverseE a (f <@> _)
-- --   apply (NEffect f) (NST a) = NEffect (f <*> liftST a)
-- --   apply (NEffect f) (NManyST a) = NManyEffect ado
-- --       x <- f
-- --       y <- liftST a
-- --       in x <$> y
-- --   apply (NEffect f) (NEffect a) = NEffect (f <*> a)
-- --   apply (NEffect f) (NManyEffect a) = NManyEffect ado
-- --       x <- f
-- --       y <- a
-- --       in x <$> y
-- --   apply (NEffect f) (NPoll a) = NPoll $ dredge b2e a
-- --     where
-- --     b2e ev = bindToEffect ev \a' -> f <@> a'
-- --   -------
-- --   apply (NPoll f) (NPure a) = NPoll (f <@> a)
-- --   apply (NPoll f) (NMany a) = NPoll $ poll \e -> makeLemmingEvent \s k -> s (sampleBy Tuple f e) \(Tuple f' f'') -> foreachST a \a' -> do
-- --      k $ f'' $ f' a'
-- --   apply (NPoll f) (NST a) = NPoll $ dredge b2e f
-- --     where
-- --     b2e ev = bindToST ev \f' -> f' <$> a
-- --   apply (NPoll f) (NManyST a) = NPoll $ poll \e -> makeLemmingEvent \s k -> s (sampleBy Tuple f e) \(Tuple f' f'') -> do
-- --       z <- a
-- --       foreachST z \a' -> do
-- --         k $ f'' $ f' a'
-- --   apply (NPoll f) (NEffect a) = NPoll $ dredge b2e f
-- --     where
-- --     b2e ev = bindToEffect ev \f' -> f' <$> a
-- --   apply (NPoll f) (NManyEffect a) = NPoll $ poll \e -> makeEvent \k -> subscribe (sampleBy Tuple f e) \(Tuple f' f'') -> do
-- --       z <- a
-- --       foreachE z \a' -> do
-- --         k $ f'' $ f' a'
-- --   apply (NPoll f) (NPoll a) = NPoll (f <*> a)

-- -- instance Applicative NutF where
-- --   pure = NPure

-- -- data NutF a = NPure a
-- --    | NMany (Array a)
-- --    | NST (ST Global a)
-- --    | NManyST (ST Global (Array a))
-- --    | NEffect (Effect a)
-- --    | NManyEffect (Effect (Array a))
-- --    | NPoll (Poll a)

-- -- crack :: forall a. NutF (Array a) -> NutF a
-- -- crack (NPure a) = NMany a
-- -- crack (NMany a) = NMany $ join a
-- -- crack (NST a) = NManyST a
-- -- crack (NManyST a) = NManyST $ map join a
-- -- crack (NEffect a) = NManyEffect a
-- -- crack (NManyEffect a) = NManyEffect $ map join a
-- -- crack (NPoll a) = NPoll $ poll \e -> makeLemmingEvent \s k -> s (sampleBy Tuple a e) \(Tuple a' f') -> foreachST a' \a'' -> do
-- --   k $ f' a''

-- -- newtype Nut = Nut (forall k. Korok k => { mid :: Maybe String, parent :: String } -> NutF k)

-- -- freshId :: NutF String
-- -- freshId = NEffect (show <$> randomInt 0 4000000)

-- -- IsEvent is gonna be impossible
-- -- Just think about a fixed point

-- -- element :: String -> Array (NutF Attribute) -> Array Nut -> Nut
-- -- element s atts elts = Nut $ NutF \{ mid, parent } -> ado
-- --   id <- freshId
-- --   let id' = fromMaybe id mid
-- --   let atts' = map (\(Attribute { key, value }) -> setAttribute { id, key = k, value = v }) atts
-- --   let elts' = map (\(Nut f) -> associateWithParent { id, parent = parent }) elts
-- --     makeElement { id } <#> traverseE atts' id <#> traverseE elts' id
  
  class CountBs txt n | txt -> n

  class CountBs1 txti txto n done | txti -> txto n done

  class GateCountBs1 done txt n | done txt -> n