{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Main where
import           Control.Applicative
import           Data.Functor.Identity

data Vec3 a = Vec3 !a !a !a deriving (Eq,Ord,Show, Functor, Foldable)

instance Traversable Vec3 where
  traverse f (Vec3 a b c) = Vec3 <$> f a <*> f b <*> f c

top, middle, bottom, left, center, right:: Functor f => (a -> f a) -> Vec3 a -> f (Vec3 a)
top = _x
middle = _y
bottom = _z
left = _x
center = _y
right = _z

-- | Usage Examples
-- >>> view (top . left) initialGrid
--
-- >>> set (top . left) (Just X) initialGrid
--
-- >>> over (top . left) (\x-> Just X) initialGrid
--
-- >>> over (traverse . traverse) (\x -> x) initialGrid
--
-- >>> over (traverse . _x) (\x -> Just X) initialGrid
--
-- >>> over (_x . traverse) (\x -> Just X) initialGrid

view ::  ((a -> Const a b) -> s -> Const a t) -> s -> a
view l s = getConst (l Const s)

over :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: ((a -> Identity b) -> (s -> Identity t)) -> b -> s -> t
set l r = over l (const r)

_x, _y, _z :: Functor f => (a -> f a) -> Vec3 a -> f (Vec3 a)
_x f (Vec3 x y z) = (\x' -> Vec3 x' y z) <$> f x
_y f (Vec3 x y z) = (\y' -> Vec3 x y' z) <$> f y
_z f (Vec3 x y z) = Vec3 x y <$> f z

data Cell = X | O deriving (Eq,Show)

type Square = Maybe Cell

type Grid a = (Vec3 (Vec3 a))

initialGrid :: Grid Square
initialGrid = Vec3 emptyRow emptyRow emptyRow
    where
        emptyRow = Vec3 Nothing Nothing Nothing

main :: IO ()
main = putStrLn "hello world"
