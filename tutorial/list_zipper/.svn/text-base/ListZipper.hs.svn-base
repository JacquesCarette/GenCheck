-- a copy of Data.List.Zipper from the ListZipper-1.2.0.2 hackage.

module ListZipper where

import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Data.Maybe (listToMaybe)


data Zipper a = Zip ![a] ![a] deriving (Eq,Show)

instance Arbitrary a => Arbitrary (Zipper a) where
  arbitrary = liftM2 Zip arbitrary arbitrary
  shrink (Zip ls rs) = [Zip ls' rs | ls' <- shrink ls]
                    ++ [Zip ls rs' | rs' <- shrink rs]

instance CoArbitrary a => CoArbitrary (Zipper a) where
  coarbitrary (Zip ls rs) = coarbitrary rs . coarbitrary ls

instance Functor Zipper  where
  fmap f (Zip ls rs) = Zip (map f ls) (map f rs)

-- | @empty@ is an empty zipper
empty :: Zipper a
empty = Zip [] []

-- | @fromList xs@ returns a zipper containing the elements of xs,
-- focused on the first element.
fromList :: [a] -> Zipper a
fromList as = Zip [] as

-- | @fromListEnd xs@ returns a zipper containing the elements of xs,
-- focused just off the right end of the list.
fromListEnd :: [a] -> Zipper a
fromListEnd as = Zip (reverse as) []

toList :: Zipper a -> [a]
toList (Zip ls rs) = reverse ls ++ rs

-- | @beginp z@ returns @True@ if the zipper is at the start.
beginp :: Zipper a -> Bool
beginp (Zip [] _ ) = True
beginp _           = False

-- | @endp z@ returns @True@ if the zipper is at the end.
-- It is not safe to call @cursor@ on @z@ if @endp z@ returns @True@.
endp :: Zipper a -> Bool
endp   (Zip _  []) = True
endp   _           = False

-- | @emptyp z@ returns @True@ if the zipper is completely empty.
-- forall z. emptyp z == beginp z && endp z
emptyp :: Zipper a -> Bool
emptyp (Zip [] []) = True
emptyp _           = False

start, end :: Zipper a -> Zipper a
start (Zip ls rs) = Zip [] (reverse ls ++ rs)
end   (Zip ls rs) = Zip (reverse rs ++ ls) []

-- | @cursor z@ returns the targeted element in @z@.
--
-- This function is not total, but the invariant is that
-- @endp z == False@ means that you can safely call
-- @cursor z@.
cursor :: Zipper a -> a
cursor (Zip _ (a:_)) = a

-- | @safeCursor@ is like @cursor@ but total.
safeCursor :: Zipper a -> Maybe a
safeCursor (Zip _ rs) = listToMaybe rs

-- | @left z@ returns the zipper with the focus
-- shifted left one element.
left :: Zipper a -> Zipper a
left  (Zip (a:ls) rs) = Zip ls (a:rs)
left  z               = z

-- | @right z@ returns the zipper with the focus
-- shifted right one element; this can move the
-- cursor off the end.
right :: Zipper a -> Zipper a
right (Zip ls (a:rs)) = Zip (a:ls) rs
right z               = z

-- | @insert x z@ adds x at the cursor.
insert :: a -> Zipper a -> Zipper a
insert a (Zip ls rs) = Zip ls (a:rs)

-- | @delete z@ removes the element at the cursor (if any).
-- Safe to call on an empty zipper.
-- forall x z. delete (insert x z) == z
delete :: Zipper a -> Zipper a
delete (Zip ls (_:rs)) = Zip ls rs
delete z               = z

-- | @push x z@ inserts x into the zipper, and advances
-- the cursor past it.
push :: a -> Zipper a -> Zipper a
push   a (Zip ls rs) = Zip (a:ls) rs

-- | @pop z@ removes the element before the cursor (if any).
-- Safe to call on an empty zipper.
-- forall x z. pop (push x z) == z
pop :: Zipper a -> Zipper a
pop    (Zip (_:ls) rs) = Zip ls rs
pop    z               = z

-- | @replace a z@ changes the current element in the zipper
-- to the passed in value.  If there is no current element,
-- the zipper is unchanged.  If you want to add the element
-- in that case instead, use @insert a (delete z)@.
replace :: a -> Zipper a -> Zipper a
replace a (Zip ls (_:rs)) = Zip ls (a:rs)
replace _ z               = z

-- | @reversez z@ returns the zipper with the elements in
-- the reverse order.  O(1).  The cursor is moved to the
-- previous element, so if the cursor was at the start,
-- it's now off the right end, and if it was off the
-- right end, it's now at the start of the reversed list.
reversez :: Zipper a -> Zipper a
reversez (Zip ls rs) = Zip rs ls

-- | @foldrz f x zip@ calls @f@ with the zipper focused on
-- each element in order, starting with the current.
-- You are guaranteed that f can safely call "cursor" on
-- its argument; the zipper won't be at the end.
foldrz :: (Zipper a -> b -> b) -> b -> Zipper a -> b
foldrz f x = go where
    go z
        | endp z    = x
        | otherwise = f z (go $ right z)

-- | @foldlz f x zip@ calls f with the zipper focused on
-- each element in order, starting with the current.
-- You are guaranteed that f can safely call "cursor" on
-- its argument; the zipper won't be at the end.
foldlz :: (b -> Zipper a -> b) -> b -> Zipper a -> b
foldlz f x z
        | endp z    = x
        | otherwise = foldlz f (f x z) (right z)

-- | @foldlz'@ is foldlz with a strict accumulator
foldlz' :: (b -> Zipper a -> b) -> b -> Zipper a -> b
foldlz' f x z
        | endp z    = x
        | otherwise = acc `seq` foldlz' f acc (right z)
        where acc = f x z

-- | @extractz@, @extendz@, and @duplicatez@ can be used to
-- implement Copointed and Comonad from category-extras.  I didn't
-- add the instances here so as not to introduce a dependency
-- on that package.
extractz :: Zipper a -> a
extractz = cursor

duplicatez :: Zipper a -> Zipper (Zipper a)
duplicatez z = Zip ls' rs' where
    rs' = foldrz (:) [] z
    ls' = map reversez $
          foldrz (\z' xs -> right z' : xs) [] $
          reversez z

extendz :: (Zipper a -> b) -> Zipper a -> Zipper b
extendz f z = Zip ls' rs' where
    rs' = foldrz (\z' xs -> f z' : xs) [] z
    ls' = foldrz (\z' xs -> f (reversez $ right z') : xs) [] $ reversez z
