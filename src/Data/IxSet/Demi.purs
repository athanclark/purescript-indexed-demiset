module Data.IxSet.Demi where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Map (Map)
import Data.Map (lookup, empty, insert) as Map
import Data.IntMap (IntMap)
import Data.IntMap (insert, lookup, empty) as IntMap
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Generic.Rep (class Generic)



type Index = Int

newtype IxDemiSet k a = IxDemiSet
  { mapping :: Map k a
  , keyMapping :: IntMap k
  , nextIndex :: Index
  }
derive instance genericIxDemiSet :: (Generic k k', Generic a a') => Generic (IxDemiSet k a) _
instance eqIxDemiSet :: (Eq k, Eq a) => Eq (IxDemiSet k a) where
  eq (IxDemiSet x) (IxDemiSet y) = x.mapping == y.mapping
instance ordIxDemiSet :: (Ord k, Ord a) => Ord (IxDemiSet k a) where
  compare (IxDemiSet x) (IxDemiSet y) = compare x.mapping y.mapping
instance showIxDemiSet :: (Show k, Show a) => Show (IxDemiSet k a) where
  show (IxDemiSet x) = show x.mapping
instance functorIxDemiSet :: Functor (IxDemiSet k) where
  map f (IxDemiSet x) = IxDemiSet x {mapping = map f x.mapping}
instance foldableIxDemiSet :: Foldable (IxDemiSet k) where
  foldMap f (IxDemiSet x) = foldMap f x.mapping
  foldr f acc (IxDemiSet x) = foldr f acc x.mapping
  foldl f acc (IxDemiSet x) = foldl f acc x.mapping
instance traversableIxDemiSet :: Traversable (IxDemiSet k) where
  traverse f (IxDemiSet x) = (\mapping -> IxDemiSet x {mapping = mapping}) <$> traverse f x.mapping
  sequence (IxDemiSet x) = (\mapping -> IxDemiSet x {mapping = mapping}) <$> sequence x.mapping



empty :: forall k a. IxDemiSet k a
empty = IxDemiSet {mapping: Map.empty, keyMapping: IntMap.empty, nextIndex: 0}

lookupKey :: forall k a. Ord k => k -> IxDemiSet k a -> Maybe a
lookupKey k (IxDemiSet {mapping}) = Map.lookup k mapping

lookup :: forall k a. Ord k => Index -> IxDemiSet k a -> Maybe {key :: k, value :: a}
lookup i set@(IxDemiSet {keyMapping}) = case IntMap.lookup i keyMapping of
  Nothing -> Nothing
  Just k -> case lookupKey k set of
    Nothing -> Nothing
    Just x -> Just {key: k, value: x}

insert :: forall k a. Ord k => k -> a -> IxDemiSet k a -> {set :: IxDemiSet k a, index :: Index}
insert k x (IxDemiSet set) =
  { set: IxDemiSet
    { mapping: Map.insert k x set.mapping
    , keyMapping: IntMap.insert set.nextIndex k set.keyMapping
    , nextIndex: set.nextIndex + 1
    }
  , index: set.nextIndex
  }

-- deleteKey :: k -> IxDemiSet k a -> IxDemiSet k a

-- delete :: Index -> IxDemiSet k a -> IxDemiSet k a

-- toUnfoldable' :: IxDemiSet k a -> f (Tuple k a)

-- toUnfoldable :: IxDemiSet k a -> f {index :: Index, key :: k, value :: a}

-- fromFoldable :: f (Tuple k a) -> {set :: IxDemiSet k a, indicides :: Array Index}

-- mapKeys :: Ord k' => (k -> k') -> IxDemiSet k a -> IxDemiSet k' a

-- eqExact :: IxDemiSet k a -> IxDemiSet k a -> Boolean

-- showExact :: IxDemiSet k a -> String
