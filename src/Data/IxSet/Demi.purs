module Data.IxSet.Demi where

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Tuple (Tuple (..))
import Data.Map (Map)
import Data.Map (lookup, empty, insert, toUnfoldable, fromFoldable, delete, size) as Map
import Data.IntMap (IntMap)
import Data.IntMap (insert, lookup, empty, filter, toUnfoldable, delete) as IntMap
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Generic.Rep (class Generic)
import Data.Array (snoc, toUnfoldable) as Array
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  ( class DynamicByteLength, class EncodeArrayBuffer, class DecodeArrayBuffer
  , byteLength, putArrayBuffer, readArrayBuffer)
import Partial.Unsafe (unsafePartial)



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
instance encodeJsonIxDemiSet :: (EncodeJson k, EncodeJson a) => EncodeJson (IxDemiSet k a) where
  encodeJson set =
    let xs :: Array _
        xs = toUnfoldable' set
    in  encodeJson $ map (\(Tuple k x) -> "key" := k ~> "value" := x ~> jsonEmptyObject) xs
instance decodeJsonIxDemiSet :: (DecodeJson k, DecodeJson a, Ord k) => DecodeJson (IxDemiSet k a) where
  decodeJson json = do
    (xs :: Array (Tuple k a)) <- decodeJson json >>= \xs' ->
      traverse (\o -> Tuple <$> o .: "key" <*> o .: "value") xs'
    let {set} = fromFoldable xs
    pure set
instance dynamicByteLengthIxDemiSet :: (DynamicByteLength k, DynamicByteLength a) => DynamicByteLength (IxDemiSet k a) where
  byteLength set =
    let xs :: Array _
        xs = toUnfoldable' set
    in  byteLength xs
instance encodeArrayBufferIxDemiSet :: (EncodeArrayBuffer k, EncodeArrayBuffer a) => EncodeArrayBuffer (IxDemiSet k a) where
  putArrayBuffer b o set =
    let xs :: Array _
        xs = toUnfoldable' set
    in  putArrayBuffer b o xs
instance decodeArrayBufferIxDemiSet :: ( DynamicByteLength k, DynamicByteLength a
                                       , DecodeArrayBuffer k, DecodeArrayBuffer a, Ord k) => DecodeArrayBuffer (IxDemiSet k a) where
  readArrayBuffer b o = do
    (mxs :: Maybe (Array _)) <- readArrayBuffer b o
    case mxs of
      Nothing -> pure Nothing
      Just xs ->
        let {set} = fromFoldable xs
        in  pure (Just set)



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

deleteKey :: forall k a. Ord k => k -> IxDemiSet k a -> IxDemiSet k a
deleteKey k (IxDemiSet x) = IxDemiSet
  { mapping: Map.delete k x.mapping
  , keyMapping: IntMap.filter (_ /= k) x.keyMapping
  , nextIndex: x.nextIndex
  }

delete :: forall k a. Ord k => Index -> IxDemiSet k a -> IxDemiSet k a
delete i set@(IxDemiSet x) = case IntMap.lookup i x.keyMapping of
  Nothing -> set
  Just k -> deleteKey k (IxDemiSet x {keyMapping = IntMap.delete i x.keyMapping})

toUnfoldable' :: forall f k a. Unfoldable f => IxDemiSet k a -> f (Tuple k a)
toUnfoldable' (IxDemiSet {mapping}) = Map.toUnfoldable mapping

toUnfoldable :: forall f k a. Unfoldable f => Ord k => IxDemiSet k a -> f {index :: Index, key :: k, value :: a}
toUnfoldable (IxDemiSet {mapping,keyMapping}) =
  let is :: Array (Tuple Index k)
      is = IntMap.toUnfoldable keyMapping
  in  Array.toUnfoldable $
        map (\(Tuple i k) ->
              { value: unsafePartial (fromJust (Map.lookup k mapping))
              , key: k
              , index: i
              }) is

fromFoldable :: forall f k a. Foldable f => Ord k => f (Tuple k a) -> {set :: IxDemiSet k a, indicies :: Array Index}
fromFoldable xs =
  let go (Tuple k x) {set,indicies} =
        let {set: set',index} = insert k x set
        in  {set: set', indicies: Array.snoc indicies index}
  in  foldr go {set: empty, indicies: []} xs

mapKeys :: forall k k' a. Ord k' => (k -> k') -> IxDemiSet k a -> IxDemiSet k' a
mapKeys f (IxDemiSet x) = IxDemiSet x {mapping = mapping', keyMapping = map f x.keyMapping}
  where
    mapping' =
      let xs :: Array _
          xs = Map.toUnfoldable x.mapping
      in  Map.fromFoldable (map (\(Tuple k y) -> Tuple (f k) y) xs)

eqExact :: forall k a. Eq k => Eq a => IxDemiSet k a -> IxDemiSet k a -> Boolean
eqExact (IxDemiSet x) (IxDemiSet y) = x.mapping == y.mapping && x.keyMapping == y.keyMapping && x.nextIndex == y.nextIndex

showExact :: forall k a. Show k => Show a => Ord k => IxDemiSet k a -> String
showExact set =
  let xs :: Array _
      xs = toUnfoldable set
  in  show xs


size :: forall k a. IxDemiSet k a -> Int
size (IxDemiSet {mapping}) = Map.size mapping
