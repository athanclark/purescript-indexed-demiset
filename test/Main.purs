module Test.Main where

import Data.IxSet.Demi (IxDemiSet, Index)
import Data.IxSet.Demi (insert, delete, lookup, empty) as IxDemiSet

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.Array (snoc) as Array
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class.Console (log)
import Test.QuickCheck (quickCheckGen, arbitrary, Result (..))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Data.Argonaut (encodeJson, decodeJson)
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int32BE (..))


main :: Effect Unit
main = do
  log "IxDemiSet"
  log " - insert exists"
  quickCheckGen insertExistsIxDemiSet
  log " - delete doesn't exist"
  quickCheckGen deleteDoesntExistIxDemiSet
  log " - json iso"
  quickCheckGen jsonIsoIxDemiSet
  log " - arraybuffer iso"
  quickCheckGen abIsoIxDemiSet



insertExistsIxDemiSet :: Gen Result
insertExistsIxDemiSet = do
  {indicies,set} <- genIxDemiSet
  k <- arbitrary
  a <- arbitrary
  let {index,set: set'} = IxDemiSet.insert k a set
  pure $ case IxDemiSet.lookup index set' of
    Nothing -> Failed "No index in map"
    Just {key,value}
      | value == a && key == k -> Success
      | otherwise -> Failed $ "Value or key doesn't match - original: " <> show {key:k,value:a} <> ", found: " <> show {key,value}


deleteDoesntExistIxDemiSet :: Gen Result
deleteDoesntExistIxDemiSet = do
  {indicies,set} <- genIxDemiSet
  k <- arbitrary
  a <- arbitrary
  let {index, set: set'} = IxDemiSet.insert k a set
      set'' = IxDemiSet.delete index set'
  pure $ case IxDemiSet.lookup index set'' of
    Nothing -> Success
    Just {key,value} -> Failed $ "Found value when shouldn't exist - original: " <> show {key:k,value:a} <> ", found: " <> show {key,value}


jsonIsoIxDemiSet :: Gen Result
jsonIsoIxDemiSet = do
  {indicies,set} <- genIxDemiSet
  pure $ case decodeJson (encodeJson set) of
    Left e -> Failed $ "Json decoding failed: " <> e
    Right set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'

abIsoIxDemiSet :: Gen Result
abIsoIxDemiSet = do
  {indicies,set} <- genIxDemiSet'
  let ab = unsafePerformEffect (encodeArrayBuffer set)
      mSet' = unsafePerformEffect (decodeArrayBuffer ab)
  pure $ case mSet' of
    Nothing -> Failed "ArrayBuffer decoding failed"
    Just set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'


genIxDemiSet :: Gen {set :: IxDemiSet Int Int, indicies :: Array Index}
genIxDemiSet = do
  (xs :: Array _) <- arbitrary
  let go (Tuple k x) {indicies,set: set'} =
        let {index,set} = IxDemiSet.insert k x set'
        in  { indicies: Array.snoc indicies index
            , set
            }
  pure $ foldr go {set: IxDemiSet.empty, indicies: []} xs

genIxDemiSet' :: Gen {set :: IxDemiSet Int32BE Int32BE, indicies :: Array Index}
genIxDemiSet' = do
  xs <- arrayOf (Tuple <$> (Int32BE <$> arbitrary) <*> (Int32BE <$> arbitrary))
  let go (Tuple k x) {indicies,set: set'} =
        let {index,set} = IxDemiSet.insert k x set'
        in  { indicies: Array.snoc indicies index
            , set
            }
  pure $ foldr go {set: IxDemiSet.empty, indicies: []} xs
