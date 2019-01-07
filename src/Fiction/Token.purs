module Fiction.Token
  ( Analysis
  , Chain
  , Entry
  , Freq
  , analyze
  , analyzeContent
  , tokenize
  ) where

import Prelude
import Control.Monad.ST (foreach, run)
import Control.Monad.ST.Ref (modify, new, read)
import Data.Array (index, elem, length, mapWithIndex) as A
import Data.Array.ST (push, run, sort, unsafeThaw) as A
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, match, split)
import Data.String.Regex.Flags (global) as RF
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (Object, empty, lookup, runST, thawST) as FO
import Foreign.Object.ST (peek, poke) as FO

type Freq =
  { head    :: Int
  , middle  :: Int
  , last    :: Int
  }

type Chain =
  { prev  :: (Array String)
  , next  :: (Array String)
  }

type Entry =
  { count :: Int
  , chain :: Chain
  , freq  :: Freq
  , name  :: String
  }

type Analysis = FO.Object Entry

vowelsRE :: Regex
vowelsRE = unsafeRegex "[aeiouy]+|[^aeiouy]+" RF.global

tokenize :: String -> Array String
tokenize word =
  case match vowelsRE word of
    Nothing     -> []
    Just tokens -> NEA.catMaybes tokens

isHeading :: Freq -> Boolean
isHeading freq = freq.head > 0

isEnding :: Freq -> Boolean
isEnding freq = freq.last > 0

isSimilarInPosition :: Freq -> Freq -> Boolean
isSimilarInPosition f1 f2 =
  isHeading f1  == isHeading f2 &&
  isEnding f1   == isEnding f2

appendChain :: (Maybe String) -> (Maybe String) -> Chain -> Chain
appendChain prev next chain =
  let
    attach :: String -> (Array String) -> (Array String)
    attach link tokens = A.run do
      buf <- A.unsafeThaw tokens
      when (not (A.elem link tokens)) do
        _ <- A.push link buf
        _ <- A.sort buf
        pure unit
      pure buf
  in
    case prev, next of
      Just p, Just n  -> chain
        { prev = attach p chain.prev
        , next = attach n chain.next
        }
      Just p, _       -> chain { prev = attach p chain.prev }
      _     , Just n  -> chain { next = attach n chain.next }
      _     , _       -> chain

createEntry :: String -> Entry
createEntry token =
  { count: 0
  , chain: { prev: [], next: [] }
  , freq: { head: 0, middle: 0, last: 0 }
  , name: token
  }

tokenWithIndex :: Int -> String -> { value :: String, index :: Int }
tokenWithIndex index value =
  { value: value
  , index: index
  }

updateAnalysis :: (Array String) -> Analysis -> Analysis
updateAnalysis tokens analysis = FO.runST do
  parsed <- FO.thawST analysis
  let len = (A.length tokens) - 1
  _ <- foreach (A.mapWithIndex tokenWithIndex tokens) \{ value, index } -> void do
    state <- FO.peek value parsed
    let
      entry = case state of
        Just e  -> e
        Nothing -> createEntry value

      freq = entry.freq
      newFreq = case index of
        i | i == 0    -> freq { head = freq.head + 1 }
          | i == len  -> freq { last = freq.last + 1 }
          | otherwise -> freq { middle = freq.middle + 1 }

      prev = A.index tokens (index - 1)
      next = A.index tokens (index + 1)

      updated = entry
        { count = entry.count + 1
        , freq  = newFreq
        , chain = appendChain prev next entry.chain
        }
    FO.poke value updated parsed
  pure parsed

analyze :: (Array String) -> Analysis
analyze words = run do
  ref <- new FO.empty
  _ <- foreach words \word -> void do
    let tokens = tokenize word
    modify (\analysis -> updateAnalysis tokens analysis) ref
  read ref

analyzeContent :: String -> Analysis
analyzeContent content =
  analyze $ split (unsafeRegex "[\\s]+" RF.global) content

-- analyzation

findEntry :: String -> Analysis -> Maybe Entry
findEntry = FO.lookup

findFreq :: String -> Analysis -> Maybe Freq
findFreq t db = map _.freq $ findEntry t db

--}
