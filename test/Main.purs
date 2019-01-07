module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Fiction.Token (Entry, analyze, tokenize)
import Foreign.Object (lookup) as O
import Test.Assert (assertEqual)
import Test.Fiction.Sample (sample)

main :: Effect Unit
main = do
  analyzeTest

analyzeTest :: Effect Unit
analyzeTest = do
  log "analyze"
  let
    analysis =
      analyze ["alien", "species", "incoming"]
    getEntry :: String -> Maybe Entry
    getEntry token = O.lookup token analysis
  
  log "- count"
  assertEqual
    { actual: map _.count $ getEntry "a"
    , expected: Just 1
    }

  log "- freq"
  assertEqual
    { actual: map _.freq $ getEntry "ie"
    , expected: Just { head: 0, middle: 2, last: 0 }
    }
