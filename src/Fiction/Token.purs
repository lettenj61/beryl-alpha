module Fiction.Token where

import Prelude
import Data.Array.NonEmpty as NA
import Data.Maybe (Maybe(..), isJust)
import Data.String as S
import Data.String.Regex (Regex, match) as Re
import Data.String.Regex.Flags (global) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re

type Freq =
  { head    :: Int
  , middle  :: Int
  , last    :: Int
  }

type Link =
  { name  :: String
  , count :: Int
  }

type Chain =
  { prev  :: (Array String)
  , next  :: (Array String)
  }

splitRE :: Re.Regex
splitRE = Re.unsafeRegex "[aeiouy]+|[^aeiouy]+" Re.global

tokenize :: String -> Array String
tokenize word =
  case Re.match splitRE word of
    Nothing       -> []
    Just matches  -> do
      token <- matches
      pure token
