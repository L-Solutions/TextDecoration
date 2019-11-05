{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Color (
    Color (..)
    , BGColor (..)
    , FGColor (..)
    , Decoration (..)
    , colorize
    , withFG
    , withBG
    -- | Instances
    , Color256
    , Color8 (..)
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

-- Color
class Color c where
    {-# MINIMAL (code | value) #-}
    code :: c -> Int
    code = read . T.unpack . value
    --
    value :: c -> Text
    value = T.pack . show . code

class (Color c) => FGColor c where
    starterFG :: c -> Text
    --
    resetFG :: c -> Text
    resetFG = const "\x1b[0m"

class (Color c) => BGColor c where
    starterBG :: c -> Text
    --
    resetBG :: c -> Text
    resetBG = const "\x1b[0m"

------------------------------------------------
-- Instance

-- Color256
type Color256 = Int

instance Color Int where
    code i
        | 0 <= i && i <= 255 = i
        | otherwise          = 0

instance FGColor Int where
    starterFG i = "\x1b[38;5;" <> value i <> "m"

instance BGColor Int where
    starterBG i = "\x1b[48;5;" <> value i <> "m"

-- Color 8
data Color8 = Black
            | Red
            | Green
            | Yellow
            | Blue
            | Magenta
            | Cyan
            | White

instance Color Color8 where
    code Black   = 30
    code Red     = 31
    code Green   = 32
    code Yellow  = 33
    code Blue    = 34
    code Magenta = 35
    code Cyan    = 36
    code White   = 37

instance FGColor Color8 where
    starterFG c = "\x1b[" <> value c <> "m"

-- Unit
instance Color () where
    code _ = 0

instance FGColor () where
    starterFG = const ""
    resetFG   = const ""

instance BGColor () where
    starterBG = const ""
    resetBG   = const ""

--

data (FGColor f, BGColor b) => Decoration f b = Decoration { fgColor :: Maybe f
                                                           , bgColor :: Maybe b
                                                           }

colorize :: (FGColor c1, BGColor c2) => Decoration c1 c2
                                     -> Text
                                     -> Text
colorize (Decoration (Just fg) (Just bg)) = withFG fg . withBG bg
colorize (Decoration (Just fg) Nothing)   = withFG fg
colorize (Decoration Nothing (Just bg))   = withBG bg
colorize _                                = id

withFG :: FGColor c => c
                    -> Text
                    -> Text
withFG c t = starterFG c <> t <> resetFG c

withBG :: BGColor c => c
                    -> Text
                    -> Text
withBG c t = starterBG c <> t <> resetBG c

