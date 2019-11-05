{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Utils (
    -- export Text
      Text (..)
    -- export T.pack and T.unpack
    , T.pack
    , T.unpack
    --
    , putText
    , putTextLn
    , eol
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

putText :: Text -> IO ()
putText = putStr . T.unpack

putTextLn :: Text -> IO ()
putTextLn = putStrLn . T.unpack

eol :: Text
eol = "\n"

