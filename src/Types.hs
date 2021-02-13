{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Control.Arrow                  ( (&&&) )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Text                      ( unpack
                                                , words
                                                )
import           Prelude                 hiding ( words )

type IdiomHash = String
type HeadPinyin = String
type TailPinyin = String
type QueryItem = (IdiomHash, HeadPinyin, TailPinyin)
type QueryChain = (QueryItem, [QueryItem])

newtype Idiom = Idiom { unIdiom :: QueryItem }
  deriving (Show)

instance FromJSON Idiom where
    parseJSON = withObject "Idiom" $ \v -> do
        (h, t) <- (head &&& last) . fmap unpack . words <$> v .: "pinyin"
        ha     <- unpack <$> v .: "word"
        return (Idiom (ha, h, t))

