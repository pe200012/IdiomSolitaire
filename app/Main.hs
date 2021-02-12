{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow                  ( Arrow(..) )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , decode
                                                , withObject
                                                )
import           Data.ByteString.Lazy.Char8     ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Foldable                  ( find )
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromJust )
import           Data.Sequence                  ( Seq(..)
                                                , fromList
                                                , singleton
                                                )
import           Data.Set                       ( Set
                                                , empty
                                                , insert
                                                , member
                                                )
import           Data.Text.Lazy                 ( words )
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding        ( decodeUtf8
                                                , encodeUtf8
                                                )
import qualified Data.Text.Lazy.IO             as T
import           Lens.Micro                     ( (^.)
                                                , _1
                                                , _2
                                                , _3
                                                , to
                                                )
import           Lens.Micro.Extras              ( view )
import           Prelude                 hiding ( words )

type IdiomHash = String
type HeadPinyin = String
type TailPinyin = String
type QueryItem = (IdiomHash, HeadPinyin, TailPinyin)
type QueryChain = (QueryItem, [QueryItem])

newtype Idiom = Idiom { unIdiom :: QueryItem } deriving Show

instance FromJSON Idiom where
    parseJSON = withObject "Idiom" $ \v -> do
        (h, t) <- (head &&& last) . fmap T.unpack . words <$> v .: "pinyin"
        ha     <- T.unpack <$> v .: "word"
        return (Idiom (ha, h, t))

solitaire :: IdiomHash -> Seq QueryChain -> Set IdiomHash -> (HeadPinyin -> [QueryItem]) -> [QueryItem]
solitaire dst Empty vis query = []
solitaire dst (x :<| q) vis query | dst == x ^. _1 . _1 = x ^. _2
                                  | (x ^. _1 . _1) `member` vis = solitaire dst q vis query
                                  | otherwise = solitaire dst (q <> fromList ((, x ^. _1 : x ^. _2) <$> query (x ^. _1 . _3))) (insert (x ^. _1 . _1) vis) query

naiveLookupIdiom :: [Idiom] -> HeadPinyin -> [QueryItem]
naiveLookupIdiom ims pinyin = unIdiom <$> filter ((== pinyin) . view (to unIdiom . _2)) ims

naiveLookupInfo :: [Idiom] -> IdiomHash -> QueryItem
naiveLookupInfo ims ih = fromJust $ find ((== ih) . view _1) (unIdiom <$> ims)

main :: IO ()
main = do
    raw :: Maybe [Idiom] <- decode . encodeUtf8 <$> T.readFile "idiom.json"
    src                  <- T.unpack <$> T.getLine
    target               <- T.unpack <$> T.getLine
    case raw of
        Nothing -> error "error occurred when reading JSON"
        Just ims ->
            let chain = solitaire target (singleton (naiveLookupInfo ims src, [])) empty (naiveLookupIdiom ims)
            in  case chain of
                    [] -> print "No solution"
                    _  -> putStrLn (intercalate " → " ((view _1 <$> reverse chain) ++ [target]))
