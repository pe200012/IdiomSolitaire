{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow                  ( Arrow(..) )
import           Control.Monad                  ( replicateM )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Identity         ( Identity(Identity)
                                                , runIdentity
                                                )
import           DD
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
import           Database.Persist.Sqlite        ( runMigration
                                                , runSqlite
                                                )
import           Lens.Micro                     ( (^.)
                                                , _1
                                                , _2
                                                , _3
                                                , to
                                                )
import           Lens.Micro.Extras              ( view )
import           Prelude                 hiding ( words )
import           Types

solitaire :: Monad m => IdiomHash -> Seq QueryChain -> Set IdiomHash -> (HeadPinyin -> m [QueryItem]) -> m [QueryItem]
solitaire dst Empty vis query = return []
solitaire dst (x :<| q) vis query
  | dst == x ^. _1 . _1 = return (x ^. _2)
  | (x ^. _1 . _1) `member` vis = solitaire dst q vis query
  | otherwise = do
    items <- query (x ^. _1 . _3)
    solitaire dst (q <> fromList ((, x ^. _1 : x ^. _2) <$> items)) (insert (x ^. _1 . _1) vis) query

solve :: String -> String -> IO ()
solve src dst = runSqlite "idiom.sqlite" $ do
  runMigration migrateAll
  src'  <- unIdiom <$> queryIdiom src
  chain <- solitaire dst (singleton (src', [])) empty (fmap (fmap unIdiom) . queryPinyin)
  liftIO $ case chain of
    [] -> print "No solution"
    _  -> putStrLn (intercalate " → " ((view _1 <$> reverse chain) ++ [dst]))

main :: IO ()
main = do
  [src, dst] <- replicateM 2 getLine
  solve src dst
