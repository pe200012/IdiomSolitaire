{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module DD where

import           Control.Monad.IO.Class
import           Control.Monad.Reader           ( ReaderT )
import           Database.Esqueleto
import           Database.Persist.Sqlite hiding ( (==.)
                                                , count
                                                )
import           Database.Persist.TH
import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Solitaire
   idiom String
   headP String
   tailP String
   deriving Show
|]

convert :: Solitaire -> Idiom
convert (Solitaire i h t) = Idiom (i, h, t)

createNewIdiom :: (MonadIO m) => Idiom -> ReaderT SqlBackend m (Key Solitaire)
createNewIdiom (Idiom (i, h, t)) = insert (Solitaire i h t)

queryIdiom :: (MonadIO m) => String -> ReaderT SqlBackend m Idiom
queryIdiom i = head . fmap (convert . entityVal) <$> select (from $ \p -> where_ (p ^. SolitaireIdiom ==. val i) >> return p)

queryPinyin :: (MonadIO m) => HeadPinyin -> ReaderT SqlBackend m [Idiom]
queryPinyin pinyin = fmap (convert . entityVal) <$> select (from $ \p -> where_ (p ^. SolitaireHeadP ==. val pinyin) >> return p)
