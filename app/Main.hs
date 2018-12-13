{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Generics.Labels     ()
import           Data.Maybe
import           Data.Proxy
import           Data.Text                as T
import           Database.Relational      as R hiding (fromMaybe, list)
import           DataSource2
import           Debug.Trace              (trace)
import qualified Entity.Beer              as Beer
import qualified Entity.Store             as Store
import           GHC.Generics
import           GHC.Int
import           Lucid
import           Network.HTTP.Types       (status404)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Prelude                  hiding (id)
import qualified Prelude                  (id)
import           TypedAdmin
import           TypedAdmin.Util

instance Default Beer.Beer

instance ToDetail Handler Beer.Beer

instance ToForm Handler Beer.Beer

data BeerDetail = BeerDetail
  { id   :: Int64
  , name :: String
  , ibu  :: Maybe Int32
  } deriving (Generic)

data UpdateBeerParam = UpdateBeerParam
  { name :: String
  , ibu  :: Maybe Int32
  } deriving (Generic)

instance ToForm Handler UpdateBeerParam

data BeerSummary = BeerSummary
  { id     :: Int64
  , name   :: String
  , images :: [Image]
  } deriving (Generic, Show)

instance HasHeader BeerSummary

instance ToDetail Handler BeerSummary where
  linkDetail x =
    pure $ Just $ "/beers/" <> tshow (x ^. #id)
  linkEdit x =
    pure $ Just $ "/beers/" <> tshow (x ^. #id) <> "/_edit"

instance ToDetail Handler BeerDetail

instance ToDetail Handler Store.Store
instance HasHeader Store.Store

data SearchBeerParam = SearchBeerParam
  { name :: Maybe String
  , ibu  :: Maybe Int32
  } deriving (Generic, Show)

instance ToForm Handler SearchBeerParam

instance ListConsole Handler BeerSummary SearchBeerParam where
  list p _ = do
    beers <- runQuery (relationalQuery' (f p) []) ()
    return $ fmap r beers
    where
      f (param :: Maybe SearchBeerParam) = relation $ do
        x <- query Beer.beer
        whenJust (param ^? _Just . #name . _Just) $ \v -> wheres $ x ! Beer.name' .=. value v
        whenJust (param ^? _Just . #ibu . _Just) $ \v -> wheres $ x ! Beer.ibu' .=. just (value v)
        return x
      -- mock
      r x = BeerSummary
        { id = Beer.id x
        , name = Beer.name x
        , images = fmap Image ["https://source.unsplash.com/100x100/?beer"]
        }
  listSublayout _ _ b = do
    div_ [] $
      a_ [href_ "/beers/_create"] "add new beer"
    div_ [] b

instance ListConsole Handler Store.Store () where
  list x _ = runQuery (relationalQuery' Store.store []) ()

instance DetailConsole Handler Beer.Beer where
  type Ident Beer.Beer = Int64
  detail i = one <$> runQuery Beer.selectBeer i

instance CreateConsole Handler Beer.Beer Beer.Beer where
  create _ x = do
    insertM Beer.insertBeer x
    pure ()
  createdRedirectPath _ _ = pure "/beers"
  detailForCreate _ _ = pure . Just $ def

toDetailUrl x = "/beers/" `append` (pack . show $ Beer.id x)

instance EditConsole Handler BeerDetail UpdateBeerParam where
  type EditIdent BeerDetail UpdateBeerParam = Int64
  editedRedirectPath _ _ ident = pure $ "/beers/" <> tshow ident <> "/_edit"
  detailForEdit _ ident = do
    b <- one <$> runQuery Beer.selectBeer ident
    case b of
      Just b ->
        return $ Just $ BeerDetail (Beer.id b) (Beer.name b) (Beer.ibu b)
      Nothing -> return Nothing
  edit _ ident x = updateM f ()
    where
      f :: Update ()
      f = update $ \proj -> do
        Beer.name' <-# value (x ^. #name)
        Beer.ibu' <-# value (x ^. #ibu)
        wheres $ proj ! Beer.id' .=. value ident
        return unitPlaceHolder

route :: [Route Handler]
route =
  [ ListR "beers" (Proxy :: Proxy BeerSummary) (Proxy :: Proxy SearchBeerParam)
  , DetailR ("beers" </> var @Int64) (Proxy :: Proxy Beer.Beer)
  , CreateR "beers" (Proxy :: Proxy Beer.Beer) (Proxy :: Proxy Beer.Beer)
  , EditR ("beers" </> var @Int64) (Proxy :: Proxy BeerDetail) (Proxy :: Proxy UpdateBeerParam)
  , ListR "stores" (Proxy :: Proxy Store.Store) (Proxy :: Proxy ())
  ]

application :: forall b . Request -> (Response -> IO b) -> IO b
application req res = res $ responseLBS status404 [("Content-Type", "text/plain")] "not found"

serve = do
  putStrLn "http://localhost:3000/beers"
  dic <- loadDictionary "i18n.yaml"
  run 3000 $ admin route Prelude.id Nothing (either (const Nothing) Just dic) application
main = serve

-- util

memptyToNothing x = if x == mempty then Nothing else Just x

one (x:_) = Just x
one _     = Nothing

debug lbl x = trace (lbl ++ ": " ++ show x) x

tshow :: Show a => a -> Text
tshow = pack . show
