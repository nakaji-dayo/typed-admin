{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module TypedAdmin.Extra where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy      (toStrict)
import qualified Data.ByteString.UTF8      as BS
import           Data.Default.Class
import           Data.Maybe
import           Data.Text                 (pack)
import           Data.Text                 (Text)
import           Data.Text.Encoding        (decodeUtf8)
import           GHC.Generics
import           GHC.Int
import           Lucid
import           Text.Blaze.Renderer.Utf8  (renderMarkup)
import           Text.Heterocephalus
import           TypedAdmin.Class
import           TypedAdmin.Instance       ()
import           TypedAdmin.Util

newtype Image = Image { unImage :: String }
  deriving (Show)

instance ToDetailField Image where
  toDetailField (Image x) = do
    a_ [href_ (pack x), target_ "_blank"] $
      img_ [src_ (pack x)]

newtype HiddenField a = HiddenField { unHiddenField :: a }
  deriving (Generic, Show)

instance Monad m => FormField m (HiddenField String) where
  toFormField n x = input_ [type_ "hidden", name_ n, value_ (pack $ fromMaybe "" $ unHiddenField <$> x)]
  fromFormField ps k = pure $ fmap HiddenField $ memptyToNothing =<< BS.toString <$> join (lookup k ps)

instance Monad m => FormField m (HiddenField Int64) where
  toFormField n x = input_ [type_ "hidden", name_ n, value_ (pack $ fromMaybe "" $ show . unHiddenField <$> x)]
  fromFormField ps k = pure $ fmap HiddenField $ lookupMaybe k ps

data Anchor a = Anchor a Text

instance ToDetailField a => ToDetailField (Anchor a) where
  toDetailField (Anchor x url) = do
    a_ [href_ url] $ toDetailField x

newtype TextArea = TextArea { unTextArea :: String }
  deriving (Show, Default)

instance Monad m => FormField m TextArea where
  toFormField n x = textarea_ [name_ n] $
    toHtml $ fromMaybe "" $ unTextArea <$> x
  fromFormField ps k = pure $ fmap TextArea $ memptyToNothing =<< BS.toString <$> join (lookup k ps)

newtype RangeField a = RangeField { unRangeField :: a }
  deriving (Show, Default)

instance (Monad m, Bounded a, Enum a) => FormField m (RangeField a) where
  toFormField n x = input_ [ type_ "range", name_ n
                           , value_ (pack $ fromMaybe "" $ show . fromEnum . unRangeField <$> x)
                           , step_ "1"
                           , min_ (pack . show $ fromEnum (minBound :: a))
                           , max_ (pack . show $ fromEnum (maxBound :: a))]
  fromFormField ps k = pure $ fmap (RangeField . toEnum) $ lookupMaybe k ps


class GoogleApiEnv m where
  getKey :: m String

data GeoLocation = GeoLocation
  { lat :: Double
  , lon :: Double
  } deriving (Show, Generic)
instance Default GeoLocation

instance (Monad m, GoogleApiEnv m) => FormField m GeoLocation where
  toFormField n v = do
    let vlat = maybe "" (pack . show . lat) v
    input_ [type_ "hidden", id_ "locationLat", name_ (n <> "lat"), value_ vlat]
    let vlon = maybe "" (pack . show . lon) v
    input_ [type_ "hidden", id_ "locationLon", name_ (n <> "lon"), value_ vlon]
    div_ [id_ "map", style_ "height: 400px;"] ""
    script_ (decodeUtf8 $ toStrict $ renderMarkup $(compileTextFile "templates/javascript/map.js"))
    key <- lift $ getKey
    script_ [src_ ("https://maps.googleapis.com/maps/api/js?key=" <> pack key <> "&callback=initMap")] ("" :: String)
   -- async defer></script>
  fromFormField ps k = do
    lat <- fromFormField ps (k <> "lat")
    lon <- fromFormField ps (k <> "lon")
    return $ GeoLocation <$> lat <*> lon

-- newtype JSTTime = JSTTime { unJSTTime :: UTCTime }

-- instance ToDetailField JSTTime where
--   toDetailField x = span_ [] (toHtml ( x))
