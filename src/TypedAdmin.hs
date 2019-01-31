{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypedAdmin
  ( module TypedAdmin.Class
  , module TypedAdmin.Instance
  , module TypedAdmin.Router
  , module TypedAdmin.Extra
  , module TypedAdmin
  ) where

import           Control.Monad.Catch        hiding (Handler)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.UTF8  as LBS
import qualified Data.ByteString.UTF8       as BS
import           Data.Maybe
import           Data.Proxy
import           Data.Text                  (pack, unpack)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Yaml                  as Y
import           Lucid
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse
import           TypedAdmin.Class
import           TypedAdmin.Extra
import           TypedAdmin.Instance
import           TypedAdmin.Router
import           TypedAdmin.Util

type Layout m =  HtmlT m () -> HtmlT m ()

runHandler' ctx a = evalStateT (runHandler a) ctx

admin :: forall m a.
  MonadState Context m => [Route m]
  -> (forall x. m x -> Handler x)
  -> Maybe (Layout m)
  -> Maybe Dic
  -> Middleware
admin rt nt layout dic = \app -> \req res -> do
  let ctx = Context dic []
  case (requestMethod req, pathInfo req) of
    (method, ps) -> do
      let
        f "GET" (ListR path p1 p2) =
          case fromPath path (Prelude.reverse ps) of
            Just () ->
              Just $ handleListConsole nt req res (path, ()) layout ctx p1 p2
            _ -> Nothing
        f "GET" (DetailR path p1) = do
          case fromPath path (Prelude.reverse ps) of
            Just x ->
              Just $ handleDetailConsole nt req res p1 ctx x
            _ -> Nothing
        f "GET" (CreateR path p1 p2) = do
          case fromPath (StaticP "_create" path) (Prelude.reverse ps) of
            Just () ->
              Just $ handleCreateConsole nt req res (path, ()) layout ctx p1 p2
            _ -> Nothing
        f "POST" (CreateR path p1 p2) = do
          case fromPath (path) (Prelude.reverse ps) of
            Just () ->
              Just $ handleCreate nt req res layout p1 p2 ctx
            _ -> Nothing
        f "GET" (EditR path p1 p2) = do
          case fromPath (StaticP "_edit" path) (reverse ps) of
            Just x ->
              Just $ handleEditConsole nt req res (path, x) layout p1 p2 ctx x
            _ -> Nothing
        f "POST" (EditR path p1 p2) = do
          handleEdit nt req res layout p1 p2 ctx <$> fromPath path (reverse ps)
        f "POST" (DeleteR path p1) = do
          case fromPath (StaticP "_delete" path) (reverse ps) of
            Just x ->
              Just $ handleDelete nt req res p1 ctx x
            _ -> Nothing
        f _ _ = Nothing
      case firstJust (f method) rt of
        Just h  -> h
        Nothing -> app req res

-- newtype Handler a = Handler { runHandler :: IO a }

contentType :: (HeaderName, BS.ByteString)
contentType = ("Content-Type", "text/html; charset=UTF-8")

-- todo:: receive func like (m - IO)

handleListConsole :: forall proxy1 proxy2 p1 p2 z m a b.
  (ListConsole m p1 p2, PathParam a b, MonadState Context m)
  => (forall x . m x -> Handler x)
  -> Request
  -> (Response -> IO z)
  -> (a, b)
  -> Maybe (Layout m)
  -> Context
  -> proxy1 p1
  -> proxy2 p2
  -> IO z
handleListConsole nt req res (path, param) layout ctx _ _ = do
  let query = queryString req
  let page = fromMaybe 0 $ lookupMaybe "page" query
  let
    f :: m LBS.ByteString
    f = do
      p <- r2m <$> fromForm query
      beers <- (list :: (Maybe p2) -> Page -> m ([p1])) p page
      mtotal <- total p (Proxy :: Proxy p1)
      let body = renderListHtml beers p (path, param, query) (page, mtotal)
      renderBST $ (fromMaybe defaultLayout layout) body
  body <- runHandler' ctx $ nt f
  res $
    responseLBS status200 [contentType] body

r2m (Right x) = Just x
r2m _         = Nothing

handleDetailConsole :: forall proxy1 p1 b m.
  (DetailConsole m p1,  MonadState Context m)
  => (forall x . m x -> Handler x)
  -> Request
  -> (Response -> IO b)
  -> proxy1 p1
  -> Context
  -> Ident p1
  -> IO b
handleDetailConsole nt req res _ ctx rid = do
  mbody <- runHandler' ctx $ nt $ do
    mr <- detail rid :: m (Maybe p1)
    case mr of
      Just r -> do
        Just <$> renderBST (renderDetailHtml r)
      Nothing -> pure Nothing
  case mbody of
     Just x ->
       res $ responseLBS status200 [("Content-Type", "text/html")] x
     Nothing ->
       res404 res

handleCreateConsole :: forall proxy1 proxy2 a b z m c d.
  (CreateConsole m a b, PathParam c d, MonadState Context m)
  => (forall x. m x -> Handler x)
  -> Request
  -> (Response -> IO z)
  -> (c, d)
  -> Maybe (Layout m)
  -> Context
  -> proxy1 a
  -> proxy2 b
  -> IO z
handleCreateConsole nt req res path layout ctx _ _ = do
  let query = queryString req
  body <- runHandler' ctx $ nt $ do
    mr <- detailForCreate query (Proxy :: Proxy b) :: m (Maybe a)
    case mr of
      Just r ->
        Just <$> (renderBST $ (fromMaybe defaultLayout layout) (toCreateForm r (Proxy :: Proxy b) path))
      Nothing ->
        pure Nothing
  case body of
    Just x ->
      res $ responseLBS status200 [contentType] x
    Nothing -> res404 res

handleCreate :: forall proxy1 proxy2 a b z m.
  (CreateConsole m a b, MonadState Context m)
  => (forall x. m x -> Handler x)
  -> Request
  -> (Response -> IO z)
  -> Maybe (Layout m)
  -> proxy1 a
  -> proxy2 b
  -> Context
  -> IO z
handleCreate nt req res layout _ _ ctx = do
  (ps, _) <- parseRequestBody lbsBackEnd req
  print ps
  mx <- runHandler' ctx $ nt $ fromForm (mapSnd Just <$> ps)
  case mx of
    Right x -> handleAll (exHandler ctx nt layout res) $ do
      path <- runHandler' ctx $ nt $ do
        create (Proxy :: Proxy a) (x :: b)
        createdRedirectPath (Proxy :: Proxy a) (x :: b)
      res $ responseLBS status302 [contentType, ("Location", BS.fromString $ unpack path)] "not found"
    Left x -> do
      body <- runHandler' ctx $ nt $ do
        renderBST $ (fromMaybe defaultLayout layout) (renderErrorHtml x)
      res $ responseLBS status200 [contentType] body

exHandler ctx nt layout res e = do
  body <- runHandler' ctx $ nt $ do
    renderBST $ (fromMaybe defaultLayout layout) (renderErrorHtml (pack $ displayException e))
  res $ responseLBS status200 [contentType] body


handleEditConsole :: forall proxy1 proxy2 a b c d z m.
  (EditConsole m a b, PathParam c d, MonadState Context m)
  => (forall x. m x -> Handler x)
  -> Request
  -> (Response -> IO z)
  -> (c, d)
  -> Maybe (Layout m)
  -> proxy1 a
  -> proxy2 b
  -> Context
  -> EditIdent a b
  -> IO z
handleEditConsole nt req res path layout _ _ ctx rid = do
  body <- runHandler' ctx $ nt $ do
    mr <- detailForEdit (Proxy :: Proxy b) rid :: m (Maybe a)
    case mr of
      Just r ->
        fmap Just $ renderBST $ (fromMaybe defaultLayout layout) (renderEditHtml r (Proxy :: Proxy b) rid path)
      Nothing -> pure Nothing
  case body of
    Just b ->
      res $ responseLBS status200 [contentType] b
    Nothing -> res400 res (LBS.fromString "not found")

handleEdit :: forall proxy1 proxy2 a b z m.
  (EditConsole m a b, MonadState Context m)
  => (forall x. m x -> Handler x)
  -> Request
  -> (Response -> IO z)
  -> Maybe (Layout m)
  -> proxy1 a
  -> proxy2 b
  -> Context
  -> EditIdent a b
  -> IO z
handleEdit nt req res layout _ _ ctx rid = do
  (ps, _) <- parseRequestBody lbsBackEnd req
  mx <- runHandler' ctx $ nt $ fromForm (mapSnd Just <$> ps)
  case mx of
    Right x -> do
      editP <- runHandler' ctx $ nt $ do
        edit (Proxy :: Proxy a) rid (x :: b)
        editedRedirectPath (Proxy :: Proxy a) (Proxy :: Proxy b) rid
      res $ responseLBS status302 [contentType, ("Location", BS.fromString $ unpack editP)] "not found"
    Left x -> do
      body <- runHandler' ctx $ nt $ do
        renderBST $ (fromMaybe defaultLayout layout) (renderErrorHtml x)
      res $ responseLBS status200 [contentType] body


handleDelete :: forall proxy1 a z m. (DeleteConsole m a)
  => (forall x. m x -> Handler x)
  -> Request
  -> (Response -> IO z)
  -> proxy1 a
  -> Context
  -> DeleteIdent a
  -> IO z
handleDelete nt req res _ ctx rid = do
  (ps, _) <- parseRequestBody lbsBackEnd req
  mx <- runHandler' ctx $ nt $ fromForm (mapSnd Just <$> ps)
  case mx of
    Right x -> do
      editP <- runHandler' ctx $ nt $ do
        delete rid (x :: a)
        deletedRedirectPath rid x
      res $ responseLBS status302 [contentType, ("Location", BS.fromString $ unpack editP)] "not found"
    Left x -> res400 res (LBS.fromStrict $ encodeUtf8 x)

res400 res str = res $ responseLBS status400 [contentType] str
res404 res = res $ responseLBS status404 [("Content-Type", "text/plain")] "not found"

defaultLayout :: Monad m => HtmlT m a -> HtmlT m a
defaultLayout x = do
  doctypehtml_ $ do
    head_ [] $ do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [rel_ "stylesheet", href_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"]
      script_ [src_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.js"] ("" :: String)
    body_ [] x

loadDictionary :: FilePath -> IO (Either ParseException Dic)
loadDictionary path = do
  decodeFileEither path
