{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module TypedAdmin.Instance where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import qualified Data.ByteString.UTF8          as BS
import           Data.Generics.Product.Subtype
import qualified Data.HashMap.Strict           as M
import           Data.Maybe
import           Data.Proxy
import           Data.Text                     as T (Text, dropWhile, pack)
import           Data.Time
import           Data.Yaml                     as Y
import           GHC.Generics
import           GHC.Int
import           Lucid
import           Network.HTTP.Types.URI        (Query, renderQuery)
import           TypedAdmin.Class
import           TypedAdmin.Router
import           TypedAdmin.Util

pushH :: MonadState Context m => Text -> m a -> m a
pushH k f = do
  modify (\x -> x { localeHierarchy = k:localeHierarchy x})
  r <- f
  modify (\x -> x { localeHierarchy = Prelude.tail (localeHierarchy x)})
  return r

toLocal :: MonadState Context m => Text -> m Text
toLocal k = do
  dic <- dic <$> get
  h <- localeHierarchy <$> get
  let k' = T.dropWhile (== '_') k
  let f d [] h k'' =
        case (M.lookup k'' =<< d, h) of
          (Just (Y.String x), _)     -> return x
          (_, _:h')                  -> f dic h' h' k''
          (Just (Y.Object dic'), []) -> f (Just dic') [] [] "."
          (_, [])                    -> return (if k'' == "." then k' else k'')
      f d (h':hs) h k'' =
        case M.lookup h' =<< d of
          Just (Y.Object dic') -> f (Just dic') hs h k''
          _                    -> f dic (tail h) (tail h) k''
  f dic h h k'

showLocal :: (Show a, MonadState Context m) => a -> m Text
showLocal = toLocal . pack . show

-- instance {-# OVERLAPS #-} (Show a) => ToHtml a where
--   toHtml = toHtml . show
--   toHtmlRaw = toHtmlRaw .show

-- instance ToHtml a => ToHtml (Maybe a) where
--   toHtml x = Data.Maybe.fromMaybe (toHtml ("" :: String)) $ toHtml <$> x
--   toHtmlRaw = toHtmlRaw

instance ToDetailField String where
  toDetailField x = span_ [] (toHtml x)

instance ToDetailField Bool where
  toDetailField x = do
    l <- lift $ showLocal x
    span_ [] (toHtml l)

instance ToDetailField Integer where
  toDetailField x = span_ [] (toHtml (show x))
instance ToDetailField Int where
  toDetailField x = toDetailField (fromIntegral x :: Integer)
instance ToDetailField Int32 where
  toDetailField x = toDetailField (fromIntegral x :: Integer)
instance ToDetailField Int64 where
  toDetailField x = toDetailField (fromIntegral x :: Integer)

instance ToDetailField Day where
  toDetailField x = span_ [] (toHtml (show x))

instance ToDetailField UTCTime where
  toDetailField x = span_ [] (toHtml (show x))

instance ToDetailField LocalTime where
  toDetailField x = span_ [] (toHtml (show x))

instance ToDetailField a => ToDetailField (Maybe a) where
  toDetailField x = fromMaybe (span_ [] "")$ toDetailField <$> x

instance {-# OVERLAPPABLE #-} ToDetailField a => ToDetailField [a] where
  toDetailField xs = do
    ul_ [] $
      forM_ (toDetailField <$> xs) $ \x -> do
        li_ [] x

instance {-# OVERLAPPABLE #-} (Show a) =>  ToDetailField a where
  toDetailField x = do
    l <- lift $ showLocal x
    span_ [] (toHtml l)

-- instance GToDetail V1 where
--   gToDetail _ = error "toRow V1"

-- instance GToDetail U1 where
--   gToDetail _ = error "toRow U1"

instance (GToDetail f, GToDetail g) => GToDetail (f :*: g) where
  gToDetail (f :*: g) = do
    x <- gToDetail f
    y <- gToDetail g
    return $ x <> y
  gDetailTitle _ = pure "*"

-- instance (GToDetail f, GToDetail g) => GToDetail (f :+: g) where
--   gToDetail _ = error "toRow sum type"

instance (ToDetailField c, Selector s) => GToDetail (M1 S s (K1 i c)) where
  gToDetail x =
    let (M1 (K1 c)) = x
    in return [(pack (selName x), toDetailField c)]
  gDetailTitle _ = pure "--"

instance {-# OVERLAPPABLE #-} (GToDetail f, Constructor c) => GToDetail (M1 C c f) where
  gToDetail (M1 x) = do
    gToDetail x
  gDetailTitle x = pure $ pack $ conName (undefined :: M1 C c rep ())

instance {-# OVERLAPPABLE #-} (GToDetail f) => GToDetail (M1 i t f) where
  gToDetail (M1 x) = do
    gToDetail x
  gDetailTitle _ = gDetailTitle (Proxy :: Proxy f)
  -- gDetailTitle x = pure $ pack $ conName (undefined :: M1 C t f ())

-- instance  {-# OVERLAPS #-} (ToDetail a, HasHeader a) => ToHtml [a] where
--   toHtml xs = do
--     table_ $ do
--       tr_ $ mapM_ th_ $ hasHeader (Proxy :: Proxy a)
--       forM_ xs $ (tr_ []) . toRow
--   toHtmlRaw = toHtml

-- instance GHasHeader V1 where
--   gHasHeader _ = error "hasHeader V1"

-- instance GHasHeader U1 where
--   gHasHeader _ = error "hasHeader U1"

instance (GHasHeader f, GHasHeader g) => GHasHeader (f :*: g) where
  gHasHeader _ = gHasHeader (Proxy :: Proxy f) ++ gHasHeader (Proxy :: Proxy g)

-- instance GHasHeader (f :+: g) where
--   gHasHeader _ = error "hasHeader sum type"

-- instance GHasHeader (K1 i c) where
--   gHasHeader (K1 x) = error "K1"

instance (Selector s) => GHasHeader (M1 S s k) where
  gHasHeader _ =
    [ do
        l <- lift $ toLocal $ pack $ selName (undefined :: t s k a)
        toHtml l
    ]
instance GHasHeader f => GHasHeader (M1 C t f) where
  gHasHeader _ = gHasHeader (Proxy :: Proxy f)

instance GHasHeader f => GHasHeader (M1 D t f) where
  gHasHeader _ = gHasHeader (Proxy :: Proxy f)

instance (GToForm m f, GToForm m g) => GToForm m (f :*: g) where
  gToForm (Just (f :*: g)) = do
    x <- gToForm (Just f)
    y <- gToForm (Just g)
    return $ x ++ y
  gToForm _ = do
    x <- gToForm (Nothing :: Maybe (f a))
    y <- gToForm (Nothing :: Maybe (g a))
    return $ x ++ y
  gFromForm ps = do
    x <- (gFromForm ps)
    y <- (gFromForm ps)
    pure $ liftA2 (:*:) x y

instance GToForm m f => GToForm m (M1 D t f) where
  gToForm (Just (M1 x)) = gToForm (Just x)
  gToForm _             = gToForm (Nothing :: Maybe (f a))
  gFromForm ps = do
    x <- gFromForm ps
    pure $ M1 <$> x

instance GToForm m f => GToForm m (M1 C t f) where
  gToForm (Just (M1 x)) = gToForm (Just x)
  gToForm _             = gToForm (Nothing :: Maybe (f a))
  gFromForm ps = do
    x <- gFromForm ps
    pure $ M1 <$> x

instance forall s m c i. (Selector s, FormField m c, MonadState Context m) => GToForm m (M1 S s (K1 i c)) where
  gToForm x = do
    let k = (\(M1 (K1 k)) -> k) <$> x
        sname = selName (undefined :: t s (K1 i (Maybe a)) p)
    visible <- isVisible k
    pure [(pack sname, (toFormField (pack $ sname) k, visible))]
  gFromForm ps = do
    let sname = selName (undefined :: t s (K1 i (Maybe a)) p)
    mx <- fromFormField ps (BS.fromString $ sname)
    case mx of
      Just x  -> pure $ Right $ M1 $ K1 x
      Nothing -> do
        lname <- toLocal $ pack sname
        msg <- toLocal $ "is_invalid_value"
        pure $ Left (lname <> " " <> msg)

instance Monad m => FormField m Bool where
  toFormField n x =
    let chkd = if (fromMaybe False x) then [checked_] else []
    in input_ $ [type_ "checkbox", name_ n] ++ chkd
  fromFormField ps k = pure $ Just $ isJust $ join (lookup k ps)

-- instance {-# OVERLAPS #-} (Integral a, Show a, Read a) => FormField a where
--   toFormField n x = input_ [type_ "number", name_ n, value_ (pack $ fromMaybe "" (show <$> x))]
--   fromFormField ps k = lookupMaybe k ps

toHtmlInput t n x = input_ [type_ t, name_ n
                           , value_ (pack $ fromMaybe "" (show <$> x))
                           , required_ ""]
toHtmlInput' t n x = input_ [type_ t, name_ n
                           , value_ (pack $ fromMaybe "" (show <$> join x))
                           ]
fromHtmlInput ps k = pure $ lookupMaybe k ps
fromHtmlInput' ps k =
  let x = BS.toString <$> join (lookup k ps)
  in pure $ Just $ memptyToNothing =<< x

-- todo: Read,ShowなものはすべてtoTypeAttr実装し共通実装では？
instance Monad m => FormField m String where
  toFormField n x = input_ [type_ "text", name_ n, value_ (pack $ fromMaybe "" x), required_ ""]
  fromFormField ps k = pure (BS.toString <$> join (lookup k ps))

instance Monad m => FormField m Int where
  toFormField = toHtmlInput "number"
  fromFormField = fromHtmlInput

instance Monad m =>  FormField m Int32 where
  toFormField = toHtmlInput "number"
  fromFormField = fromHtmlInput

instance Monad m => FormField m Int64 where
  toFormField = toHtmlInput "number"
  fromFormField = fromHtmlInput

instance Monad m => FormField m Double where
  toFormField = toHtmlInput "text"
  fromFormField = fromHtmlInput

instance Monad m => FormField m Day where
  toFormField = toHtmlInput "date"
  fromFormField = fromHtmlInput

-- とりあえず、雑にしとく

instance {-# OVERLAPS #-} FormField m a => FormField m (Maybe a) where
  toFormField n x = toFormField n (join x)
  fromFormField ps k = do
    Just <$> fromFormField ps k
     -- todo: Justが出てくる箇所多分型間違ってるので、見直す
  isVisible x = isVisible (join x)

instance {-# OVERLAPPING #-} Monad m => FormField m (Maybe String) where
  toFormField n x = input_ [type_ "text", name_ n, value_ (pack $ fromMaybe "" $ join x)
                           , class_ "maybe"]
  fromFormField ps k =
    let x = BS.toString <$> join (lookup k ps)
    in pure $ Just $ memptyToNothing =<< x

instance {-# OVERLAPPING #-} Monad m => FormField m (Maybe Int) where
  toFormField = toHtmlInput' "number"
  fromFormField ps k = Just <$> fromFormField ps k

instance {-# OVERLAPPING #-} Monad m =>  FormField m (Maybe Int32) where
  toFormField = toHtmlInput' "number"
  fromFormField ps k = Just <$> fromFormField ps k

instance {-# OVERLAPPING #-} Monad m => FormField m (Maybe Int64) where
  toFormField = toHtmlInput' "number"
  fromFormField ps k = Just <$> fromFormField ps k

instance {-# OVERLAPPING #-} Monad m => FormField m (Maybe Double) where
  toFormField = toHtmlInput' "text"
  fromFormField ps k = Just <$> fromFormField ps k

instance {-# OVERLAPPING #-} Monad m => FormField m (Maybe Day) where
  toFormField = toHtmlInput' "date"
  fromFormField ps k = Just <$> fromFormField ps k

newtype SelectForm a = SelectForm { unSelectForm :: a}
  deriving (Show)


selectFormField' :: forall a m.
  (Enum a, Bounded a, Show a, Read a, Monad m, MonadState Context m) =>
  Bool -> Text -> Maybe (SelectForm a) -> HtmlT m ()
selectFormField' required n x = do
  let min = minBound :: a
      max = maxBound :: a
  select_ [name_ n] $ do
    when (not required) $ option_ [value_ ""] ""
    forM_ [min .. max] $ \o -> do
      let sed = case x of
            Just (SelectForm x)
              | fromEnum o == fromEnum x -> [selected_ ""]
              | otherwise -> []
            _ -> []
      l <- lift $ toLocal $ pack $ show o
      option_ ([value_ $ pack $ show $ fromEnum o] ++ sed) $ toHtmlRaw l

fromSelectFormField ps k = pure $ fmap (SelectForm . toEnum) $ lookupMaybe k ps

instance {-# OVERLAPPING #-} (Enum a, Bounded a, Show a, Read a, Monad m, MonadState Context m) => FormField m (Maybe (SelectForm a)) where
  toFormField n x = selectFormField' False n (join x)
  fromFormField ps k = Just <$> fromSelectFormField ps k

instance (Enum a, Bounded a, Show a, Read a, Monad m, MonadState Context m) => FormField m (SelectForm a) where
  toFormField n x = selectFormField' True n x
  fromFormField = fromSelectFormField


instance Monad m => ToForm m () where
  toForm _ = pure mempty
  fromForm _ = pure $ pure mempty

renderListHtml ::
  forall a b c d m . (Monad m, ListConsole m a b, PathParam c d, MonadState Context m)
  => [a]
  -> Maybe b
  -> (c, d, Query)
  -> (Int, Pager)
  -> HtmlT m ()
renderListHtml xs p (path, param, query) (page, pager) = do
  let pathText = toText $ renderPath path param
  -- liftIO $ print $ show (Proxy :: Proxy a)
  let hs = hasHeader (Proxy :: Proxy a)
  title <- lift $ detailTitle (Proxy :: Proxy a)
  title' <- toLocal title
  h1_ [] $ toHtml $ title'
  listSublayout p xs $ div_ [class_ ("_typed_admin_list_" <> title)] $ do
    pushH title $ do
      form_ [method_ "GET", action_ pathText, class_ "_typed_admin_search_form"] $ do
        form <- lift $ toForm p
        div_ [class_ "_typed_admin_fields"] $ do
          forM_ form $ \(name, (fld, visible)) -> do
            div_ [] $ do
              dt_ [] $ do
                when visible $ do
                  name' <- toLocal name
                  label_ [] (toHtml name')
              dd_ [] $
                  fld
        div_ [class_ "_typed_admin_actions"] $
          when (or $ snd . snd <$> form) $ do
            dt_ [] "action"
            dd_ [] $ do
              lblSearch <- toLocal "search"
              input_ [type_ "submit", value_ lblSearch, class_ "mdc-button"]
      table_ [] $ do
        thead_ [] $
          hs
        tbody_ [] $ do
          forM_ xs $ \row -> do
            detail <- lift $ toDetail row
            tr_ [] $ do
              forM_ detail $ \(n, cell) -> do
                td_ [class_ ("_typed_admin_field_" <> n)] cell
              td_ [class_ "_typed_admin_actions"] $
                ul_ [] $ do
                  editLbl <- toLocal "edit"
                  forM_ [("detail", linkDetail), (editLbl, linkEdit)] $ \(l, g) -> do
                    mp <- lift $ g row
                    ll <- toLocal l
                    whenJust mp $ \p -> li_ [] $ a_ [href_ p] $ toHtml ll
              --     forM_ (catMaybes
              --            [ (\u -> a_ [href_ u] "detail") <$> linkDetail row
              --            , (\u -> a_ [href_ u] "edit") <$> linkEdit row
              --            ]
              --           ) $ \a -> li_ [] a
      div_ [class_ "_typed_admin_pager"] $ do
        prev <- lift $ toLocal "<"
        next <- lift $ toLocal ">"
        first <- lift $ toLocal "<<"
        last <- lift $ toLocal ">>"
        let rQuery q = pack (BS.toString (renderQuery True q))
            pageQ d = rQuery (setPageQuery (page + d) query)
        case pager of
          Total t -> do
            when (page > 0 && t /= 0) $ do
              a_ [href_ (pathText <> rQuery (setPageQuery 0 query))] $ toHtml first
              a_ [href_ (pathText <> pageQ (-1))] $ toHtml prev
            when (page - pagerScope > 0) $
              span_ "..."
            forM_ [0..(t - 1)] $ \p -> do
              let sp = toHtml $ show (p + 1)
              if page == p
              then
                span_ $ sp
              else
                when (page - pagerScope <= p && p <= page + pagerScope) $
                  a_ [href_ (pathText <> rQuery (setPageQuery p query))] sp
            when (page + pagerScope < t - 1) $
              span_ "..."
            when (t - 1 /= page && t /= 0) $ do
              a_ [href_ (pathText <> pageQ 1)] $ toHtml next
              a_ [href_ (pathText <> rQuery (setPageQuery (t-1) query))] $ toHtml last
          Auto -> do
            when (page > 0) $ do
              a_ [href_ (pathText <> pageQ (-1))] $ toHtml prev
              a_ [href_ (pathText <> pageQ 1)] $ toHtml next
          None -> return ()

pagerScope = 20

setPageQuery page query =
  let p = ("page", Just $ BS.fromString $ show (page))
  in p:(filter ((/= "page") . fst) query)

renderDetailHtml :: forall a b m .
  (Monad m, DetailConsole m a, MonadState Context m)
  => a -> HtmlT m ()
renderDetailHtml x = do
  title <- lift $ detailTitle (Proxy :: Proxy a)
  title' <- toLocal title
  detail <- lift $ toDetail x
  div_ [class_ ("_typed_admin_detail_" <> title)] $ do
    h1_ [] $ toHtml $ title'
    detailSublayout x $ do
      pushH title $ do
        dl_ [] $ do
          forM_ detail $ \(name, field) -> do
            dt_ [] (toHtml name)
            dd_ [] field

toCreateForm :: forall a b c d proxy m .
  (Monad m, CreateConsole m a b, PathParam c d, MonadState Context m)
  => a
  -> proxy b
  -> (c, d)
  -> HtmlT m ()
toCreateForm x _ (path, param) = do
  title <- lift $ detailTitle (Proxy :: Proxy a)
  title' <- toLocal title
  div_ [class_ ("_typed_admin_detail_" <> title)] $ pushH title $ do
    h1_ [] $ toHtml $ title'
    detail <- lift $ toDetail x
    form <- lift $ toForm (Just (upcast x :: b))
    let pathText = toText $ renderPath path param
    let r =
          form_ [method_ "POST", action_ pathText] $ do
            dl_ $ do
              forM_ detail $ \(name, detailField) -> do
                name' <- toLocal name
                let ffield = lookup name form
                dt_ [class_ ("_typed_admin_label_" <> name)] $ do
                  when (maybe True snd ffield) $ do
                    label_ [] (toHtml name')
                dd_ [class_ ("_typed_admin_field_" <> name)] $
                  fromMaybe (detailField) $ fmap fst ffield
              dt_ $ do
                lblActions <- toLocal "actions"
                p_ [] (toHtml lblActions)
              dd_ $ do
                lblSubmit <- toLocal "create"
                input_ [type_ "submit", value_ lblSubmit]

    createSublayout (Proxy :: Proxy b) x r

renderEditHtml :: forall a b c d proxy m .
  (Monad m, EditConsole m a b, PathParam c d, MonadState Context m)
  => a
  -> proxy b
  -> EditIdent a b
  -> (c, d)
  -> HtmlT m ()
renderEditHtml x _ ident (path, param) = do
  title <- lift $ detailTitle (Proxy :: Proxy a)
  title' <- toLocal title
  div_ [class_ ("_typed_admin_detail_" <> title)] $ do
    h1_ [] $ toHtml $ title'
    detail <- lift $ toDetail x
    form <- lift $ toForm (Just (upcast x :: b))
    let pathText = toText $ renderPath path param
    sublayout (Proxy :: Proxy b) x $ pushH title $ do
      form_ [method_ "POST", action_ pathText] $ do
        dl_ $ do
          forM_ detail $ \(name, detailField) -> do
            let ffield = lookup name form
            dt_ [class_ ("_typed_admin_label_" <> name)] $ do
              when (maybe True snd ffield) $ do
                name' <- toLocal name
                label_ [] (toHtml name')
            dd_ [class_ ("_typed_admin_field_" <> name)] $
              fromMaybe (detailField) $ fmap fst ffield
          dt_ $ do
            lblActions <- toLocal "actions"
            p_ [] (toHtml lblActions)
          dd_ $ do
            lblSubmit <- toLocal "save"
            input_ [type_ "submit", value_ lblSubmit]

renderDeleteHtml :: forall a b c d proxy m .
  (Monad m, DeleteConsole m a, PathParam c d, MonadState Context m)
  => DeleteIdent a
  -> a
  -> (c, d)
  -> HtmlT m ()
renderDeleteHtml ident x (path, param) = do
  form <- lift $ toForm (Just x)
  let pathText = toText $ renderPath path param
  confirm <- lift $ toLocal "do_you_really_want_to_delete?"
  form_ [ method_ "POST", action_ pathText
        , onsubmit_ ("return confirm('" <> confirm <> "');")] $ do
    forM_ form (fst . snd)
    lblSubmit <- toLocal "delete"
    input_ [type_ "submit", value_ lblSubmit]


renderErrorHtml :: forall m .
  (Monad m, MonadState Context m) => Text -> HtmlT m ()
renderErrorHtml msg = do
  title <- toLocal "error_occured"
  back <- toLocal "back"
  h1_ [] $ toHtml title
  p_ [] $ toHtml msg
  a_ [href_ "#", onclick_ "javascript:window.history.back(-1);return false;"] $ toHtml back

-- instance (ToHtml c) => GToEditForm (K1 i c) where
--   gToEditForm (K1 x) = do
--     td_ (toHtml x)

-- instance (GToEditForm f) => GToEditForm (M1 i t f) where
--   gToEditForm (M1 x) = do
--     gToEditForm x


-- todo: create router library, and refactor listPath andcreatePath, etc.
