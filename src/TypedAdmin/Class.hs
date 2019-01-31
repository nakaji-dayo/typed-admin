{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module TypedAdmin.Class where

import           Control.Monad.Base            (MonadBase)
import           Control.Monad.Catch           hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Data.ByteString
import           Data.Generics.Product.Subtype
import           Data.Proxy
import           Data.Text                     (Text)
import qualified Data.Yaml                     as Y
import           GHC.Generics
import           Lucid                         (HtmlT (..))
import           Network.HTTP.Types


type PathText = Text
type Page = Int

type Dic = Y.Object
data Context = Context
  { dic             :: Maybe Dic
  , localeHierarchy :: [Text]
  }
class MonadState Context m => L18N m

-- class Monad m => MonadState' r (m :: * -> *) | m -> r where
--   ask' :: m r
--   local' :: (r -> r) -> m a -> m a
--   reader' :: (r -> a) -> m a

-- instance Monad m => MonadState' r (ReaderT r m) where
--   ask' = ask
--   local' = local
--   reader' = reader

-- instance MonadState' r m => MonadState' r (HtmlT m) where
--   ask' = lift ask'
--   local' f a = a

-- asks' :: MonadState' r m => (r -> a) -> m a
-- asks' = reader'

newtype Handler a = Handler
  { runHandler :: StateT Context IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadState Context
  , MonadBase IO
  , MonadBaseControl IO
  , MonadThrow
  , MonadCatch
  )

-- class ToName a where
--   toName :: proxy a -> Text

-- 廃止できないか？(ToDetailでカバー?)
class HasHeader a where
  hasHeader :: (Monad m, MonadState Context m) => proxy a -> [HtmlT m ()]
  default hasHeader:: (Monad m, MonadState Context m, Generic a, GHasHeader (Rep a)) => proxy a -> [HtmlT m ()]
  hasHeader x = gHasHeader (Proxy :: Proxy (Rep a))

class GHasHeader (f :: * -> *) where
  gHasHeader :: (Monad m, MonadState Context m) => proxy f -> [HtmlT m ()]

class ToDetailField a where
  toDetailField :: (Monad m, MonadState Context m) => a -> HtmlT m ()

class MonadIO m => ToDetail m a where
  toDetail :: a -> m [(Text, HtmlT m ())]
  default toDetail :: (Generic a, GToDetail (Rep a), MonadState Context m) => a -> m [(Text, HtmlT m ())]
  toDetail x = do
    gToDetail (from x)
    -- case linkDetail x of
    --   Just p ->
    --     td_ [] $ a_ [href_ p] "detail"
    --   Nothing -> return ()
  linkDetail :: a -> m (Maybe PathText)
  linkDetail _ = return Nothing
  linkEdit :: a -> m (Maybe PathText)
  linkEdit _ = return Nothing
  detailTitle :: proxy a -> m Text
  default detailTitle :: (Generic a, GToDetail (Rep a)) => proxy a -> m Text
  detailTitle x = gDetailTitle (Proxy :: Proxy (Rep a))

class GToDetail (f :: * -> *) where
  gToDetail :: (MonadIO m, Monad m2, MonadState Context m2) => f a -> m [(Text, HtmlT m2 ())]
  gDetailTitle :: (Monad m) => proxy f -> m Text

class Monad m =>  ToForm m a where
  toForm :: Maybe a -> m [(Text, (HtmlT m (), Bool))]
  default toForm :: (Generic a, GToForm m (Rep a)) => Maybe a -> m [(Text, (HtmlT m (), Bool))]
  toForm x = gToForm (from <$> x)
  fromForm :: [(ByteString, Maybe ByteString)] -> m (Either Text a)
  default fromForm :: (Generic a, GToForm m (Rep a)) => [(ByteString, Maybe ByteString)] -> m (Either Text a)
  fromForm x = fmap to <$> gFromForm x

class (Monad m, MonadState Context m) => GToForm m (f :: * -> *) where
  gToForm :: Maybe (f a) -> m [(Text, (HtmlT m (), Bool))]
  gFromForm :: [(ByteString, Maybe ByteString)] -> m (Either Text (f a))

class Monad m => FormField m a where
  toFormField :: Text -> Maybe a -> HtmlT m ()
  fromFormField :: [(ByteString, Maybe ByteString)] -> ByteString -> m (Maybe a)
  isVisible :: Maybe a -> m Bool
  isVisible _ = pure True

-- todo: wrap [a]
class (HasHeader a, ToDetail m a, ToForm m b) => ListConsole m a b where
  list :: (Maybe b) -> Page -> m ([a])
  total :: Maybe b -> proxy a -> m (Maybe Int)
  total _ _ = pure Nothing
  listSublayout :: Maybe b -> [a] -> HtmlT m () -> HtmlT m ()
  listSublayout _ _ x = x
-- todo wrap a

class (ToDetail m a) => DetailConsole m a where
  type Ident a
  detail :: Ident a -> m (Maybe a)
  detailSublayout :: a -> HtmlT m () -> HtmlT m ()
  detailSublayout _ x = x

-- class (ToForm m a, MonadIO m) => CreateConsole m a where
--   create :: a -> m ()
--   createPath :: proxy a -> m (PathText, PathText)

-- todo: edit, createを抽象化
-- editのみroute parameterを受け取っているのは間違い
-- note:
-- 小リソースの作成など、作成時に固定で使う値があるため、
-- 作成に使うデータbが、読み込むデータaのsubsetになっている。
-- ただし、aに値を設定する必要がない場合がほとんどであり、使いにくいかも（defで対応している）
-- フォームにできる時点で初期値を持てると言う意味かも
class (ToDetail m a, ToForm m b, Subtype b a) => CreateConsole m a b where
  create :: proxy a -> b -> m ()
  createdRedirectPath :: proxy a -> b -> m PathText
  -- createPath :: a -> proxy b -> m PathText
  detailForCreate :: Query -> proxy b -> m (Maybe a)
  createSublayout :: proxy b -> a -> HtmlT m () -> HtmlT m ()
  createSublayout _ _ x = x

class (ToDetail m a, ToForm m b, Subtype b a) => EditConsole m a b where
  type EditIdent a b
  edit :: proxy a -> EditIdent a b -> b -> m () -- todo: use AllowAmbiguousTypes and TypeApplication?
  -- editPath :: proxy a -> proxy b -> EditIdent a b -> m (PathText, PathText)
  editedRedirectPath :: proxy a -> proxy b -> EditIdent a b -> m PathText
  detailForEdit :: proxy b -> EditIdent a b -> m (Maybe a)
  sublayout :: proxy b -> a -> HtmlT m () -> HtmlT m ()
  sublayout _ _ x = x


class (Monad m, ToForm m a) => DeleteConsole m a where
  type DeleteIdent a
  delete :: DeleteIdent a -> a -> m ()
  deletedRedirectPath :: DeleteIdent a -> a -> m PathText

-- todo: /storeを対応、commentとかに変えて子コードにしてみる
-- todo: layout改善
-- todo: Console間のリンクを改善
