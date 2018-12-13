{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module TypedAdmin.Router where

import           Control.Monad
import           Data.String
import           Data.Text        (Text, intercalate, pack)
import           Data.Typeable    (Typeable)
import           TypedAdmin.Class
import           Web.HttpApiData

class PathParam a b where
  fromPath :: a -> [Text] -> Maybe b
  renderPath :: a -> b -> [Text]

instance PathParam (Path a) () where
  fromPath x y = f (Just Nothing) x y
    where
      f :: Maybe (Maybe ()) -> (Path b) -> [Text] -> Maybe ()
      f Nothing _ _ = Nothing
      f a EmptyP [] = join a
      f _ (StaticP p1 ps) (x:xs)
        | p1 == x = f (Just $ Just ()) ps xs
        | otherwise = Nothing
      f _ _ _ = Nothing
  renderPath path _ = f [] path
    where
      f :: [Text] -> (Path a) -> [Text]
      f xs EmptyP          = xs
      f xs (StaticP p1 ps) = f (p1:xs) ps

toText = ("/" <>) . intercalate "/"

instance (FromHttpApiData a, ToHttpApiData a) => PathParam (Path '[a]) a where
  fromPath x y = f (Just Nothing) x y
    where
      f :: Maybe (Maybe a) -> (Path b) -> [Text] -> Maybe a
      f Nothing _ _ = Nothing
      f a EmptyP [] = join a
      f a (StaticP p1 ps) (x:xs)
        | p1 == x = f a ps xs
        | otherwise = Nothing
      f (Just Nothing) (VarP ps) (x:xs) = f (Just $ parseUrlPieceMaybe x) ps xs
      f _ _ _ = Nothing
  renderPath path x = f [] path
    where
      f :: [Text] -> (Path b) -> [Text]
      f xs EmptyP          = xs
      f xs (StaticP p1 ps) = f (p1:xs) ps
      f xs (VarP ps)       = f (toUrlPiece x:xs) ps

data Route m where
  ListR :: (ListConsole m a b) => Path c -> p1 a -> p2 b -> Route m
  DetailR :: (DetailConsole m a, PathParam (Path c) (Ident a)) => Path c -> p1 a -> Route m
  CreateR :: CreateConsole m a b => Path c -> p1 a -> p2 b -> Route m
  EditR :: (EditConsole m a b, PathParam (Path c) (EditIdent a b)) => Path c -> p1 a -> p2 b -> Route m
  DeleteR :: (DeleteConsole m a, PathParam (Path c) (DeleteIdent a)) => Path c -> p1 a -> Route m

instance a ~ '[] => IsString (Path a) where
  fromString x = StaticP (pack x) EmptyP

instance a ~ '[] => IsString (Path a -> Path a) where
  fromString x = StaticP (pack x)

data Path (as :: [*]) where
  EmptyP :: Path '[]
  StaticP :: Text -> Path as -> Path as
  VarP :: (FromHttpApiData a, Typeable a) => Path as -> Path (a ': as)
deriving instance Show (Path i)

static = StaticP

var :: (FromHttpApiData a, Typeable a) => Path as -> Path (a : as)
var = VarP

(</>) :: Path as -> (Path as -> Path bs) -> Path bs
(</>) x y = y x
