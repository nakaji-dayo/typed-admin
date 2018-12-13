module TypedAdmin.Util where

import           Control.Monad
import qualified Data.ByteString.UTF8 as BS
import           Data.Maybe
import           Text.Read

lookupMaybe k ps = readMaybe =<< BS.toString <$> join (lookup k ps)

whenJust (Just x) f = f x
whenJust _ _        = return ()

mapSnd f (x, y) = (x, f y)

memptyToNothing :: (Eq a, Monoid a) => a -> Maybe a
memptyToNothing x = if x == mempty then Nothing else Just x


firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f
