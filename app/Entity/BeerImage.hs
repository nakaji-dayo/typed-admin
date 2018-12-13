{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Entity.BeerImage where

import           Database.Relational
import           DataSource2         (defineTable)
import           GHC.Generics
import           Prelude             hiding (id)

$(defineTable "beer_image" [''Show, ''Generic, ''Eq])
