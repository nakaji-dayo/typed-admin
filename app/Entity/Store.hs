{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Entity.Store where

import           Database.Relational
import           DataSource2         (defineTable)
import           GHC.Generics
import           Prelude             hiding (id)

$(defineTable "store" [''Show, ''Generic, ''Eq])
