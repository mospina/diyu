{-# LANGUAGE TemplateHaskell #-}

module Progress where

import Database.Persist.TH

data Progress = Todo | Doing | Done
    deriving (Show, Read, Eq)

derivePersistField "Progress"
