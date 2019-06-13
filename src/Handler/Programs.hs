{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Programs where

import Import

getProgramsR :: Text -> Handler Html
getProgramsR profile = do
    defaultLayout $ do
        setTitle . toHtml $ profile
        $(widgetFile "programs/index")

postProgramsR :: Text -> Handler Html
postProgramsR profile = error "Not yet implemented: postProgramsR"
