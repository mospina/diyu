{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Programs where

import Import

getProgramsR :: Text -> Handler Html
getProgramsR profile = do
    muser <- maybeAuthPair
    let mProfileOwner = maybeProfileOwner profile muser
    defaultLayout $ do
        setTitle . toHtml $ profile
        $(widgetFile "programs/index")

postProgramsR :: Text -> Handler Html
postProgramsR profile = error "Not yet implemented: postProgramsR"

maybeProfileOwner :: Text -> Maybe (UserId, User) -> Maybe Profile
maybeProfileOwner _ Nothing = Nothing
maybeProfileOwner profile (Just (uid, user)) = undefined 
