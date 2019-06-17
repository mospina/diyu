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
    mProfileOwner <- maybeProfileOwner profile muser
    defaultLayout $ do
        setTitle . toHtml $ profile
        $(widgetFile "programs/index")

postProgramsR :: Text -> Handler Html
postProgramsR profile = do
    userEntity <- requireAuthPair
    mProfileOwner <- maybeProfileOwner profile (Just userEntity)
    case mProfileOwner of
        Just profileOwner -> redirect $ ProgramsR profile
        Nothing -> permissionDenied "This action is not permitted in this account"

maybeProfileOwner :: Text -> Maybe (UserId, User) -> Handler (Maybe (Entity Profile))
maybeProfileOwner profile mUser = do 
    case mUser of
        Just (uid, _) -> runDB $ selectFirst [ProfileName ==. profile, ProfileUserId ==. uid] []
        Nothing -> return Nothing
