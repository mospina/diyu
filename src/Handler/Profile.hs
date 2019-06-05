{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

-- data Profile = Profile { name :: Text }

profileForm :: UserId -> AForm Handler Profile
profileForm userId = Profile
    <$> areq textField (bfs ("Profile Name" :: Text)) Nothing
    <*> pure userId

getProfileR :: Handler Html
getProfileR = do
    (uid, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ profileForm uid
    let maybeProfile = Nothing :: Maybe Profile
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
    (uid, user) <- requireAuthPair
    ((res, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ profileForm uid
    let maybeProfile = case res of
            FormSuccess profile -> Just profile
            _ -> Nothing
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")

-- return the string before @ of the given email
getProfileNameFromEmail :: Text -> Text
getProfileNameFromEmail = takeWhile (/='@')

-- get or create the profile of the given user, and returns it's ID
getOrCreateProfile :: UserId -> Handler ProfileId
getOrCreateProfile userId = runDB $ do
    mProfile <- getBy $ UniqueUserId userId
    case mProfile of
        Just (Entity pid _) -> return pid
        Nothing  -> insert $ Profile "example" userId
