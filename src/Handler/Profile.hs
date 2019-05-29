{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

data Profile = Profile { name :: Text }

profileForm :: AForm Handler Profile
profileForm = Profile
    <$> areq textField (bfs ("Profile Name" :: Text)) Nothing

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm profileForm
    let profileName = Nothing :: Maybe Profile
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
    (_, user) <- requireAuthPair
    ((res, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm profileForm
    let profileName = case res of
            FormSuccess profile -> Just profile
            _ -> Nothing
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")
