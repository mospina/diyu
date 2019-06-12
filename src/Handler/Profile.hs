{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import System.Random
import System.IO.Unsafe

import Data.Char (isAlphaNum)

-- data Profile = Profile { name :: Text }

profileForm :: UserId -> AForm Handler Profile
profileForm userId = Profile
    <$> areq uniqueProfileNameField (bfs ("Profile Name" :: Text)) Nothing
    <*> pure userId
  where
    errorMessage :: Text
    errorMessage = "Profile name already used"   

    uniqueProfileNameField = 
        checkM validateUniqueName $ 
        check validateAlphanum $
        check validateNonReserved textField

    validateNonReserved y
        | elem y ["profile"] = Left errorMessage
        | otherwise = Right y

    validateUniqueName :: Text -> Handler (Either Text Text)
    validateUniqueName y = do 
        mProfile <- runDB $ getBy $ UniqueName y
        case mProfile of
            Just _ -> return $ Left errorMessage
            Nothing -> return $ Right y
    
    validateAlphanum y 
        | any (not . isAlphaNum) y = Left ("Profile name must be alphanumeric" :: Text)
        | otherwise = Right y
        
getProfileR :: Handler Html
getProfileR = do
    (uid, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ profileForm uid
    pid <- getOrCreateProfile uid
    maybeProfile <- runDB $ get pid
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
    (uid, user) <- requireAuthPair
    pid <- getOrCreateProfile uid
    ((res, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ profileForm uid
    case res of
        FormSuccess (Profile pName _) -> do 
            runDB $ update pid [ProfileName =. pName]
            redirect ProfileR
        _ -> do
            maybeProfile <- runDB $ get pid
            defaultLayout $ do
                setTitle . toHtml $ userEmail user <> "'s User page"
                $(widgetFile "profile")

-- return the string before @ of the given email
getProfileNameFromEmail :: Text -> Text
getProfileNameFromEmail email = fromEmailPart ++ randonPart 
    where fromEmailPart = takeWhile (/='@') email 
          randonPart = fromString $ take 10 $ randomRs ('a', 'z') $ unsafePerformIO newStdGen

-- get or create the profile of the given user, and returns it's ID
getOrCreateProfile :: UserId -> Handler ProfileId
getOrCreateProfile userId = runDB $ do
    user <- get userId
    mProfile <- getBy $ UniqueUserId userId
    case mProfile of
        Just (Entity pid _) -> return pid
        Nothing  -> insert $ Profile (getTempProfile user) userId
    where getTempProfile (Just u) = getProfileNameFromEmail $ userEmail u
          getTempProfile Nothing = fromString $ take 10 $ randomRs ('a', 'z') $ unsafePerformIO newStdGen
