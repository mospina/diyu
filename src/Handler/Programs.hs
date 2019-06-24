{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Programs where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Data.Char (isAlphaNum)

programForm :: ProfileId -> AForm Handler Program
programForm profileId = Program
    <$> areq textField (bfs ("Program Name" :: Text)) Nothing
    <*> aopt textareaField (bfs ("Description" :: Text)) Nothing
    <*> areq uniqueSlugField (bfs ("Slug" :: Text)) Nothing
    <*> pure profileId
  where
    errorMessage :: Text
    errorMessage = "Slug already used"

    uniqueSlugField = 
        checkM validateUniqueSlug $
        check validateAlphanum textField   

    validateUniqueSlug :: Text -> Handler (Either Text Text)
    validateUniqueSlug y = do 
        mProgram <- runDB $ selectFirst [ProgramProfileId ==. profileId, ProgramSlug ==. y] []
        case mProgram of
            Just _ -> return $ Left errorMessage
            Nothing -> return $ Right y

    validateAlphanum y 
        | any (not . isSlug) y = Left ("Slug must be alphanumeric" :: Text)
        | otherwise = Right $ toLower y

    isSlug :: Char -> Bool
    isSlug c = (isAlphaNum c) || (c `elem` ("-_" :: String))

getProgramsR :: Text -> Handler Html
getProgramsR profile = do
    muser <- maybeAuthPair
    programs <- runDB $ do 
        maybeProfile <- selectFirst [ProfileName ==. profile] []
        case maybeProfile of
            Just (Entity pId _) -> selectList [ProgramProfileId ==. pId] []
            Nothing -> return []
    mProfileOwner <- maybeProfileOwner profile muser
    mForm <- case mProfileOwner of
        Just (Entity pid _) -> do
            form <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ programForm pid
            return $ Just form
        Nothing -> return Nothing
    defaultLayout $ do
        setTitle . toHtml $ profile
        $(widgetFile "programs/index")

postProgramsR :: Text -> Handler Html
postProgramsR profile = do
    userEntity <- requireAuthPair
    programs <- runDB $ do 
        maybeProfile <- selectFirst [ProfileName ==. profile] []
        case maybeProfile of
            Just (Entity pId _) -> selectList [ProgramProfileId ==. pId] []
            Nothing -> return []
    mProfileOwner <- maybeProfileOwner profile (Just userEntity)
    case mProfileOwner of
        Just (Entity pid _) -> do
            ((res, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ programForm pid
            case res of
                FormSuccess program -> do
                    programId <- runDB $ insert program
                    redirect $ ProgramsR profile
                _ -> do
                    let mForm = Just (formWidget, formEnctype)    
                    defaultLayout $ do
                         setTitle . toHtml $ profile   
                         $(widgetFile "programs/index")
        Nothing -> permissionDenied "This action is not permitted in this account"

getProgramR :: Text -> Text -> Handler Html
getProgramR profile programSlug = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profile muser
    defaultLayout $ do
        setTitle . toHtml $ programSlug
        $(widgetFile "programs/show")

postProgramR :: Text -> Text -> Handler Html
postProgramR profile programSlug = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profile muser
    case mProfileOwner of
        Just (Entity pid _) -> do
            defaultLayout $ do
                setTitle . toHtml $ programSlug
                $(widgetFile "programs/show")
        Nothing -> permissionDenied "This action is not permitted in this account"

maybeProfileOwner :: Text -> Maybe (UserId, User) -> Handler (Maybe (Entity Profile))
maybeProfileOwner profile mUser = do 
    case mUser of
        Just (uid, _) -> runDB $ selectFirst [ProfileName ==. profile, ProfileUserId ==. uid] []
        Nothing -> return Nothing
