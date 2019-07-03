{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Course where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)
import Yesod.Text.Markdown
import Data.Char (isAlphaNum)
import Handler.Programs (maybeProfileOwner)

--------------------------------------------------------------------------------

articleForm :: ProfileId -> CourseId -> AForm Handler Article
articleForm profileId courseId = Article
    <$> areq textField (bfs ("Title" :: Text)) Nothing
    <*> areq uniqueSlugField (bfs ("Slug" :: Text)) Nothing
    <*> areq markdownField (bfs ("Body" :: Text)) Nothing
    <*> pure courseId
    <*> pure profileId
  where
    errorMessage :: Text
    errorMessage = "Slug already used"

    uniqueSlugField = 
        checkM validateUniqueSlug $
        check validateAlphanum textField   

    validateUniqueSlug :: Text -> Handler (Either Text Text)
    validateUniqueSlug y = do 
        mArticle <- runDB $ selectFirst [ArticleCourseId ==. courseId, ArticleSlug ==. y] []
        case mArticle of
            Just _ -> return $ Left errorMessage
            Nothing -> return $ Right y

    validateAlphanum y 
        | any (not . isSlug) y = Left ("Slug must be alphanumeric" :: Text)
        | otherwise = Right $ toLower y

    isSlug :: Char -> Bool
    isSlug c = (isAlphaNum c) || (c `elem` ("-_" :: String))

--------------------------------------------------------------------------------

getCourseR :: Text -> Text -> Text -> Handler Html
getCourseR profName progSlug course = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profName muser
    mCourse <- maybeCourse profName progSlug course
    cId <- case mCourse of
        Just (Entity cId _) -> return cId
        Nothing -> notFound
    mForm <- case mProfileOwner of 
        Just (Entity profId _) -> do
            form <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ articleForm profId cId
            return $ Just form
        Nothing -> return Nothing
    defaultLayout $ do
        setTitle . toHtml $ course
        $(widgetFile "courses/show")

postCourseR :: Text -> Text -> Text -> Handler Html
postCourseR profName progSlug course = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profName muser
    mCourse <- maybeCourse profName progSlug course
    cId <- case mCourse of
        Just (Entity cId _) -> return cId
        Nothing -> notFound
    case mProfileOwner of 
        Just (Entity profId _) -> do
            ((res, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm
                                                            $ articleForm profId cId
            case res of
                FormSuccess article -> do
                    articleId <- runDB $ insert article
                    redirect $ CourseR profName progSlug course
                _ -> do
                    let mForm = Just (formWidget, formEnctype)
                    defaultLayout $ do
                        setTitle . toHtml $ course
                        $(widgetFile "courses/show")
        Nothing -> permissionDenied "This action is not permitted in this account"

--------------------------------------------------------------------------------

maybeCourse :: Text -> Text -> Text -> Handler (Maybe (Entity Course))
maybeCourse profName progSlug course = runDB $ do
    mProfile <- getBy $ UniqueName profName
    case mProfile of
        Just (Entity pId _) -> do
            mProgram <- selectFirst [ProgramProfileId ==. pId, ProgramSlug ==. progSlug] []
            case mProgram of 
                Just (Entity progId _) -> selectFirst [CourseProfileId ==. pId, CourseProgramId ==. progId, CourseSlug ==. course] []
                Nothing -> return Nothing
        Nothing -> return Nothing
