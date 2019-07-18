{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Programs where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Data.Char (isAlphaNum)
import Progress

--------------------------------------------------------------------------------

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

courseForm :: ProfileId -> ProgramId -> AForm Handler Course
courseForm profileId programId = Course
    <$> areq textField (bfs ("Course Name" :: Text)) Nothing
    <*> aopt textareaField (bfs ("Description" :: Text)) Nothing
    <*> areq uniqueSlugField (bfs ("Slug" :: Text)) Nothing
    <*> aopt urlField (bfs ("Url" :: Text)) Nothing
    <*> areq (selectFieldList progress) (bfs ("Progress" :: Text)) (Just Todo)
    <*> pure programId
    <*> pure profileId
  where
    errorMessage :: Text
    errorMessage = "Slug already used"

    uniqueSlugField = 
        checkM validateUniqueSlug $
        check validateAlphanum textField   

    validateUniqueSlug :: Text -> Handler (Either Text Text)
    validateUniqueSlug y = do 
        mCourse <- runDB $ selectFirst [CourseProgramId ==. programId, CourseSlug ==. y] []
        case mCourse of
            Just _ -> return $ Left errorMessage
            Nothing -> return $ Right y

    validateAlphanum y 
        | any (not . isSlug) y = Left ("Slug must be alphanumeric" :: Text)
        | otherwise = Right $ toLower y

    isSlug :: Char -> Bool
    isSlug c = (isAlphaNum c) || (c `elem` ("-_" :: String))

    progress :: [(Text, Progress)]
    progress = [("Todo", Todo), ("Doing", Doing), ("Done", Done)]

--------------------------------------------------------------------------------
getProgramsR :: Text -> Handler Html
getProgramsR profile = do
    muser <- maybeAuthPair
    programs <- runDB $ do 
        maybeProfile <- selectFirst [ProfileName ==. profile] []
        case maybeProfile of
            Just (Entity pId _) -> selectList [ProgramProfileId ==. pId] []
            Nothing -> return []
    mProfileOwner <- maybeProfileOwner profile muser
    articleList <- getArticles profile
    articles <- widgetToPageContent $ generateArticleWidget articleList
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
    articleList <- getArticles profile
    articles <- widgetToPageContent $ generateArticleWidget articleList
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
getProgramR profile progSlug = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profile muser
    mProgram <- maybeProgram profile progSlug
    courses <- case mProgram of
        Just (Entity progId _) -> runDB $ selectList [CourseProgramId ==. progId] []
        Nothing -> return [] -- this is notFound !
    articleList <- getArticlesForCourses courses
    articles <- widgetToPageContent $ generateArticleWidget articleList
    mForm <- case mProfileOwner of
        Just (Entity profId _) -> do
            progId <- case mProgram of
                Just (Entity progId _) -> return progId
                Nothing -> notFound
            form <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ courseForm profId progId
            return $ Just form
        Nothing -> return Nothing
    defaultLayout $ do
        setTitle . toHtml $ progSlug
        $(widgetFile "programs/show")

postProgramR :: Text -> Text -> Handler Html
postProgramR profile progSlug = do
    userEntity <- requireAuthPair
    mProfileOwner <- maybeProfileOwner profile (Just userEntity)
    mProgram <- maybeProgram profile progSlug
    progId <- case mProgram of
        Just (Entity progId _) -> return progId
        Nothing -> notFound
    courses <- runDB $ selectList [CourseProgramId ==. progId] []
    articleList <- getArticlesForCourses courses
    articles <- widgetToPageContent $ generateArticleWidget articleList
    case mProfileOwner of
        Just (Entity profId _) -> do
            ((res, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm 
                                                            $ courseForm profId progId
            case res of
                FormSuccess course -> do
                    courseId <- runDB $ insert course
                    redirect $ ProgramR profile progSlug
                _ -> do
                    let mForm = Just (formWidget, formEnctype)
                    defaultLayout $ do
                        setTitle . toHtml $ progSlug
                        $(widgetFile "programs/show")
        Nothing -> permissionDenied "This action is not permitted in this account"

--------------------------------------------------------------------------------
maybeProfileOwner :: Text -> Maybe (UserId, User) -> Handler (Maybe (Entity Profile))
maybeProfileOwner profile mUser = do 
    case mUser of
        Just (uid, _) -> runDB $ selectFirst [ProfileName ==. profile, ProfileUserId ==. uid] []
        Nothing -> return Nothing

maybeProgram :: Text -> Text -> Handler (Maybe (Entity Program))
maybeProgram profName progSlug = runDB $ do
    mProfile <- getBy $ UniqueName profName
    case mProfile of
        Just (Entity pId _) -> selectFirst [ProgramProfileId ==. pId, ProgramSlug ==. progSlug] []
        Nothing -> return Nothing

filterCourses :: (IsSequence seq, Element seq ~ Entity Course) => seq -> Progress -> seq
filterCourses courses progress = filter (\(Entity _ course)-> courseProgress course == progress) courses

getArticles :: Text -> Handler ([Entity Article])
getArticles profName = runDB $ do
    mProfile <- getBy $ UniqueName profName
    case mProfile of
        Just (Entity pId _) -> selectList [ArticleProfileId ==. pId] [LimitTo 10]
        Nothing -> return []

getArticlesForCourses :: [Entity Course] -> Handler ([Entity Article])
getArticlesForCourses courses = runDB $ do
    let courseIds = map (\(Entity cId _)-> cId) courses
    selectList [ArticleCourseId <-. courseIds] [LimitTo 10]
-------------------------------------------------------------------------------

data ArticleBrief = ArticleBrief { article :: Article
                                 , url :: Route App
                                 }

{--
 - ArticleBrief is: ArticleBrief Article Text (Maybe Text)
 -   interp. An article with a url
 -
 - brief = ArticleBrief { article=article,
 -                      , url=ArticleR "mospina" "cs" "htc1" "intro"
 -                      }
 -
 - fnForArticleBrief :: ArticleBrief -> ...
 - fnForArticleBrief ab =
 -   ... (fnForArticle $ article ab)     ;Article
 -       (fnForRoute $ url  ab)          ;Route
 -
 -- Template rules used:
 -   - compound: 2 fields
 -     - article: reference Article
 -     - url: reference Route App 
 ---------------------------
 - Article is: Article { title=Text, slug=Text, body=Markdown, courseId=CourseId, profileId=ProfileId}
 -  interp. An article with title, slug body that belongs to courseId and profileId
 -
 - fnForArticle :: Article -> ...
 - fnForArticle article =
 -  ... (articleTitle article)
 -      (articleSlug  article)
 -      (fnForMarkdown (articleBody article))
 -      (fnForCourse (articleCourseId article))
 -      (fnForProfile (articleprofileId article))
 -
 - fnForListOfArticles :: [Article] -> ...
 - fnForListOfArticles [] = []
 - fnForListOfArticles (first:rest) = fnForArticle first : fnForListOfArticles rest
 ------------------------------
 - Profile is: Profile {name=Text, userId=UserId}
 -  interp. A user profile with name that belongs to userId
 -
 - fnForProfile :: Profile -> ...
 - fnForProfile profile = 
 -  ... (profileName profile)
 -      (fnForUser $ profileUserId profile)
--} 
      
-- Return a widget with the given list of articles
generateArticleWidget :: [Entity Article] -> Widget
generateArticleWidget articles = do 
    articleBriefs <- handlerToWidget $ mapM (\(Entity _ a)->createArticleBrief a) articles
    $(widgetFile "articles/article")

-- Return a article brief for the given Article.
createArticleBrief :: Article -> Handler ArticleBrief
createArticleBrief article = do
    mProfile <- runDB $ get $ articleProfileId article
    profName <- case mProfile of
        Nothing -> return ""
        Just profile -> return $ profileName profile
    mcourse <- runDB $ get $ articleCourseId article
    (progSlug, cSlug) <- case mcourse of
        Nothing -> return ("", "")
        Just course -> do 
            mprogram <- runDB $ get $ courseProgramId course
            case mprogram of
                Nothing -> return ("", "")
                Just program -> return $ (programSlug program, courseSlug course)       
    return ArticleBrief { article=article
                        , url=ArticleR profName progSlug cSlug (articleSlug article)
                        }
{--                                          
fnForProfile profileId = Text
  ... (profileName profile)
      (fnForUser $ profileUserId profile)

fnForArticle :: Article -> ...
fnForArticle article =
    ArticleBrief { article=article
                 , url= ArticleR (fnForProfile (articleprofileId article))
                                 ...
                                 (fnForCourse (articleCourseId article))
                                 (articleSlug  article)
                 , brief=(fnForMarkdown (articleBody article))
                 } 
--}

