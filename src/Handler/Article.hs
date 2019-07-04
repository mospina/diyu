{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Article where

import Import
import Handler.Course (maybeCourse)
getArticleR :: Text -> Text -> Text -> Text -> Handler Html
getArticleR profName progSlug courseS articleS = do
    mCourse <- maybeCourse profName progSlug courseS
    cId <- case mCourse of
        Just (Entity cId _) -> return cId
        Nothing -> notFound
    mArticle <- runDB $ selectFirst [ ArticleCourseId ==. cId
                                    , ArticleSlug ==. articleS] 
                                    []
    article <- case mArticle of
        Just (Entity _ article) -> return article
        Nothing -> notFound

    defaultLayout $ do
        setTitle . toHtml $ articleS
        $(widgetFile "articles/show")
