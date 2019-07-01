{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Course where

import Import
import Handler.Programs (maybeProfileOwner)

--------------------------------------------------------------------------------

getCourseR :: Text -> Text -> Text -> Handler Html
getCourseR profName progSlug course = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profName muser
    defaultLayout $ do
        setTitle . toHtml $ course
        $(widgetFile "courses/show")

postCourseR :: Text -> Text -> Text -> Handler Html
postCourseR profName progSlug course = do
    muser <- maybeAuthPair
    mProfileOwner <- maybeProfileOwner profName muser
    defaultLayout $ do
        setTitle . toHtml $ course
        $(widgetFile "courses/show")
