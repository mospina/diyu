{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ArticleSpec (spec) where

import TestImport
import Progress

spec :: Spec
spec = withApp $ do

    describe "getArticleR" $ do
        it "asserts success when article exist" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            ownerCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo
            _article <- createArticle ownerCourse "Programming notes" "prog-notes"

            get $ ArticleR "foo" "computer-science" "how-to-code" "prog-notes"

            statusIs 200
            bodyContains "Markdown text"

        it "asserts NotFound when article doesn't exist" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            ownerCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo

            get $ ArticleR "foo" "computer-science" "how-to-code" "prog-notes"

            statusIs 404
