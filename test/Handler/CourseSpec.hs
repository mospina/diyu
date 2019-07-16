{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.CourseSpec (spec) where

import TestImport
import Progress
import qualified Handler.Course as C

spec :: Spec
spec = withApp $ do

    describe "getCourseR" $ do
        it "asserts limited access to anonymous users" $ do
            userEntity <- createUser "foo"
            profileEntity <- createProfile userEntity "foo"
            programEntity <- createProgram profileEntity "Computer Science" "computer-science"
            _todoCourse <- createCourse programEntity "How to Code" "how-to-code" Todo

            get $ CourseR "foo" "computer-science" "how-to-code"

            statusIs 200
            bodyNotContains "form"

        it "asserts limited access to users that don't own the profile" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            _todoCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo

            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity

            get $ CourseR "foo" "computer-science" "how-to-code"

            statusIs 200
            bodyNotContains "form"

        it "asserts full access to users that own the profile" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            _todoCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo
            authenticateAs ownerEntity

            get $ CourseR "foo" "computer-science" "how-to-code"

            statusIs 200
            bodyContains "form"

    describe "postCourseR" $ do
        it "gives unauthorized access to anonymous users" $ do
            userEntity <- createUser "foo"
            profileEntity <- createProfile userEntity "foo"
            programEntity <- createProgram profileEntity "Computer Science" "computer-science"
            _todoCourse <- createCourse programEntity "How to Code" "how-to-code" Todo

            post $ CourseR "foo" "computer-science" "how-to-code"

            statusIs 403

        it "gives unauthorized access to users that don't own the profile" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            _todoCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo

            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity

            post $ CourseR "foo" "computer-science" "how-to-code"

            statusIs 403

        it "process form properly" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            _todoCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo
            authenticateAs ownerEntity

            get $ CourseR "foo" "computer-science" "how-to-code"
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl $ CourseR "foo" "computer-science" "how-to-code"
                addToken
                byLabelContain "Title" "Why Computer Science"
                byLabelContain "Slug" "why-computer-science"
                byLabelContain "Body" "# Why Computer Science"

            statusIs 303
            _ <- followRedirect

            htmlAnyContain ".article-item" "Why Computer Science"

    describe "createArticleBrief" $ do
        it "creates the brief for a given article" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            ownerProgram <- createProgram ownerProfile "Computer Science" "computer-science"
            ownerCourse <- createCourse ownerProgram "How to Code" "how-to-code" Todo
            (Entity _ ownerArticle) <- createArticle ownerCourse "Programming notes" "prog-notes"

            articleBrief <- runHandler $ C.createArticleBrief ownerArticle
            assertEq "Article is in articleBrief" (C.article articleBrief) ownerArticle
            assertEq "Url is in articleBrief" 
                     (C.url articleBrief)
                     (ArticleR "foo" "computer-science" "how-to-code" "prog-notes")
            assertEq "Brief is in articleBrief" 
                     (C.brief articleBrief)
                     ("Mardown text")
