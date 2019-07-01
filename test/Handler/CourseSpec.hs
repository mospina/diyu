{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.CourseSpec (spec) where

import TestImport
import Progress

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

--    describe "postCourseR" $ do
--        error "Spec not implemented: postCourseR"
--
