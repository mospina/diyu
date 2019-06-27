{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProgramsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getProgramsR" $ do
        it "asserts limited access to anonymous users" $ do
            userEntity <- createUser "foo"
            _ <- createProfile userEntity "foo"
            get $ ProgramsR "foo"

            statusIs 200
            bodyNotContains "form"

        it "asserts limited access to users that don't own the profile" $ do
            ownerEntity <- createUser "foo"
            _ <- createProfile ownerEntity "foo"

            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity

            get $ ProgramsR "foo"

            statusIs 200
            bodyNotContains "form"

        it "asserts full access to users that own the profile" $ do
            ownerEntity <- createUser "foo"
            _ <- createProfile ownerEntity "foo"
            authenticateAs ownerEntity

            get $ ProgramsR "foo"

            statusIs 200
            bodyContains "form"


    describe "postProgramsR" $ do
        it "gives unauthorized response to anonymous users" $ do
            userEntity <- createUser "foo"
            _ <- createProfile userEntity "foo"
            post $ ProgramsR "foo"
            statusIs 403

        it "gives unauthorized response to users that don't own the profile" $ do
            ownerEntity <- createUser "foo"
            _ <- createProfile ownerEntity "foo"

            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity

            post $ ProgramsR "foo"
            statusIs 403

        it "process form properly" $ do
            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity
            get $ ProgramsR "bar"
            
            request $ do
                setMethod "POST"
                setUrl $ ProgramsR "bar"
                addToken
                byLabelContain "Program Name" "Computer Science"
                byLabelContain "Slug" "computer-science"

            statusIs 303
            _ <- followRedirect

            htmlAllContain ".program-item" "Computer Science"
                
        it "slug is unique for profile" $ do
            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity
            get $ ProgramsR "bar"
            
            request $ do
                setMethod "POST"
                setUrl $ ProgramsR "bar"
                addToken
                byLabelContain "Program Name" "Computer Science"
                byLabelContain "Slug" "computer-science"

            statusIs 303

            get $ ProgramsR "bar"
            request $ do
                setMethod "POST"
                setUrl $ ProgramsR "bar"
                addToken
                byLabelContain "Program Name" "Computer Science Two"
                byLabelContain "Slug" "computer-science"

            statusIs 200
            htmlAnyContain ".error-block" "already used"

            anotherUserEntity <- createUser "foo"
            _ <- createProfile anotherUserEntity "foo"
            authenticateAs anotherUserEntity
            get $ ProgramsR "foo"
            
            request $ do
                setMethod "POST"
                setUrl $ ProgramsR "foo"
                addToken
                byLabelContain "Program Name" "Computer Science"
                byLabelContain "Slug" "computer-science"

            statusIs 303
            _ <- followRedirect

            htmlAllContain ".program-item" "Computer Science"

        it "slug is alphanum" $ do
            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity
            get $ ProgramsR "bar"
            
            request $ do
                setMethod "POST"
                setUrl $ ProgramsR "bar"
                addToken
                byLabelContain "Program Name" "Computer Science"
                byLabelContain "Slug" "computer science"

            statusIs 200
            htmlAnyContain ".error-block" "must be alphanumeric"

        it "slug is saved as lowercase" $ do
            userEntity <- createUser "bar"
            _ <- createProfile userEntity "bar"
            authenticateAs userEntity
            get $ ProgramsR "bar"
            
            request $ do
                setMethod "POST"
                setUrl $ ProgramsR "bar"
                addToken
                byLabelContain "Program Name" "Computer Science"
                byLabelContain "Slug" "Computer-Science"

            statusIs 303
            _ <- followRedirect

            htmlAllContain ".program-item" "computer-science"

    describe "getProgramR" $ do
        it "asserts limited access to anonymous users" $ do
            userEntity <- createUser "foo"
            profileEntity <- createProfile userEntity "foo"
            _ <- createProgram profileEntity "Computer Science" "computer-science"
            get $ ProgramR "foo" "computer-science"

            statusIs 200
            bodyNotContains "form"

        it "asserts limited access to users that don't own the profile" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            _ <- createProgram ownerProfile "Computer Science" "computer-science"

            userEntity <- createUser "bar"
            profileEntity <- createProfile userEntity "bar"
            _ <- createProgram profileEntity "Computer Science" "computer-science"
            authenticateAs userEntity

            get $ ProgramR "foo" "computer-science"

            statusIs 200
            bodyNotContains "form"

        it "asserts full access to users that own the profile" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            _ <- createProgram ownerProfile "Computer Science" "computer-science"
            authenticateAs ownerEntity

            get $ ProgramR "foo" "computer-science"

            statusIs 200
            bodyContains "form"

    describe "postProgramR" $ do
        it "gives unauthorized response to anonymous users" $ do
            userEntity <- createUser "foo"
            profileEntity <- createProfile userEntity "foo"
            _ <- createProgram profileEntity "Computer Science" "computer-science"
            post $ ProgramR "foo" "computer-science"

            statusIs 403

        it "gives unauthorized response to users that don't own the profile" $ do
            ownerEntity <- createUser "foo"
            ownerProfile <- createProfile ownerEntity "foo"
            _ <- createProgram ownerProfile "Computer Science" "computer-science"

            userEntity <- createUser "bar"
            profileEntity <- createProfile userEntity "bar"
            _ <- createProgram profileEntity "Computer Science" "computer-science"
            authenticateAs userEntity

            post $ ProgramR "foo" "computer-science"
            statusIs 403

        it "process form properly" $ do
            userEntity <- createUser "bar"
            profileEntity <- createProfile userEntity "bar"
            _ <- createProgram profileEntity "Computer Science" "computer-science"
            authenticateAs userEntity
            get $ ProgramR "bar" "computer-science"
            
            request $ do
                setMethod "POST"
                setUrl $ ProgramR "bar" "computer-science"    
                addToken
                byLabelContain "Course Name" "How to Code I"
                byLabelContain "Slug" "how-to-code-1"
                byLabelContain "Url" "https://courses.edx.org/courses/how-code"
                byLabelContain "Progress" "2" -- In the form, return values are numeric

            statusIs 303
            _ <- followRedirect

            statusIs 200
            htmlAllContain ".course-item" "How to Code I"
