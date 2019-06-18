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
                
            
