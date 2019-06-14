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

--    describe "postProgramsR" $ do
--        error "Spec not implemented: postProgramsR"

