{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Profile page" $ do
        it "asserts no access to my-account for anonymous users" $ do
            get ProfileR
            statusIs 403

        it "asserts access to my-account for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get ProfileR
            statusIs 200

        it "asserts user's information is shown" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity

            get ProfileR
            let (Entity _ user) = userEntity
            htmlAnyContain ".username" . unpack $ userEmail user

        it "asserts form is processed correctly" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            get ProfileR

            request $ do
                setMethod "POST"
                setUrl ProfileR
                addToken
                byLabelContain "Profile Name" "foobar"

            statusIs 200
            htmlAllContain ".upload-response" "foobar"
