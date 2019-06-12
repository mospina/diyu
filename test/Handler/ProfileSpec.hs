{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import TestImport
import Handler.Profile (getProfileNameFromEmail)

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
            userEntity <- createUser "bar@foo.com"
            authenticateAs userEntity

            get ProfileR
            let (Entity _ user) = userEntity
            htmlAnyContain ".username" . unpack $ userEmail user
            htmlAllContain ".upload-response" "bar"

        it "asserts form is processed correctly" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            get ProfileR

            request $ do
                setMethod "POST"
                setUrl ProfileR
                addToken
                byLabelContain "Profile Name" "foobar"

            statusIs 303
            _ <- followRedirect

            htmlAllContain ".upload-response" "foobar"
        
        it "asserts profile name is given correctly" $
            let condition = isPrefixOf "foo" $ getProfileNameFromEmail "foo@bar.com"
            in assertEq "Name of foo@bar.com is foo" condition True

        it "asserts form doesn't accept reserved words" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            get ProfileR

            request $ do
                setMethod "POST"
                setUrl ProfileR
                addToken
                byLabelContain "Profile Name" "profile"

            statusIs 200
            htmlAnyContain ".error-block" "already used"

        it "asserts form doesn't accept used profile names" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            get ProfileR

            request $ do
                setMethod "POST"
                setUrl ProfileR
                addToken
                byLabelContain "Profile Name" "foobar"

            statusIs 303
            _  <- followRedirect

            anotherUserEntity <- createUser "foo"
            authenticateAs anotherUserEntity
            get ProfileR

            request $ do
                setMethod "POST"
                setUrl ProfileR
                addToken
                byLabelContain "Profile Name" "foobar"

            statusIs 200
            htmlAnyContain ".error-block" "already used"

        it "asserts form only accept alphanumeric characters" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity
            get ProfileR

            request $ do
                setMethod "POST"
                setUrl ProfileR
                addToken
                byLabelContain "Profile Name" "my site"

            statusIs 200
            htmlAnyContain ".error-block" "must be alphanumeric"


