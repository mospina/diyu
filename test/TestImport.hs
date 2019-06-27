{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Progress

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: DB [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
-- I would like to use AuthEmail but I can create the salted password properly
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userEmail u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = runDB $ do
    user <- insertEntity User
        { userEmail = ident
        , userPassword = Nothing
        , userVerkey = Nothing
        , userVerified = True
        }
    return user

createProfile :: (Entity User) -> Text -> YesodExample App (Entity Profile)
createProfile userEntity profName = runDB $ do
    let (Entity userId _) = userEntity
    profile <- insertEntity Profile
        { profileName = profName
        , profileUserId = userId
        }
    return profile

createProgram :: (Entity Profile) -> Text -> Text -> YesodExample App (Entity Program)
createProgram profileEntity name slug = runDB $ do
    let (Entity profileId _) = profileEntity
    program <- insertEntity Program
        { programName = name
        , programSlug = slug
        , programDescription = Nothing
        , programProfileId = profileId
        }
    return program  

createCourse :: (Entity Program) -> Text -> Text -> Progress -> YesodExample App (Entity Course)
createCourse progEntity name slug progress = runDB $ do
    let (Entity progId program) = progEntity
    course <- insertEntity Course
        { courseName = name
        , courseSlug = slug
        , courseProgress = progress
        , courseDescription = Nothing
        , courseUrl = Nothing
        , courseProgramId = progId
        , courseProfileId = programProfileId program
        }
    return course  
