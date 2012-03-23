{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , withYesodHeroku
    , getApplicationDev
    ) where

import Import
import Settings
import Settings.StaticFiles (staticSite)
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as M
import Web.Heroku

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Echo
import Handler.Mirror
import Handler.Blog

-- heroku hack
withYamlHerokuEnvironment :: Show e
                          => FilePath -- ^ the yaml file
                          -> e        -- ^ the environment you want to load
                          -> (Value -> Y.Parser a) -- ^ what to do with the mapping
                          -> IO a
withYamlHerokuEnvironment fp env f = do
    mval <- Y.decodeFile fp
    extra <- dbConnParams
    print mval
    case mval of
        Nothing -> fail $ "Invalid YAML file: " ++ show fp
        Just (Object obj)
            | Just v <- M.lookup (T.pack $ show env) obj
                -> case v of
                    (Object obj') -> let v' = Object (insertAll extra obj') in (print v' >> Y.parseMonad f v')
        _ -> fail $ "Could not find environment: " ++ show env
  where
    insertAll xs m = foldr insertPair m xs
    insertPair ("dbname", v) = M.insert "database" (String v)
    insertPair (k, v) = M.insert k (String v)

withYesodHeroku :: AppConfig DefaultEnv Extra -> Logger -> IO Application
withYesodHeroku conf logger = do
   manager <- newManager def
   s <- staticSite
   dbconf <- withYamlHerokuEnvironment "config/postgresql.yml" (appEnv conf)
             Database.Persist.Store.loadConfig
   p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
   Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
   let foundation = Cwl conf setLogger s p manager dbconf
   app <- toWaiAppPlain foundation
   return $ logWare app
 where
#ifdef DEVELOPMENT
   logWare = logCallbackDev (logBS setLogger)
   setLogger = logger
#else
   setLogger = toProduction logger -- by default the logger is set for development
   logWare = logCallback (logBS setLogger)
#endif


-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Cwl" resourcesCwl

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    let foundation = Cwl conf setLogger s p manager dbconf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader withYesodHeroku
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
