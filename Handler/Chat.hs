{-# LANGUAGE RecordWildCards #-}
module Handler.Chat
    ( getChatHomeR
    , getChatCheckR
    , getChatPostR
    )
where

import Import
import Control.Concurrent.STM
import Data.Map (fromList)

getChatHomeR :: Handler RepHtml
getChatHomeR = do
    Cwl {..} <- getYesod
    client <- liftIO . atomically $ do
        c <- readTVar nextClient
        writeTVar nextClient (c+1)
        cs <- readTVar chatClients
        chan <- case cs of
                     []      -> newTChan
                     (_,x):_ -> dupTChan x
        writeTVar chatClients ((c,chan) : cs)
        return c
    defaultLayout $ do
        setTitle "Chat Page"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
        $(widgetFile "chat")
        

getChatCheckR :: Handler RepJson
getChatCheckR = do
    liftIO $ putStrLn "Check"
    Cwl {..} <- getYesod
    client <- do
        c <- lookupGetParam "client"
        case c of
            Nothing -> invalidArgs ["No client value in Check request"]
            Just c' -> return $ read $ unpack c'
    cs <- liftIO . atomically $ readTVar chatClients
    chan <- case lookup client cs of
                Nothing -> invalidArgs ["Bad client value"]
                Just ch -> return ch
    -- block until there's something there
    first <- liftIO . atomically $ readTChan chan
    let ChatMsg s c = first
    jsonToRepJson $ fromList $ [("sender" :: Text, s), ("content", c)]

getChatPostR :: Handler RepJson
getChatPostR = do
    liftIO $ putStrLn "Post"
    Cwl {..} <- getYesod
    (sender, content) <- do
        s <- lookupGetParam "name"
        c <- lookupGetParam "send"
        case (s, c) of
             (Just s', Just c') -> return (s', c')
             _                  -> invalidArgs ["Either name or send not provided."]
    liftIO . atomically $ do
        cs <- readTVar chatClients
        let chan = snd . head $ cs -- doesn't matter which one we use, they're all duplicates
        writeTChan chan (ChatMsg sender content)
    jsonToRepJson ("success" :: Text)
