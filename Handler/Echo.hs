module Handler.Echo where

import Import

getEchoR :: Text -> Handler RepHtml
getEchoR text = do
    defaultLayout $ do
        [whamlet|<h1>#{text}|]

