module Handler.Mirror where

import Import
import qualified Data.Text as T

getMirrorR, postMirrorR :: Handler RepHtml
getMirrorR = do
    defaultLayout $ do
        $(widgetFile "mirror")

postMirrorR = do
    postedText <- runInputPost $ ireq textField "content"
    defaultLayout $ do
        $(widgetFile "posted")
