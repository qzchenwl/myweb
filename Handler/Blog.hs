module Handler.Blog
    ( getBlogR
    , postBlogR
    , getArticleR
    )
where

import Import
import Yesod.Form.Nic (nicHtmlField)

entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq    textField "Title" Nothing
    <*> areq    nicHtmlField "Content" Nothing

-- The view showing the list of articles
getBlogR :: Handler RepHtml
getBlogR = do
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    ((_,articleWidget), enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "articles")

-- we continue Handler/Blog.hs
postBlogR :: Handler RepHtml
postBlogR = do
    ((res, articleWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
        _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")

getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
