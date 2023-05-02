{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad ((<=<))
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Maybe (mapMaybe)
import Data.Monoid (First (..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll hiding (pandocCompiler, readPandoc, writePandoc) -- use the "With" variants of these functions instead
import System.FilePath (dropExtension, takeBaseName, takeExtension, (<.>), (</>))
import Text.Pandoc (Block (..), Pandoc (..))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Walk (query)

root :: String
root = "https://blog.ielliott.io"

main :: IO ()
main = do
  now <- getCurrentTime

  hakyll $ do
    match "templates/*" $ compile templateBodyCompiler

    match "css/*" $ do
      route $ gsubRoute "css/" (const "res/")
      compile compressCssCompiler

    match "js/*" $ do
      route $ gsubRoute "js/" (const "res/")
      compile copyFileCompiler

    match "talks/*" $ do
      route idRoute
      compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    match (fromList ["favicon.ico", "CNAME"]) $ do
      route idRoute
      compile copyFileCompiler

    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" (postListCtx posts Nothing)
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= prettifyUrls

    match (fromList ["about.md", "resources.md", "404.md"]) $ do
      route $ setExtension "html"
      compile $ do
        pandocCompilerWith pandocReaderOptions pandocWriterOptions
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= prettifyUrls

    postInfos :: [(Identifier, PostInfo)] <- getPostInfos "posts/*"
    match "posts/*" $ do
      route $ metadataRoute fromPermalink
      compile $ do
        identifier <- getUnderlying

        postPandocItem <- readPandocWith pandocReaderOptions =<< getResourceBody
        post <- saveSnapshot "content" $ writePandocWith pandocWriterOptions postPandocItem

        _excerpt <- saveSnapshot "excerpt" =<< getExcerpt identifier postPandocItem
        _tags <- saveSnapshot "tags" =<< makeItem =<< getTags post.itemIdentifier

        loadAndApplyTemplate
          "templates/post.html"
          (postCtx identifier postInfos)
          post
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= prettifyUrls

    tags :: [String] <- getAllTags "posts/*"
    create (fmap (\tag -> fromFilePath $ "tags" </> tag <.> "html") tags) $ do
      route idRoute
      compile $ do
        tag <- takeBaseName . toFilePath <$> getUnderlying
        postsForTag <- do
          posts <- loadAll "posts/*"
          postsWithTags <- traverse (\post -> (,) post <$> getTags post.itemIdentifier) posts
          recentFirst $ mapMaybe (\(post, postTags) -> if tag `elem` postTags then Just post else Nothing) postsWithTags

        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" (postListCtx postsForTag $ Just tag)
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= prettifyUrls

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        singlePages <- loadAll $ fromList ["about.md", "resources.md", "404.md"]
        posts <- recentFirst =<< loadAll @String "posts/*"
        tagsPages <- loadAll "tags/*"
        let pages = singlePages <> posts <> tagsPages

        makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" (sitemapCtx pages)
          >>= prettifyUrls

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedConfiguration =
              FeedConfiguration
                { feedTitle = "blog.ielliott.io"
                , feedDescription = "Isaac Elliott's personal blog"
                , feedAuthorName = "Isaac Elliott"
                , feedAuthorEmail = "isaace71295@gmail.com"
                , feedRoot = root
                }

        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"

        atomTemplate <- loadBody "templates/atom.xml"
        atomItemTemplate <- loadBody "templates/atom-item.xml"
        renderAtomWithTemplates atomTemplate atomItemTemplate feedConfiguration (feedCtx now) posts

postInfoCtx :: Context String
postInfoCtx =
  dateField "date" "%e %B, %Y"
    <> prettyUrlField "url"
    <> listFieldWith @String
      "tags"
      (field "name" (pure . (.itemBody)))
      (\post -> traverse makeItem . (.itemBody) =<< loadSnapshot @[String] post.itemIdentifier "tags")
    <> metadataField

postCtx :: Identifier -> [(Identifier, PostInfo)] -> Context String
postCtx identifier postInfos =
  bodyField "body"
    <> postInfoCtx
    <> maybe
      ( boolField "has_previous" (const False)
          <> boolField "has_previous" (const False)
      )
      ( \postInfo ->
          maybe
            (boolField "has_previous" (const False))
            ( \previousId ->
                boolField "has_previous" (const True)
                  <> prettyUrlFromField "previous_url" previousId
                  <> field "previous_title" (\_ -> getMetadataField' previousId "title")
            )
            postInfo.previous
            <> maybe
              (boolField "has_next" (const False))
              ( \nextId ->
                  boolField "has_next" (const True)
                    <> prettyUrlFromField "next_url" nextId
                    <> field "next_title" (\_ -> getMetadataField' nextId "title")
              )
              postInfo.next
      )
      (lookup identifier postInfos)

pageCtx :: Context String
pageCtx =
  metadataField
    <> bodyField "body"
    <> constField "seo" ""

postListCtx :: [Item String] -> Maybe String -> Context String
postListCtx posts mTag =
  listField
    "posts"
    ( postInfoCtx
        <> field "excerpt" (\post -> (.itemBody) <$> loadSnapshot post.itemIdentifier "excerpt")
    )
    (return posts)
    <> foldMap (constField "tag") mTag

pandocReaderOptions :: ReaderOptions
pandocReaderOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        enableExtension Ext_backtick_code_blocks
          . enableExtension Ext_markdown_in_html_blocks
          . enableExtension Ext_auto_identifiers
          . enableExtension Ext_gfm_auto_identifiers
          $ getDefaultExtensions "commonmark_x"
    }

pandocWriterOptions :: WriterOptions
pandocWriterOptions =
  defaultHakyllWriterOptions
    { writerExtensions = enableExtension Ext_tex_math_dollars defaultHakyllWriterOptions.writerExtensions
    , writerHTMLMathMethod = MathML
    }

prettyUrl :: String -> String
prettyUrl url =
  if isLocal && takeExtension url == ".html"
    then dropExtension url
    else url
 where
  isLocal = root `isPrefixOf` url || not ("://" `isInfixOf` url)

prettyUrlField :: String -> Context a
prettyUrlField key =
  field key $ \item ->
    maybe
      (fail $ "no route url found for item " ++ show item.itemIdentifier)
      (toUrl . prettyUrl)
      <$> getRoute item.itemIdentifier

prettyUrlFromField :: String -> Identifier -> Context a
prettyUrlFromField key identifier =
  field key $ \item ->
    maybe
      (fail $ "no route url found for item " ++ show item.itemIdentifier)
      (toUrl . prettyUrl)
      <$> getRoute identifier

prettifyUrls :: Item String -> Compiler (Item String)
prettifyUrls =
  pure . fmap (withUrls prettyUrl)

fromPermalink :: Metadata -> Routes
fromPermalink metadata =
  case lookupString "permalink" metadata of
    Nothing ->
      idRoute <> setExtension "html"
    Just permalink ->
      constRoute $
        ( case permalink of
            '/' : permalink' -> permalink'
            _ -> permalink
        )
          <> ".html"

data PostInfo = PostInfo {previous :: Maybe Identifier, next :: Maybe Identifier}

getPostInfos :: Pattern -> Rules [(Identifier, PostInfo)]
getPostInfos postsPattern =
  fmap
    ( \postIds ->
        zipWith3
          (\postId previous next -> (postId, PostInfo{previous, next}))
          postIds
          (Nothing : fmap Just postIds)
          (fmap Just (drop 1 postIds) <> [Nothing])
    )
    . sortChronological
    =<< getMatches postsPattern

getExcerpt :: Identifier -> Item Pandoc -> Compiler (Item String)
getExcerpt identifier postPandocItem = do
  mMetadataExcerpt <- getMetadataField identifier "excerpt"
  case mMetadataExcerpt of
    Just excerpt ->
      makeItem excerpt
    Nothing -> do
      excerptPandocItem <-
        traverse
          ( maybe
              ( do
                  path <- getResourceFilePath
                  fail $ "failed to extract excerpt from " <> path
              )
              (\block -> pure $ Pandoc Pandoc.nullMeta [block])
              . getFirst
              . query
                ( \block -> case block of
                    Para{} -> First $ Just block
                    _ -> mempty
                )
          )
          postPandocItem
      pure $ writePandocWith pandocWriterOptions excerptPandocItem

getAllTags :: Pattern -> Rules [String]
getAllTags =
  fmap (nub . concat) . traverse getTags <=< getMatches

sitemapCtx :: [Item String] -> Context String
sitemapCtx pages =
  constField "root" root
    <> listField
      "pages"
      ( dateField "date" "%0Y-%m-%dT%H:%M%Ez"
          <> prettyUrlField "url"
          <> constField "root" root
      )
      (return pages)

feedCtx :: UTCTime -> Context String
feedCtx now =
  metadataField
    <> prettyUrlField "url"
    <> field "description" (\item -> (.itemBody) <$> loadSnapshot @String item.itemIdentifier "excerpt")
    <> constField "updated" (formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M%Ez" now)
    <> bodyField "body"
