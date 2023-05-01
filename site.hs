{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Monoid (First (..))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Traversable (for)
import Hakyll
import Skylighting (SyntaxMap, loadSyntaxesFromDir)
import System.FilePath (dropExtension, takeBaseName, takeExtension, (<.>), (</>))
import Text.Pandoc (Block (..), Pandoc (..))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Options
import Text.Pandoc.Walk (query)

root :: String
root = "https://blog.ielliott.io"

postCtx :: Context String
postCtx =
  bodyField "body"
    <> dateField "date" "%e %B, %Y"
    <> prettyUrlField "url"
    <> listFieldWith @String
      "tags"
      (field "name" (pure . (.itemBody)))
      (\post -> traverse makeItem . (.itemBody) =<< loadSnapshot @[String] post.itemIdentifier "tags")
    <> metadataField

pageCtx :: Context String
pageCtx =
  metadataField
    <> bodyField "body"
    <> constField "seo" ""

postListCtx :: [Item String] -> Maybe String -> Context String
postListCtx posts mTag =
  listField
    "posts"
    ( postCtx
        <> field "excerpt" (\post -> (.itemBody) <$> loadSnapshot post.itemIdentifier "excerpt")
    )
    (return posts)
    <> foldMap (constField "tag") mTag

mathPandocCompiler :: Compiler (Item String)
mathPandocCompiler =
  pandocCompilerWith
    ( defaultHakyllReaderOptions
        { readerExtensions =
            getDefaultExtensions "commonmark_x"
        }
    )
    mathPandocWriterOptions

mathPandocWriterOptions :: WriterOptions
mathPandocWriterOptions =
  defaultHakyllWriterOptions
    { writerExtensions = enableExtension Ext_tex_math_dollars defaultHakyllWriterOptions.writerExtensions
    , writerHTMLMathMethod = MathML
    }

prettyUrl :: String -> String
prettyUrl url =
  if isLocal url && takeExtension url == ".html"
    then dropExtension url
    else url
 where
  isLocal url = root `isPrefixOf` url || not ("://" `isInfixOf` url)

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

    match (fromList ["favicon.ico", "CNAME", "talks/*", "files/*"]) $ do
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
        pandocCompiler
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= prettifyUrls

    postAdjacency :: [(Identifier, (Maybe Identifier, Maybe Identifier))] <-
      fmap
        ( \postIds ->
            zip
              postIds
              ( zip
                  (Nothing : fmap Just postIds)
                  (fmap Just (drop 1 postIds) <> [Nothing])
              )
        )
        . sortChronological
        =<< getMatches "posts/*"

    match "posts/*" $ do
      route $
        metadataRoute
          ( \metadata ->
              ( case lookupString "permalink" metadata of
                  Nothing ->
                    idRoute <> setExtension "html"
                  Just permalink ->
                    constRoute $
                      ( case permalink of
                          '/' : permalink' -> permalink'
                          _ -> permalink
                      )
                        <> ".html"
              )
          )
      compile $ do
        identifier <- getUnderlying

        postPandocItem <- readPandoc =<< getResourceBody
        post <-
          saveSnapshot "content" $
            writePandocWith mathPandocWriterOptions postPandocItem

        mMetadataExcerpt <- getMetadataField identifier "excerpt"
        _excerpt <-
          saveSnapshot "excerpt"
            =<< case mMetadataExcerpt of
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
                              Para content ->
                                First $ Just block
                              _ -> mempty
                          )
                    )
                    postPandocItem
                pure $ writePandocWith mathPandocWriterOptions excerptPandocItem
        _tags <- saveSnapshot "tags" =<< makeItem =<< getTags post.itemIdentifier

        loadAndApplyTemplate
          "templates/post.html"
          ( postCtx
              <> maybe
                ( boolField "has_previous" (const False)
                    <> boolField "has_previous" (const False)
                )
                ( \(mPrevious, mNext) ->
                    maybe
                      (boolField "has_previous" (const False))
                      ( \previousId ->
                          boolField "has_previous" (const True)
                            <> prettyUrlFromField "previous_url" previousId
                            <> field "previous_title" (\_ -> getMetadataField' previousId "title")
                      )
                      mPrevious
                      <> maybe
                        (boolField "has_next" (const False))
                        ( \nextId ->
                            boolField "has_next" (const True)
                              <> prettyUrlFromField "next_url" nextId
                              <> field "next_title" (\_ -> getMetadataField' nextId "title")
                        )
                        mNext
                )
                (lookup identifier postAdjacency)
          )
          post
          >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= prettifyUrls

    tags :: [String] <- fmap concat . traverse getTags =<< getMatches "posts/*"
    create (fromFilePath . (\tag -> "tags" </> tag <.> "html") <$> nub tags) $ do
      route idRoute
      compile $ do
        tag <- takeBaseName . toFilePath <$> getUnderlying
        postsForTag <- do
          posts <- loadAll "posts/*"
          postsWithTags <- for posts $ \post -> do
            tags <- getTags post.itemIdentifier
            pure (post, tags)
          recentFirst . map fst $ filter (\(_, tags) -> tag `elem` tags) postsWithTags

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
            sitemapCtx =
              constField "root" root
                <> listField
                  "pages"
                  ( dateField "date" "%0Y-%m-%dT%H:%M%Ez"
                      <> prettyUrlField "url"
                      <> constField "root" root
                  )
                  (return pages)

        makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
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

        let
          feedCtx =
            metadataField
              <> prettyUrlField "url"
              <> field "description" (\item -> (.itemBody) <$> loadSnapshot @String item.itemIdentifier "excerpt")
              <> constField "updated" (formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M%Ez" now)
              <> bodyField "body"

        atomTemplate <- loadBody "templates/atom.xml"
        atomItemTemplate <- loadBody "templates/atom-item.xml"
        renderAtomWithTemplates atomTemplate atomItemTemplate feedConfiguration feedCtx posts