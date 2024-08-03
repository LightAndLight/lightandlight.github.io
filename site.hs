{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative (empty)
import Control.Monad ((<=<))
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Maybe (mapMaybe)
import Data.Monoid (First (..))
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- use the "With" variants of these functions instead

import Data.Text (Text)
import Hakyll hiding (pandocCompiler, readPandoc, writePandoc)
import Network.Wai.Application.Static (StaticSettings (..))
import System.Directory (doesFileExist)
import System.FilePath (dropExtension, takeBaseName, takeExtension, (<.>), (</>))
import Text.Pandoc (Block, Inline (..), Pandoc (..), nullAttr, readHtml, runPure)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Walk (Walkable, query, walk)
import Text.Pandoc.Writers (writePlain)
import WaiAppStatic.Types (File (..), fromPiece, unsafeToPiece)

root :: String
root = "https://blog.ielliott.io"

-- | Serves the contents of `/path/to/x.html` when `/path/to/x` is requested.
withPrettyUrls :: Configuration -> Configuration
withPrettyUrls config =
  config
    { previewSettings = \path ->
        let settings = config.previewSettings path
         in settings
              { ssLookupFile =
                  \pieces ->
                    case splitAt (length pieces - 1) pieces of
                      (prefix, [piece])
                        | let fileName = fromPiece piece
                        , takeExtension (Text.unpack fileName) == "" ->
                            settings.ssLookupFile $ prefix <> [unsafeToPiece $ fileName <> ".html"]
                      _ -> settings.ssLookupFile pieces
              , ssGetMimeType =
                  \file ->
                    if takeExtension (Text.unpack (fromPiece file.fileName)) == ""
                      then do
                        htmlExists <- doesFileExist $ path </> Text.unpack (fromPiece file.fileName) <.> "html"
                        if htmlExists
                          then pure "text/html"
                          else settings.ssGetMimeType file
                      else settings.ssGetMimeType file
              }
    }

data BreakOn a b
  = Found {prefix :: [a], target :: b, suffix :: [a]}
  | Missing [a]

breakOn :: (a -> Maybe b) -> [a] -> BreakOn a b
breakOn predicate items =
  case items of
    [] ->
      Missing []
    item : items' ->
      case predicate item of
        Nothing ->
          case breakOn predicate items' of
            Found{prefix, target, suffix} ->
              Found{prefix = item : prefix, target, suffix}
            Missing items'' ->
              Missing (item : items'')
        Just item' ->
          Found{prefix = [], target = item', suffix = items'}

separateBy :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
separateBy predicate items =
  case breakOn predicate items of
    Missing items' ->
      (items', [])
    Found{prefix, target, suffix} ->
      case separateBy predicate suffix of
        (prefix', suffix') ->
          (prefix, (target, prefix') : suffix')

main :: IO ()
main = do
  now <- getCurrentTime

  hakyllWith (withPrettyUrls defaultConfiguration) $ do
    match "templates/*" $ compile templateBodyCompiler

    match "css/*" $ do
      route $ gsubRoute "css/" (const "res/")
      compile compressCssCompiler

    match "js/*" $ do
      route $ gsubRoute "js/" (const "res/")
      compile copyFileCompiler

    match "fonts/*" $ do
      route $ gsubRoute "fonts/" (const "res/")
      compile copyFileCompiler

    match "talks/*" $ do
      route idRoute
      compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    match "posts/images/*" $ do
      route $ gsubRoute "posts/" (const "")
      compile copyFileCompiler

    match (fromList ["favicon.ico", "CNAME"]) $ do
      route idRoute
      compile copyFileCompiler

    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"

        metadata <-
          loadAndApplyTemplate
            "templates/index-metadata.html"
            ( constField "root" root
                <> constField "url" "/"
                <> constField "title" "blog.ielliott.io"
                <> constField "description" "Isaac Elliott's personal blog."
            )
            =<< makeItem ("" :: String)

        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" (postListCtx posts Nothing)
          >>= loadAndApplyTemplate
            "templates/page.html"
            (bodyField "body" <> constField "metadata" metadata.itemBody)
          >>= prettifyUrls

    match (fromList ["about.md", "resources.md", "cv.md", "talks.md", "404.md"]) $ do
      route $ setExtension "html"
      compile $ do
        metadata <-
          loadAndApplyTemplate
            "templates/page-metadata.html"
            (constField "root" root <> urlField "url" <> metadataField)
            =<< makeItem ("" :: String)

        pandocCompilerWith pandocReaderOptions pandocWriterOptions
          >>= loadAndApplyTemplate
            "templates/page.html"
            (pageCtx <> constField "metadata" metadata.itemBody)
          >>= prettifyUrls

    postInfos :: [(Identifier, PostInfo)] <- getPostInfos "posts/*"
    match "posts/*" $ do
      route $ metadataRoute fromPermalink
      compile $ do
        identifier <- getUnderlying

        postPandocItem <- do
          document <- readPandocWith pandocReaderOptions =<< getResourceBody
          pure $ linkHeaders . tableOfContents <$> document

        post <- saveSnapshot "content" $ writePandocWith pandocWriterOptions postPandocItem

        excerpt <- saveSnapshot "excerpt" =<< getExcerpt identifier postPandocItem
        _tags <- saveSnapshot "tags" =<< makeItem =<< getTags post.itemIdentifier

        let postCtx = mkPostCtx identifier postInfos

        metadata <-
          loadAndApplyTemplate
            "templates/post-metadata.html"
            ( dateField "date" "%0Y-%m-%dT%H:%M%Ez"
                <> constField "root" root
                <> constField "excerpt" excerpt.itemBody
                <> functionField
                  "plaintext"
                  ( \args item -> do
                      case args of
                        [arg] ->
                          case runPure $ writePlain pandocWriterOptions =<< readHtml pandocReaderOptions (Text.pack arg) of
                            Left err -> error $ "error in function \"plaintext\" in " <> toFilePath item.itemIdentifier <> ": " <> show err
                            Right string -> pure . Text.unpack $ Text.strip string
                        _ ->
                          error $
                            "incorrect number of arguments to function \"plaintext\" in "
                              <> toFilePath item.itemIdentifier
                  )
                <> postCtx
            )
            =<< makeItem ""

        loadAndApplyTemplate "templates/post.html" postCtx post
          >>= loadAndApplyTemplate "templates/page.html" (pageCtx <> constField "metadata" metadata.itemBody)
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

        metadata <-
          loadAndApplyTemplate
            "templates/page-metadata.html"
            ( constField "root" root
                <> urlField "url"
                <> constField "title" ("Posts about " <> tag)
                <> constField "description" ("Posts about " <> tag <> ".")
            )
            =<< makeItem ("" :: String)

        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" (postListCtx postsForTag $ Just tag)
          >>= loadAndApplyTemplate
            "templates/page.html"
            ( constField "title" ("Posts about " <> tag)
                <> bodyField "body"
                <> constField "metadata" metadata.itemBody
            )
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

linkHeaders :: Pandoc -> Pandoc
linkHeaders =
  walk @Block
    ( \block ->
        case block of
          Pandoc.Header level attr@(identifier, _classes, _attributes) content
            | level > 1
            , not (Text.null identifier) ->
                Pandoc.Header level attr [Link nullAttr content ("#" <> identifier, "")]
          _ -> block
    )

tableOfContents :: Pandoc -> Pandoc
tableOfContents document =
  walk @Block
    ( \block ->
        case block of
          Pandoc.Div attr@("toc", _classes, _attributes) _ ->
            Pandoc.Div
              attr
              -- This breaks if I use `Pandoc.Header 3 nullAttr [Str "Contents"]`
              -- I don't know why.
              [ Pandoc.RawBlock "html" "<h3>Contents</h3>"
              , contents
              ]
          _ -> block
    )
    document
 where
  contents =
    fromContents . toContents $
      query @Block
        ( \block -> case block of
            Pandoc.Header{} -> [block]
            _ -> mempty
        )
        document

data Contents = Header
  { level :: Int
  , id :: Text
  , classes :: [Text]
  , attrs :: [(Text, Text)]
  , content :: [Inline]
  , children :: [Contents]
  }
  deriving (Eq, Show)

toContents :: [Block] -> [Contents]
toContents = go 2
 where
  go :: Int -> [Block] -> [Contents]
  go level =
    fmap
      ( \((headerLevel, (identifier, classes, attrs), content), blocks) ->
          let omitChildren =
                case lookup "toc:omit_children" attrs of
                  Just value ->
                    case value of
                      "true" ->
                        True
                      "false" ->
                        False
                      _ ->
                        error $ "invalid contents:omit_children value: " <> Text.unpack value
                  Nothing ->
                    False
           in Header
                headerLevel
                identifier
                classes
                attrs
                content
                (if omitChildren then [] else go (level + 1) blocks)
      )
      . snd
      . separateBy
        ( \case
            Pandoc.Header headerLevel attrs content
              | level == headerLevel ->
                  Just (headerLevel, attrs, content)
            _ -> Nothing
        )

fromContents :: [Contents] -> Block
fromContents contents =
  Pandoc.BulletList $
    ( \content ->
        Pandoc.Plain [Link nullAttr content.content ("#" <> content.id, "")]
          : case content.children of
            [] -> []
            _ ->
              [fromContents content.children]
    )
      <$> contents

postInfoCtx :: Context String
postInfoCtx =
  dateField "date" "%e %B, %Y"
    <> prettyUrlField "url"
    <> listFieldWith @String
      "tags"
      (field "name" (pure . (.itemBody)))
      (\post -> traverse makeItem . (.itemBody) =<< loadSnapshot @[String] post.itemIdentifier "tags")
    <> field "has_tags" (\post -> do
         item <-loadSnapshot @[String] post.itemIdentifier "tags"
         if null item.itemBody
         then empty
         else pure ""
       )
    <> metadataField

mkPostCtx :: Identifier -> [(Identifier, PostInfo)] -> Context String
mkPostCtx identifier postInfos =
  bodyField "body"
    <> postInfoCtx
    <> maybe
      (boolField "has_previous" (const False) <> boolField "has_previous" (const False))
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
  metadataField <> bodyField "body"

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
        -- Syntax for adding attributes to Markdown elements.
        -- `tableOfContents` uses this to omit a heading's children from the contents listing.
        enableExtension Ext_header_attributes
          .
          -- Uses `Div` blocks for `<div>` tags so that I can post-process them.
          -- See `tableOfContents` for an example.
          enableExtension Ext_native_divs
          . enableExtension Ext_backtick_code_blocks
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
              (\block -> pure $ Pandoc Pandoc.nullMeta [removeFootnotes block])
              . getFirst
              . query
                ( \block -> case block of
                    Pandoc.Para{} -> First $ Just block
                    _ -> mempty
                )
          )
          postPandocItem
      pure $ writePandocWith pandocWriterOptions excerptPandocItem
  where
    removeFootnotes :: Walkable [Inline] b => b -> b
    removeFootnotes = walk @[Inline] (filter (\case; Note{} -> False; _ -> True))

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
