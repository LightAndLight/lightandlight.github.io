---
title: Local Hoogle
permalink: /local-hoogle
date: 2026-03-30T00:30:00Z
tags:
  - programming
  - haskell
---

<div id="toc"><!-- generated --></div>

[Hoogle](https://github.com/ndmitchell/hoogle) didn't work when I tried to use it for local documentation search,
so I fixed it.

## Hoogle patches

See also: my fork [here](https://github.com/LightAndLight/hoogle).

### Proof of concept: read `haddock-html` field from `ghc-pkg` entries

When I tried to generate a Hoogle database (on NixOS) I got this error:

```
$ cabal exec -- hoogle generate --local --database db.hoo
Starting generate
Reading ghc-pkg... 0.04s

Packages missing documentation: array base binary bytestring Cabal Cabal-syntax containers deepseq directory exceptions filepath ghc ghc-bignum ghc-boot ghc-boot-th ghc-compact ghc-experimental ghc-heap ghc-internal ghc-platform ghc-prim ghc-toolchain ghci haskeline hpc integer-gmp mtl os-string parsec pretty process rts semaphore-compat stm system-cxx-std-lib template-haskell terminfo text time transformers unix xhtml
No packages were found, aborting (use no arguments to index all of Stackage)
```

Hoogle was using hard-coded package documentation locations that weren't present on my system.
[`ghc-pkg`](https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html#package-management-the-ghc-pkg-command)
package descriptions have a `haddock-html` field, so Hoogle should just use that.

<details open>
<summary>Hoogle patch 1/2</summary>

```diff
diff --git a/src/Input/Cabal.hs b/src/Input/Cabal.hs
index 6180fe06..f31292ec 100644
--- a/src/Input/Cabal.hs
+++ b/src/Input/Cabal.hs
@@ -117,8 +117,32 @@ readGhcPkg settings = do
     let fixer p = p{packageLibrary = True, packageDocs = g <$> packageDocs p}
     let f ((stripPrefix "name: " -> Just x):xs) = Just (mkPackageName $ trimStart x, fixer $ readCabal settings $ bstrPack $ unlines xs)
         f _ = Nothing
-    pure $ Map.fromList $ mapMaybe f $ splitOn ["---"] $ lines $ filter (/= '\r') $ UTF8.toString stdout
-
+    let
+        withHaddockHtml (name, package) =
+            case packageDocs package of
+                Nothing -> do
+                    packageDocs' <- readGhcPkgHaddockHtml (unPackageName name)
+                    pure (name, package{packageDocs = packageDocs'})
+                Just{} ->
+                    pure (name, package)
+    fmap Map.fromList $ traverse withHaddockHtml $ mapMaybe f $ splitOn ["---"] $ lines $ filter (/= '\r') $ UTF8.toString stdout
+
+readGhcPkgHaddockHtml ::
+    -- | Package name
+    String ->
+    IO (Maybe FilePath)
+readGhcPkgHaddockHtml packageName = do
+    let args = ["field", packageName, "haddock-html", "--expand-pkgroot", "--simple"]
+    (exit, stdout, stderr) <- BS.readProcessWithExitCode "ghc-pkg" args mempty
+    if exit /= ExitSuccess
+        then do
+            putStrLn "error when calling ghc-pkg:"
+            putStrLn $ "args: " ++ show args
+            putStrLn $ "exit: " ++ show exit
+            putStrLn $ "stderr: " ++ UTF8.toString stderr
+            pure Nothing
+        else
+            pure . Just . trimEnd $ UTF8.toString stdout

 -- | Given a tarball of Cabal files, parse the latest version of each package.
 parseCabalTarball :: Settings -> FilePath -> IO (Map.Map PkgName Package)
```

</details>

### Proof of concept: make `generate --local` respect `GHC_ENVIRONMENT` variable

Now Hoogle doesn't crash on my system:

```
$ cabal exec -- hoogle generate --local --database db.hoo
Starting generate
Reading ghc-pkg... 1.04s
[6/42] bytestring... 0.06s
[12/42] ghc... 2.10s
[19/42] ghc-prim... 0.26s
[37/42] xhtml... 0.02s
Packages missing documentation: ghc-compact ghc-experimental integer-gmp rts system-cxx-std-lib
Found 30 warnings when processing items

Reordering items... 0.03s
Writing tags... 0.10s
Writing names... 0.10s
Writing types... 0.37s
Took 6.76s
```

But it only includes GHC's boot libraries in the generated index; my project's dependencies are missing.
I made Hoogle use the
[`GHC_ENVIRONMENT`](https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html#envvar-GHC_ENVIRONMENT)
variable to decide which packages should be included in the generated database
(this variable is provided by `cabal exec`).
I also needed this part of Hoogle to use package IDs instead of package names.

This code does the job, but it feels hacky.
I wouldn't want it to be merged into Hoogle as-is.

<details open>
<summary>Hoogle patch 2/2</summary>

```diff
diff --git a/src/Input/Cabal.hs b/src/Input/Cabal.hs
index f31292ec..31aee29d 100644
--- a/src/Input/Cabal.hs
+++ b/src/Input/Cabal.hs
@@ -21,6 +21,7 @@ import System.Exit
 import qualified System.Process.ByteString as BS
 import qualified Data.ByteString.UTF8 as UTF8
 import System.Directory
+import System.Environment
 import Data.Maybe
 import Data.Tuple.Extra
 import qualified Data.Map.Strict as Map
@@ -86,9 +87,29 @@ packagePopularity cbl = mp `seq` (errs, mp)
 ---------------------------------------------------------------------
 -- READERS

+-- | Returns a list of extra ghc-pkg flags, and an optional list of enabled packages.
+readGhcEnvironment :: IO ([String], Maybe [String])
+readGhcEnvironment = do
+    mFile <- lookupEnv "GHC_ENVIRONMENT"
+    case mFile of
+        Nothing -> pure ([], Nothing)
+        Just file -> do
+            contents <- readFile file
+            let
+                f line rest@(flags, enabledPackages)
+                    | line == "clear-package-db" = ("--no-user-package-db" : flags, enabledPackages)
+                    | line == "global-package-db" = ("--global" : flags, enabledPackages)
+                    | Just packageDb <- stripPrefix "package-db " line = (("--package-db=" ++ packageDb) : flags, enabledPackages)
+                    | Just name <- stripPrefix "package-id " line = (flags, name : enabledPackages)
+                    | otherwise = rest
+
+            let (flags, enabledPackages) = foldr f ([], []) (lines contents)
+            pure (flags, Just enabledPackages)
+
 -- | Run 'ghc-pkg' and get a list of packages which are installed.
 readGhcPkg :: Settings -> IO (Map.Map PkgName Package)
 readGhcPkg settings = do
+    (ghcPkgFlags, mEnabledPackages) <- readGhcEnvironment
     topdir <- findExecutable "ghc-pkg"
     (exit, stdout, stderr) <-
     -- From GHC 9.0.1, the `haddock-html` field in `*.conf` files for GHC boot
@@ -108,31 +129,57 @@ readGhcPkg settings = do
     -- correct manually the affected `*.conf` files.

     -- important to use BS process reading so it's in Binary format, see #194
-      BS.readProcessWithExitCode "ghc-pkg" ["dump", "--expand-pkgroot"] mempty
+      BS.readProcessWithExitCode "ghc-pkg" (["dump", "--expand-pkgroot"] ++ ghcPkgFlags) mempty
     when (exit /= ExitSuccess) $
         errorIO $ "Error when reading from ghc-pkg, " ++ show exit ++ "\n" ++ UTF8.toString stderr
     let g (stripPrefix "$topdir" -> Just x) | Just t <- topdir = takeDirectory t ++ x
         -- ^ Backwards compatibility with GHC < 9.0
         g x = x
     let fixer p = p{packageLibrary = True, packageDocs = g <$> packageDocs p}
-    let f ((stripPrefix "name: " -> Just x):xs) = Just (mkPackageName $ trimStart x, fixer $ readCabal settings $ bstrPack $ unlines xs)
+
+    let
+        findPackageId [] = Nothing
+        findPackageId (x:xs)
+            | Just rest <- stripPrefix "id:" x =
+                if null rest
+                    then
+                        -- some package database entries have the `id` field's value
+                        -- on the next line.
+                        --
+                        -- should really use a proper parser.
+                        case xs of
+                            x':_xs' -> Just $ trimStart x'
+                            [] -> Nothing
+                    else Just $ trimStart rest
+            | otherwise = findPackageId xs
+
+    let
+        f ((stripPrefix "name: " -> Just x):xs)
+            | Just pId <- findPackageId xs
+            , maybe True (pId `elem`) mEnabledPackages =
+            Just (pId, mkPackageName $ trimStart x, fixer $ readCabal settings $ bstrPack $ unlines xs)
         f _ = Nothing
+
     let
-        withHaddockHtml (name, package) =
+        withHaddockHtml (pId, name, package) =
             case packageDocs package of
                 Nothing -> do
-                    packageDocs' <- readGhcPkgHaddockHtml (unPackageName name)
-                    pure (name, package{packageDocs = packageDocs'})
+                    packageDocs' <- readGhcPkgHaddockHtml ghcPkgFlags pId
+                    pure (pId, name, package{packageDocs = packageDocs'})
                 Just{} ->
-                    pure (name, package)
-    fmap Map.fromList $ traverse withHaddockHtml $ mapMaybe f $ splitOn ["---"] $ lines $ filter (/= '\r') $ UTF8.toString stdout
+                    pure (pId, name, package)
+    
+    let dump = splitOn ["---"] $ lines $ filter (/= '\r') $ UTF8.toString stdout
+    Map.fromList . fmap (\(_pId, name, pkg) -> (name, pkg)) <$> traverse withHaddockHtml (mapMaybe f dump)

 readGhcPkgHaddockHtml ::
-    -- | Package name
+    -- | Extra ghc-pkg flags
+    [String] ->
+    -- | Package ID
     String ->
     IO (Maybe FilePath)
-readGhcPkgHaddockHtml packageName = do
-    let args = ["field", packageName, "haddock-html", "--expand-pkgroot", "--simple"]
+readGhcPkgHaddockHtml flags pkgId = do
+    let args = ["field", pkgId, "haddock-html", "--unit-id", "--expand-pkgroot", "--simple"] ++ flags
     (exit, stdout, stderr) <- BS.readProcessWithExitCode "ghc-pkg" args mempty
     if exit /= ExitSuccess
         then do
```

</details>

## Cabal patch

Fixes a [Cabal bug](https://github.com/haskell/cabal/issues/11217) that caused Haddock to sometimes generate invalid URLs.
In Cabal's main branch as [this commit](https://github.com/haskell/cabal/commit/423ce6c8032cb66a78c8068533c295b99416841c).

<details open>
<summary>`Cabal/src/Distribution/Simple/Program/HcPkg.hs`</summary>

```diff
diff --git a/Cabal/src/Distribution/Simple/Program/HcPkg.hs b/Cabal/src/Distribution/Simple/Program/HcPkg.hs
index a494bc63f..7ad7eba4e 100644
--- a/Cabal/src/Distribution/Simple/Program/HcPkg.hs
+++ b/Cabal/src/Distribution/Simple/Program/HcPkg.hs
@@ -351,7 +351,7 @@ mungePackagePaths pkgroot pkginfo =
     , libraryDynDirs = mungePaths (libraryDynDirs pkginfo)
     , frameworkDirs = mungePaths (frameworkDirs pkginfo)
     , haddockInterfaces = mungePaths (haddockInterfaces pkginfo)
-    , haddockHTMLs = mungeUrls (haddockHTMLs pkginfo)
+    , haddockHTMLs = mungePaths (mungeUrls (haddockHTMLs pkginfo))
     }
   where
     mungePaths = map mungePath
```

</details>

## Usage

With the patched versions of Cabal and Hoogle, I can build a local Hoogle database for a Haskell project and its dependencies.

To my `cabal.project` file, I add:

```
documentation: True
haddock-hoogle: True
haddock-html: True

package *
  documentation: True
  haddock-hoogle: True
  haddock-html: True
```

The first block configures docs / Hoogle for the project's packages, and the second block is for the project's dependencies.
If you don't care about searching your project's code then omit the first block.

Then I run:

```
$ cabal build all
$ cabal exec -- hoogle generate --local --database db.hoo
$ hoogle serve --local --database db.hoo
```
