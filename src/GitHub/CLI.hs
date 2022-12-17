{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GitHub.CLI where

import Data.Text (pack, replace, toLower)
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GHC.Read (Read (readPrec), lexP)
import qualified GitHub as G
import Options.Applicative.Types
import Options.Generic
import Relude
import qualified Turtle

--------------------------------------------------------------------------------

data Access = Public | Private deriving (Show, Read, Typeable, Generic)

instance ParseField Access where
  readField = do
    str <- readerAsk
    case toLower $ pack str of
      "public" -> pure Public
      "private" -> pure Private
      _ -> fail "could not parse Access"

instance ParseRecord Access

--------------------------------------------------------------------------------

data Display = Name | URL deriving (Show, Read, Typeable, Generic)

instance ParseField Display where
  readField = do
    str <- readerAsk
    case toLower $ pack str of
      "name" -> pure Name
      "url" -> pure URL
      _ -> fail "could not parse Display"

instance ParseRecord Display

--------------------------------------------------------------------------------

data Options = Options
  { org :: Maybe Text,
    access :: Maybe Access,
    display :: Maybe Display,
    lang :: Maybe Text
  }
  deriving (Show, Read, Typeable, Generic)

instance ParseRecord Options

--------------------------------------------------------------------------------

log = T.hPutStrLn stderr

--------------------------------------------------------------------------------

getAuthToken :: IO (Maybe G.Auth)
getAuthToken = do
  (exitCode, output) <- Turtle.procStrict "gh" ["auth", "token"] Turtle.empty
  case exitCode of
    Turtle.ExitFailure _ -> log output $> Nothing
    Turtle.ExitSuccess -> do
      let token = replace "\n" "" output
      pure $ Just $ G.OAuth $ encodeUtf8 token

listAllOrganizations auth = G.github auth $ G.organizationsR G.FetchAll

listAllRepos auth =
  G.github auth $
    G.currentUserReposR G.RepoPublicityAll G.FetchAll

--------------------------------------------------------------------------------

displayRepo :: Display -> G.Repo -> Text
displayRepo URL = unURL . G.repoHtmlUrl
  where
    unURL (G.URL u) = u
displayRepo _ =
  mconcat
    [ G.untagName . G.simpleOwnerLogin . G.repoOwner,
      const "/",
      G.untagName . G.repoName
    ]

--------------------------------------------------------------------------------

runOptions options auth = do
  listAllRepos auth >>= \case
    Left e -> log $ show e
    Right repos -> do
      let filteredRepos = V.filter (getAll . filters) repos
      let render = displayRepo (fromMaybe Name $ display options)
      mapM_ (T.putStrLn . render) filteredRepos
  where
    filters =
      mconcat $
        catMaybes
          [ byOrg <$> org options,
            byAccess <$> access options,
            byLang <$> lang options
          ]

byOrg :: Text -> G.Repo -> All
byOrg o repo = All $ ((==) `on` toLower) repoOwnerText o
  where
    repoOwnerText = G.untagName $ G.simpleOwnerLogin $ G.repoOwner repo

byAccess :: Access -> G.Repo -> All
byAccess Private repo = All $ G.repoPrivate repo
byAccess Public repo = All $ not $ G.repoPrivate repo

byLang :: Text -> G.Repo -> All
byLang languageText repo = All $ Just fLanguage == rLanguage
  where
    fLanguage = toLower languageText
    rLanguage = toLower . G.getLanguage <$> G.repoLanguage repo

runCLI = do
  options <- getRecord "github-ls - List and filter your github repositories"
  getAuthToken >>= \case
    Nothing -> log "No auth"
    Just auth -> runOptions options auth
