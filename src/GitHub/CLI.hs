{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
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

data Access = Public | Private deriving (Show, Read, Typeable, Generic, Eq)

instance ParseField Access where
  readField = do
    str <- readerAsk
    case toLower $ pack str of
      "public" -> pure Public
      "private" -> pure Private
      _ -> fail "could not parse Access"

instance ParseRecord Access

--------------------------------------------------------------------------------

data Display
  = Name
  | URL
  | SSH
  | Git
  deriving (Show, Read, Typeable, Generic, Eq)

instance ParseField Display where
  readField = do
    str <- readerAsk
    case toLower $ pack str of
      "name" -> pure Name
      "url" -> pure URL
      "ssh" -> pure SSH
      "git" -> pure Git
      _ -> fail "could not parse Display"

instance ParseRecord Display

--------------------------------------------------------------------------------

newtype Organization = Organization Text
  deriving (Show, Read, Typeable, Generic, Eq)

unOrganization (Organization o) = o

instance ParseField Organization where
  metavar _ = "ORGANIZATION"
  parseField a b c d = Organization <$> parseField a b c d

--------------------------------------------------------------------------------

newtype Language = Language Text
  deriving (Show, Read, Typeable, Generic, Eq)

unLanguage (Language l) = l

instance ParseField Language where
  metavar _ = "LANGUAGE"
  parseField a b c d = Language <$> parseField a b c d

--------------------------------------------------------------------------------

data Options = Options
  { org ::
      Maybe Organization <?> "Print only repos owned by this organization",
    access ::
      Maybe Access <?> "Print only repos with this access (public|private)",
    display ::
      Maybe Display <?> "Print field (name|url|ssh|git)",
    lang ::
      Maybe Language <?> "Print only repos matching this language",
    archived ::
      Bool <?> "Print also archived repos"
  }
  deriving (Show, Typeable, Generic)

instance ParseRecord Options

--------------------------------------------------------------------------------

log = T.hPutStrLn stderr

--------------------------------------------------------------------------------

login :: IO ()
login = void $ Turtle.procStrict "gh" ["auth", "login"] Turtle.empty

getAuthToken :: IO (Maybe G.Auth)
getAuthToken = do
  (exitCode, output) <- Turtle.procStrict "gh" ["auth", "token"] Turtle.empty
  case exitCode of
    Turtle.ExitFailure _ -> pure Nothing
    Turtle.ExitSuccess -> do
      let token = replace "\n" "" output
      pure $ Just $ G.OAuth $ encodeUtf8 token

listAllOrganizations auth = G.github auth $ G.organizationsR G.FetchAll

listAllRepos auth =
  G.github auth $
    G.currentUserReposR G.RepoPublicityAll G.FetchAll

--------------------------------------------------------------------------------

unURL (G.URL u) = u

displayRepo :: Display -> G.Repo -> Text
displayRepo URL = unURL . G.repoHtmlUrl
displayRepo Git = maybe "" unURL . G.repoGitUrl
displayRepo SSH = maybe "" unURL . G.repoSshUrl
displayRepo _ =
  mconcat
    [ G.untagName . G.simpleOwnerLogin . G.repoOwner,
      const "/",
      G.untagName . G.repoName
    ]

--------------------------------------------------------------------------------

runOptions :: Options -> G.Auth -> IO ()
runOptions options auth = do
  listAllRepos auth >>= \case
    Left e -> log $ show e
    Right repos -> do
      let filteredRepos = V.filter (getAll . filters) repos
      let render = displayRepo $ fromMaybe Name displayValue
      mapM_ (T.putStrLn . render) filteredRepos
  where
    filters =
      mconcat $
        catMaybes
          [ byOrg <$> orgValue,
            byAccess <$> accessValue,
            byLang <$> langValue
          ]
          <> [excludeArchived | not archivedValue]
    orgValue = unHelpful $ org options
    displayValue = unHelpful $ display options
    accessValue = unHelpful $ access options
    langValue = unHelpful $ lang options
    archivedValue = unHelpful $ archived options

byOrg :: Organization -> G.Repo -> All
byOrg (Organization o) repo = All $ ((==) `on` toLower) repoOwnerText o
  where
    repoOwnerText = G.untagName $ G.simpleOwnerLogin $ G.repoOwner repo

byAccess :: Access -> G.Repo -> All
byAccess Private repo = All $ G.repoPrivate repo
byAccess Public repo = All $ not $ G.repoPrivate repo

byLang :: Language -> G.Repo -> All
byLang (Language languageText) repo = All $ Just fLanguage == rLanguage
  where
    fLanguage = toLower languageText
    rLanguage = toLower . G.getLanguage <$> G.repoLanguage repo

excludeArchived :: G.Repo -> All
excludeArchived = All . not . G.repoArchived

runCLI = do
  options <- getRecord "github-ls - List your github repositories"
  token <- runMaybeT $ MaybeT getAuthToken <|> MaybeT (login >> getAuthToken)
  case token of
    Nothing -> log "No auth"
    Just auth -> runOptions options auth
