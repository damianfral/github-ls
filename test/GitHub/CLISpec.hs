{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GitHub.CLISpec (spec) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Function
import Data.GenValidity
import Data.GenValidity.Text
import Data.Text (toLower)
import qualified Data.Text as T
import Data.Validity
import GitHub.CLI
import Options.Generic
import Relude hiding (sort)
import Test.QuickCheck (suchThat)
import Test.Syd
import Test.Syd.Validity

instance Eq Options where
  (Options a1 a2 a3 a4 a5 a6) == (Options b1 b2 b3 b4 b5 b6) =
    (==)
      ( unHelpful a1,
        unHelpful a2,
        unHelpful a3,
        unHelpful a4,
        unHelpful a5,
        unHelpful a6
      )
      ( unHelpful b1,
        unHelpful b2,
        unHelpful b3,
        unHelpful b4,
        unHelpful b5,
        unHelpful b6
      )

instance Validity Access

instance GenValid Access

instance Validity Language

instance GenValid Language

instance Validity Display

instance GenValid Display

instance Validity Organization

instance GenValid Organization

instance Validity Sorting

instance GenValid Sorting

spec :: Spec
spec =
  it "decodes valid options" $ do
    forAllValid $ \(org, lang) ->
      forAllValid $ \(access, display, archived, sort) -> do
        let options =
              Options
                { org = Helpful $ Just org,
                  access = Helpful $ Just access,
                  lang = Helpful $ Just lang,
                  display = Helpful $ Just display,
                  archived = Helpful archived,
                  sort = Helpful $ Just sort
                }
        let record =
              [ "--display",
                toLower $ show display,
                "--lang",
                unLanguage lang,
                "--org",
                unOrganization org,
                "--access",
                toLower (show access),
                "--sort",
                toLower $ T.drop 2 (show sort) -- drop "by'
              ]
                <> ["--archived" | archived]
        --
        getRecordPure record `shouldBe` Just options
