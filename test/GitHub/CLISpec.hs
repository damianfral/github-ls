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
import Relude
import Test.QuickCheck (suchThat)
import Test.Syd
import Test.Syd.Validity

instance Eq Options where
  (Options a b c d) == (Options w x y z) =
    (unHelpful a, unHelpful b, unHelpful c, unHelpful d)
      == (unHelpful w, unHelpful x, unHelpful y, unHelpful z)

instance Validity Access

instance GenValid Access

instance Validity Language

instance Validity Display

instance GenValid Display

instance Validity Organization

instance GenValid Organization

instance GenValid Language

spec :: Spec
spec =
  it "decodes valid options" $ do
    forAllValid $ \(org, access, lang, display) -> do
      let options =
            Options
              { org = Helpful $ Just org,
                access = Helpful $ Just access,
                lang = Helpful $ Just lang,
                display = Helpful $ Just display
              }
      let record =
            [ "--display",
              toLower $ show display,
              "--lang",
              unLanguage lang,
              "--org",
              unOrganization org,
              "--access",
              toLower (show access)
            ]
      getRecordPure record `shouldBe` Just options
