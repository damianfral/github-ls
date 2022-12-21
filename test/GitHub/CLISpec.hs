{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GitHub.CLISpec (spec) where

import Data.GenValidity
import Data.GenValidity.Text
import qualified Data.Text as T
import Data.Validity
import GitHub.CLI
import Options.Generic
import Relude hiding (sort)
import Test.QuickCheck (suchThat)
import Test.Syd
import Test.Syd.Validity

instance Validity Access

instance GenValid Access

instance Validity Language

instance GenValid Language where
  genValid = Language <$> genValid `suchThat` (/=) mempty

instance Validity Display

instance GenValid Display

instance Validity Organization

instance GenValid Organization where
  genValid = Organization <$> genValid `suchThat` (/=) mempty

instance Validity Sorting

instance GenValid Sorting

instance Validity (Options Unwrapped)

instance GenValid (Options Unwrapped)

toArg name Nothing = []
toArg name (Just v) = ["--" <> name, v]

spec :: Spec
spec =
  it "decodes valid options" $ do
    forAllValid $ \options@Options {..} -> do
      let record =
            mconcat
              [ toArg "display" $ show <$> display,
                toArg "lang" $ unLanguage <$> lang,
                toArg "org" $ unOrganization <$> org,
                toArg "access" $ show <$> access,
                toArg "sort" $ T.drop 2 . show <$> sort -- drop "by'
              ]
              <> ["--archived" | archived]
      unwrapRecordPure record `shouldBe` Just options
