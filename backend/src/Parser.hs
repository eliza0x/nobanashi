{-# LANGUAGE OverloadedStrings #-}

module Parser(parse) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.MMark        as MMark
import qualified Text.Megaparsec   as M
import qualified Lucid

parse :: Text -> Either Text Text
parse txt = case MMark.parse "" txt of
    Left bundle -> Left . T.pack $ M.errorBundlePretty bundle
    Right r -> Right . TL.toStrict . Lucid.renderText $ MMark.render r
