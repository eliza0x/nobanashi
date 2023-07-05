{-# LANGUAGE OverloadedStrings #-}

module Parser(parse) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Debug.Trace as D 

import qualified CMark
import qualified Skylighting as S
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html.Renderer.Text as B

highlightNode :: CMark.Node -> Either Text CMark.Node 
highlightNode (CMark.Node pos type' nodes) = (CMark.Node pos <$> highlightNode' type') <*> mapM highlightNode nodes
    where 
    highlightNode' :: CMark.NodeType -> Either Text CMark.NodeType
    highlightNode' (CMark.CODE_BLOCK lang code) = case highlight lang code of
        Left e -> Left e
        Right html -> Right . CMark.HTML_BLOCK . TL.toStrict . B.renderHtml $ html
    highlightNode' other = Right other

    highlight :: Text -> Text -> Either Text B.Html
    highlight lang code = case syntax lang of
        Nothing -> Left $ "failed to find syntax \"" <> lang <> "\""
        Just s -> case S.tokenize opts s code of 
            Right sourceLine -> Right . (S.formatHtmlBlock S.defaultFormatOpts) $ sourceLine
            Left err -> Left . T.pack $ err
        where
        opts = S.TokenizerConfig { S.syntaxMap = S.defaultSyntaxMap, S.traceOutput = False }
        syntax lang = lang `S.lookupSyntax` S.defaultSyntaxMap


parse :: Text -> Either Text Text
parse txt = let
    node = highlightNode $ CMark.commonmarkToNode [] txt
    html = CMark.nodeToHtml [CMark.optUnsafe] <$> node
    in html
