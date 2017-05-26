{-# LANGUAGE OverloadedStrings #-}

module Net.FixerIO(rates) where

import           Data.Aeson            (Value)
import qualified Data.ByteString as B
import           Network.HTTP.Simple


req :: B.ByteString -> [B.ByteString] -> Request
req base symbols =
    setRequestMethod "GET" .
    setRequestHost "api.fixer.io" .
    setRequestPath "latest" .
    setRequestQueryString [("base", base'), ("symbols", symbols')] $ defaultRequest
  where
    base' = Just base
    symbols' = Just (B.intercalate "," symbols)


rates :: B.ByteString -> [B.ByteString] -> IO Value
rates base symbols = do
  response <- httpJSON (req base symbols)
  return $ getResponseBody response
