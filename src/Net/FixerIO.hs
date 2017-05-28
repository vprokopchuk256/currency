module Net.FixerIO(rates) where

import           Data.Scientific
import qualified Data.Text             as T
import           Data.Aeson
import qualified Data.ByteString       as B
import           Network.HTTP.Simple
import           Data.HashMap.Strict(toList, (!))


request :: B.ByteString -> [B.ByteString] -> Request
request base symbols =
    setRequestMethod "GET" .
    setRequestHost "api.fixer.io" .
    setRequestPath "latest" .
    setRequestQueryString [("base", base'), ("symbols", symbols')] $ defaultRequest
  where
    base' = Just base
    symbols' = Just (B.intercalate "," symbols)

extractRates :: Value -> [(T.Text, T.Text, Float)]
extractRates (Object v) =
    triples $ v ! "rates"
  where
    triples (Object o) = map triple $ toList o
    triple (k, n) = (base, k, toFloat n)
    base = toText $ v ! "base"
    toText (String t) = t
    toFloat (Number n) = toRealFloat n

rates :: B.ByteString -> [B.ByteString] -> IO [(T.Text, T.Text, Float)]
rates base symbols = do
  response <- httpJSON (request base symbols)
  return $ extractRates $ getResponseBody response
