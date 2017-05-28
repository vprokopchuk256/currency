module Net.FixerIO(rates) where

import           Data.Scientific       (toRealFloat)
import           Data.Text             (Text)
import           Data.Aeson            (Value(..))
import           Data.ByteString       (ByteString, intercalate)
import           Network.HTTP.Simple
import           Data.HashMap.Strict   (toList, (!))


request :: ByteString -> [ByteString] -> Request
request base symbols =
    setRequestMethod "GET" .
    setRequestHost "api.fixer.io" .
    setRequestPath "latest" .
    setRequestQueryString [("base", base'), ("symbols", symbols')] $ defaultRequest
  where
    base' = Just base
    symbols' = Just (intercalate "," symbols)

extractRates :: Value -> [(Text, Text, Float)]
extractRates (Object v) =
    triples $ v ! "rates"
  where
    triples (Object o) = map triple $ toList o
    triple (k, n) = (base, k, toFloat n)
    base = toText $ v ! "base"
    toText (String t) = t
    toFloat (Number n) = toRealFloat n

rates :: [ByteString] -> IO [(Text, Text, Float)]
rates symbols = concat <$> sequence [ratesFor base | base <- symbols]
  where
    ratesFor base = do
      response <- httpJSON (request base symbols)
      return $ extractRates $ getResponseBody response

