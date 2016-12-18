module Lib where

import           Control.Monad          (replicateM, when)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Char              (chr, ord)
import           Data.List              (isInfixOf, isPrefixOf, isSuffixOf)
import           Data.Maybe             (fromJust, isNothing)

import           Crypto.Cipher.RC4      (State, combine, initialize)

-- Printable ASCII: 20 to 7E / 00 to 7F (32 to 126 / 0 to 127)
isPrintable :: Char -> Bool
isPrintable c = ord c >= 32 && ord c <= 126

asciiOrGTFO :: String -> Maybe String
asciiOrGTFO chars = if isPrintable `all` chars then Just chars else Nothing

encrypt :: BS.ByteString -> BS.ByteString -> String
encrypt text key = unpack . snd $ combine (initialize key) text

encrypt' :: BS.ByteString -> BS.ByteString -> Maybe String
encrypt' text key = applyContext (initialize key) text 0

applyContext :: State -> BS.ByteString -> Int -> Maybe String
applyContext state bytes pos
    | encPrintable = if pos+1 == BS.length bytes then Just . unpack $ newBytes else applyContext newState newBytes (pos+1)
    | otherwise    = Nothing
    where (newState, encrypted) = combine state (BS.take 1 (BS.drop pos bytes))
          encByte = BS.index encrypted 0
          encPrintable = encByte >= 32 && encByte <= 126
          newBytes = BS.concat [BS.take pos bytes, encrypted, BS.drop (pos+1) bytes]

contains :: String -> String -> Maybe String -> Bool
contains needle location maybeHaystack
    | isNothing maybeHaystack = False
    | location == "prefix"    = needle `isPrefixOf` haystack
    | location == "suffix"    = needle `isSuffixOf` haystack
    | otherwise               = needle `isInfixOf`  haystack
    where haystack = fromJust maybeHaystack

iterKeys :: Int -> [String]
iterKeys len = replicateM (len `div` 8) (map chr [0..255])

printIfMatches :: BS.ByteString -> (Maybe String -> Bool) -> String -> IO ()
printIfMatches ct check keystr =
    let key = pack keystr
        decrypted = encrypt' ct key
        -- decrypted = (asciiOrGTFO . encrypt (fst . decode $ ct)) key
    in  when (check decrypted)
        (putStrLn $ fromJust decrypted ++ " (key: " ++ (unpack . encode $ key) ++ ")")

-- Easy example:
-- "st" "prefix" "E6148E3DD8E8" 24
-- Real example:
-- "Key=" "prefix" "C75AABD33B29FD21BB84C35E" 32

-- TODO: docs
-- TODO: fix the MISSING message (better CLI)
-- TODO: perf / refactor
