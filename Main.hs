module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Crypto.Cipher.RC4 (initialize, combine, State)

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (decode, encode)
import Data.Char (ord, chr)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust, isNothing)
import Control.Monad (when, replicateM)

data Parameters = Parameters
  { ciphertext :: String
  , known      :: String
  , position   :: String
  , keylen     :: Int }

parameters :: Parser Parameters
parameters = Parameters
    <$> argument str
        ( metavar "<ciphertext>" )
    <*> strOption
        ( long "known"
       <> metavar "<plaintext>"
       <> short 'k'
       <> help "Known plaintext.")
    <*> strOption
        ( long "position"
       <> metavar "<prefix|postfix|infix>"
       <> short 'p'
       <> help "Known plaintext position.")
    <*> option auto
        ( long "keylen"
       <> metavar "<length>"
       <> short 'l'
       <> help "Key length in bits.")

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

searchKeys :: Parameters -> IO ()
searchKeys (Parameters ctext kn pos bitlen) =
    let bsctext = fst . decode $ pack ctext
        ptcheck = contains kn pos
    in  mapM_ (printIfMatches bsctext ptcheck) (iterKeys bitlen)
-- in  putStrLn . unpack . encode . pack $ (encrypt (pack "string") (pack "KEY")) -- normal encryption
-- in  putStrLn $ (encrypt (fst . decode $ pack "e6148e3dd8e8") (pack "KEY")) -- decrypts to "string"
-- in  putStrLn . fromJust $ (encrypt2 (fst . decode $ pack "e6148e3dd8e8") (pack "KEY")) -- decrypts to "string"

printIfMatches :: BS.ByteString -> (Maybe String -> Bool) -> String -> IO ()
printIfMatches ct check keystr =
    let key = pack keystr
        decrypted = encrypt' ct key
        -- decrypted = (asciiOrGTFO . encrypt (fst . decode $ ct)) key
    in  when (check decrypted)
        (putStrLn $ (fromJust decrypted) ++ " (key: " ++ (unpack . encode $ key) ++ ")")

-- Easy example:
-- "st" "prefix" "E6148E3DD8E8" 24
-- Real example:
-- "Key=" "prefix" "C75AABD33B29FD21BB84C35E" 32

-- TODO: docs
-- TODO: fix the MISSING message (better CLI)
-- TODO: build with Stack

main :: IO ()
main = execParser opts >>= searchKeys
    where
        opts = info (helper <*> parameters)
            ( fullDesc
           <> progDesc "Perform exhaustive search."
           <> header "rc4brute - ARC4 exhaustive search tool" )
