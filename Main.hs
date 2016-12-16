module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Crypto.Cipher.RC4 (initialize, combine)

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (decode, encode)
import Data.Char (ord, chr)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust, isNothing)
import Control.Monad (when)

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

keys :: [BS.ByteString]
keys = [ pack $ map chr [i, j, k] | i <- [0 .. 255], j <- [0 .. 255], k <- [0 .. 255] ]

-- Printable ASCII: 20 to 7E / 00 to 7F (32 to 126 / 0 to 127)
isPrintable :: Char -> Bool
isPrintable c = ord c >= 32 && ord c <= 126

asciiOrGTFO :: String -> Maybe String
asciiOrGTFO chars = if isPrintable `all` chars then Just chars else Nothing

encrypt :: BS.ByteString -> BS.ByteString -> String
encrypt text key = unpack . snd $ combine (initialize key) text

contains :: Maybe String -> String -> String -> Bool
contains haystack needle location
    | isNothing haystack   = False
    | location == "prefix" = needle `isPrefixOf` fromJust haystack
    | location == "suffix" = needle `isSuffixOf` fromJust haystack
    | otherwise            = needle `isInfixOf`  fromJust haystack

--
--
--
--

searchKeys :: Parameters -> IO ()
searchKeys (Parameters ctext kn pos _) = mapM_ (printIfMatches ctext kn pos) keys

printIfMatches :: String -> String -> String -> BS.ByteString -> IO ()
printIfMatches ct pt pos key = let { decrypted = (asciiOrGTFO . encrypt (fst . decode $ pack ct)) key } in
                        when (contains decrypted pt pos)
                             (putStrLn $ (fromJust decrypted) ++ " (key: " ++ (unpack . encode $ key) ++ ")")

-- Real example:
-- known = "Key="
-- ciphertext = "C75AABD33B29FD21BB84C35E"
-- keys = [ pack $ map chr [i, j, k, l] | i <- [0 .. 255], j <- [0 .. 255], k <- [0 .. 255], l <- [0 .. 255] ]
--

-- Easy example:
-- (key: KEY [75, 69, 89]) + (plain: string) = e6148e3dd8e8
--known' = "st"
--position' = "prefix"
--ciphertext' = "E6148E3DD8E8"
--

-- TODO: docs
-- TODO: fix the MISSING message
-- TODO: build with Stack
-- TODO: keylen

main :: IO ()
main = execParser opts >>= searchKeys
    where
        opts = info (helper <*> parameters)
            ( fullDesc
           <> progDesc "Perform exhaustive search."
           <> header "rc4brute - ARC4 exhaustive search tool" )
