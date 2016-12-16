module Main where

import Lib

import Options.Applicative

import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Char8 (pack, unpack)

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

searchKeys :: Parameters -> IO ()
searchKeys (Parameters ctext kn pos bitlen) =
    let bsctext = fst . decode $ pack ctext
        ptcheck = contains kn pos
    in  mapM_ (printIfMatches bsctext ptcheck) (iterKeys bitlen)
-- in  putStrLn . unpack . encode . pack $ (encrypt (pack "string") (pack "KEY")) -- normal encryption
-- in  putStrLn $ (encrypt (fst . decode $ pack "e6148e3dd8e8") (pack "KEY")) -- decrypts to "string"
-- in  putStrLn . fromJust $ (encrypt2 (fst . decode $ pack "e6148e3dd8e8") (pack "KEY")) -- decrypts to "string"

main :: IO ()
main = execParser opts >>= searchKeys
    where
        opts = info (helper <*> parameters)
            ( fullDesc
           <> progDesc "Perform exhaustive search."
           <> header "rc4brute - ARC4 exhaustive search tool" )
