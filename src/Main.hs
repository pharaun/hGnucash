{-# LANGUAGE OverloadedStrings #-}
import qualified Text.XML as X
import qualified Data.XML.Types as XT
import Data.XML.Pickle

import qualified Data.Map.Lazy as Map
import Data.Either
import System.Environment
import System.Exit
import System.IO

import qualified Data.Text as T

import Test
import Types
import GnuCashParser
import Ledger


main = do
    args <- getArgs
    case args of
        [filename] -> do
            xml <- process filename
            print xml

        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1

--process :: String -> IO (UNode T.Text, Maybe XMLParseError)
process filename = do
    inputNode <- X.readFile (X.def {X.psRetainNamespaces = False}) filename

    case (roundTrip gnuData) of
        Left _  -> print "error"
        Right x -> do
            putStrLn ""
            print $ fst x
            putStrLn ""
            print $ snd x
            putStrLn ""

    return $ unpickle (xpRoot $ xpUnliftElems xpGnuCash) (X.toXMLElement $ X.documentRoot inputNode)
