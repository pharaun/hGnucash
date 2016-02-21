{-# LANGUAGE OverloadedStrings #-}
import qualified Text.XML as X
import qualified Data.XML.Types as XT
import Data.XML.Pickle

import qualified Data.Map.Lazy as Map
import Data.Either
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import qualified Data.Text as T

import Test
import Types
import GnuCashParser
import Ledger


gimmieXml = do
    xml <- process "data/2015.xml"
    return $ rights [xml] !! 0

gimmieAccountMap xml = accountMap (accounts $ (books xml) !! 0)

showAccountMap am = do
    putStrLn $ Map.showTree $ Map.map aName am

showAccountList al = do
    putStrLn $ show $ reverse $ map (T.unpack . aName) al

pickAccount am = fromJust $ Map.lookup "a79d31b8d6e2d2b8d84a44a4c39c05b0" am

parentAllAccounts xml = do
    let am = gimmieAccountMap xml
    let parents = Map.foldr (\a l -> (a : findParents a am) : l) [] am

    mapM_ showAccountList parents





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
