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
import Control.Monad

import qualified Data.Text as T

import Test
import Types
import GnuCashParser
import Ledger


gimmieXml = do
    xml <- process "data/2015.xml" False
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

showAccountFull am field = do
    if isNothing field
    then mapM_ (\(_, a) -> print a >> putStrLn "") $ Map.toList am
    else mapM_ (\(_, a) -> print ((fromJust field) a)) $ Map.toList am


showMass field = do
    xmls <- mapM
        (\f -> fmap (\a -> rights [a] !! 0) (process f False))
        ("data/2004_2007.xml" : (map (\a -> "data/" ++ a ++ ".xml") ["2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"]))

    if isNothing field
    then mapM_ (\a -> print a >> putStrLn "") xmls
    else mapM_ (\a -> print ((fromJust field) a)) xmls


showTransactions xml = do
    mapM_ (\t -> transactionLine t (gimmieAccountMap xml)) (transaction $ (books xml) !! 0)


main = do
    args <- getArgs
    case args of
        [filename] -> do
            xml <- process filename True
            print xml

        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1

--process :: String -> IO (UNode T.Text, Maybe XMLParseError)
process filename roundtrip = do
    inputNode <- X.readFile (X.def {X.psRetainNamespaces = False}) filename

    when roundtrip $
        case (roundTrip gnuData) of
            Left _  -> print "error"
            Right x -> do
                putStrLn ""
                print $ fst x
                putStrLn ""
                print $ snd x
                putStrLn ""

    return $ unpickle (xpRoot $ xpUnliftElems xpGnuCash) (X.toXMLElement $ X.documentRoot inputNode)
