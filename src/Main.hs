{-# LANGUAGE OverloadedStrings #-}
import qualified Text.XML as X
import qualified Data.XML.Types as XT
import Data.XML.Pickle

import System.Environment
import System.Exit
import System.IO

import qualified Data.Text as T

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


    let dat = GnuCash (CountData "book" 1)
                ( GnuCashBook "2.0.0" (BookId "guid" "3d2281ed92e804792714679c1b0cab5c") (CountData "account" 49) (CountData "transaction" 903)
                )

    let out = XT.Document (XT.Prologue [] Nothing []) (pickle (xpRoot $ xpUnliftElems xpGnuCash) dat) []
    let iin = unpickle (xpRoot $ xpUnliftElems xpGnuCash) (XT.documentRoot out)

    print dat
    putStrLn ""
    print out
    putStrLn ""
    print iin
    putStrLn ""
    case iin of
        Left _  -> print "False"
        Right x -> print (x == dat)
    putStrLn ""

    return $ unpickle (xpRoot $ xpUnliftElems xpGnuCash) (X.toXMLElement $ X.documentRoot inputNode)


data GnuCash = GnuCash
    { countBook :: CountData
    , books :: GnuCashBook
    } deriving (Show, Eq)

data CountData = CountData
    { countType :: T.Text
    , countAmount :: Integer
    } deriving (Show, Eq)

data GnuCashBook = GnuCashBook
    { version :: T.Text -- TODO: ADT this
    , guid :: BookId
    , countAccount :: CountData
    , countTransaction :: CountData
    } deriving (Show, Eq)

data BookId = BookId
    { idType :: T.Text
    , idValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)


xpGnuCash :: PU [XT.Node] GnuCash
xpGnuCash =
    xpWrap
        (uncurry GnuCash)
        (\(GnuCash count books) -> (count, books))
        -- TODO: verify type ~= "books"
        -- TODO: xpBooks -> (xpList0 xpBooks) (for some reason xpList breaks with xpPair)
        (xpElemNodes "gnc-v2" (xpPair xpCountData xpBooks))


xpCountData :: PU [XT.Node] CountData
xpCountData =
    xpWrap
        (uncurry CountData)
        (\(CountData cType amount) -> (cType, amount))
        (xpElem "gnc:count-data" (xpAttr "cd:type" xpText) (xpContent xpPrim))

xpBooks :: PU [XT.Node] GnuCashBook
xpBooks =
    xpWrap
        (\(version, (guid, account, transaction)) -> GnuCashBook version guid account transaction)
        (\(GnuCashBook version guid account transaction) -> (version, (guid, account, transaction)))
        (xpElem "gnc:book" (xpAttr "version" xpText)
            -- TODO: fix up xpCountData (It double reads the 'account' type it should read account then transactions
            (xpTriple xpBookId xpCountData xpCountData)
        )

xpBookId :: PU [XT.Node] BookId
xpBookId =
    xpWrap
        (uncurry BookId)
        (\(BookId idType idValue) -> (idType, idValue))
        (xpElem "book:id" (xpAttr "type" xpText) (xpContent xpText))
