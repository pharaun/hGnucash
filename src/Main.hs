import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Pickle

import System.Environment
import System.Exit
import System.IO

import qualified Data.ByteString.Lazy as BL
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
    inputText <- BL.readFile filename

    return $ unpickleXML defaultParseOptions xpGnuCash inputText


data GnuCash = GnuCash
    { countBook :: Integer
    , books :: [GnuCashBook]
    } deriving Show

data GnuCashBook = GnuCashBook
    { guid :: String
    , countAccount :: Integer
    , countTransaction :: Integer
    }
    | NoBook
    deriving Show


xpGnuCash :: PU (Node String String) GnuCash
xpGnuCash =
    xpWrap
        ( \(count, books) -> GnuCash count books
        , \(GnuCash count books) -> (count, books)
        ) $
        -- TODO: xpList -> xpListMinLength (of book count)
        -- TODO: verify type ~= "books"
        xpRoot (xpElemNodes "gnc-v2" (xpPair (xpElemNodes "gnc:count-data" (xpContent xpPrim)) (xpList xpBooks)))


--xpBooks :: PU (Node String String) GnuCashBook
xpBooks = xpLift NoBook


--    xpRoot
--        (xpElemNodes "gnc-v2"
--            (xpPair
--                (xpElem "gnc:count-data" (xpAttr "cd:type" xpText0) (xpContent xpPrim))
--                (xpListMinLen 1
--                    (xpElem "gnc:book"
--                        (xpAttr "version" xpText0)
--                        (xpTriple
--                            (xpElem "book:id" (xpAttr "type" xpText0) (xpContent xpPrim))
--                            (xpElem "gnc:count-data" (xpAttr "type" xpText0) (xpContent xpPrim))
--                            (xpElem "gnc:count-data" (xpAttr "type" xpText0) (xpContent xpPrim))
--                        )
--                    )
--                )
--            )
--        )
