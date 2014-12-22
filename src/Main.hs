-- | A "hello world" example of hexpat that lazily parses a document, printing
-- it to standard out.

import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Pickle

import System.Environment
import System.Exit
import System.IO

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

main = do
    let pu = (xpRoot $ xpElemNodes "people" $ xpList xpPerson)

    BL.putStrLn $ pickleXML pu people
    print $ unpickleXML defaultParseOptions pu $ pickleXML pu people

--    args <- getArgs
--    case args of
--        [filename] -> do
--            (xml, mErr) <- process filename
--
--            -- Process document before handling error, so we get lazy processing.
--            BL.hPutStr stdout $ format xml
--            putStrLn ""
--            case mErr of
--                Nothing -> return ()
--                Just err -> do
--                    hPutStrLn stderr $ "XML parse failed: "++show err
--                    exitWith $ ExitFailure 2
--
--        otherwise  -> do
--            hPutStrLn stderr "Usage: helloworld <file.xml>"
--            exitWith $ ExitFailure 1

process :: String -> IO (UNode T.Text, Maybe XMLParseError)
process filename = do
    inputText <- BL.readFile filename
    return $ parse defaultParseOptions inputText

-- Pickle
data Person = Person String Int String deriving Show

people =
    [ Person "Dave" 27 "A fat thin man with long short hair"
    , Person "Jane" 21 "Lives in a white house with green windows"
    ]

xpPerson :: PU [UNode String] Person
xpPerson =
    -- How to wrap and unwrap a Person
    xpWrap (\((name, age), descr) -> Person name age descr,
        \(Person name age descr) -> ((name, age), descr)) $
    xpElem "person"
        (xpPair
        (xpAttr "name" xpText0)
        (xpAttr "age" xpickle))
        (xpContent xpText0)
