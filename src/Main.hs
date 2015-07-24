{-# LANGUAGE OverloadedStrings #-}
import qualified Text.XML as X
import qualified Data.XML.Types as XT
import Data.XML.Pickle

import Data.Either
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
                [ GnuCashBook "2.0.0"
                    (BookId "guid" "3d2281ed92e804792714679c1b0cab5c")
                    (CountData "account" 49)
                    (CountData "transaction" 903)
                    [Commodity "2.0.0" (SpaceId "ISO4217" "USD") "currency"]
                    (PriceDb "2.0.0"
                        [ Price
                            (PriceId "guid" "c6e71724e737f4e5c7f5e1eaa60a1e32")
                            (SpaceId "ISO4217" "CNY")
                            (SpaceId "ISO4217" "USD")
                            (Date "2010-09-29 00:00:00 -0400")
                            "user:xfer-dialog"
                            "579/3737"
                        ]
                    )
                    [ Account "2.0.0" "Foobar" (AccountId "guid" "6149d1c96c021e5f5d0ff886718b7f7d") "ROOT" Nothing 0 Nothing Nothing
                    , Account "2.0.0" "Foobar"
                        (AccountId "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                        "BANK"
                        (Just $ SpaceId "ISO4217" "USD")
                        0
                        (Just $ AccountSlots
                            [ Slot "placeholder" "string" (Left "true")
                            , Slot "reconcile-info" "frame" (Right
                                [ Slot "list-date" "integer" (Left "0")
                                , Slot "last-interval" "frame" (Right
                                    [ Slot "days" "integer" (Left "3")
                                    , Slot "Months" "integer" (Left "0")
                                    ]
                                    )
                                ]
                                )
                            ]
                        )
                        (Just $ AccountParentId "guid" "ff9490d50f87adaa01bdaec2119f8a98")
                    ]
                ]

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
    , books :: [GnuCashBook]
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
    , commoditys :: [Commodity]
    , priceDb :: PriceDb
    , accounts :: [Account]
    } deriving (Show, Eq)

data Account = Account
    { aVersion :: T.Text -- TODO: ADT this
    , aName :: T.Text
    , aGuid :: AccountId
    , aType :: T.Text
    , aCommodity :: Maybe SpaceId
    , aCommodityScu :: Integer
    , aSlots :: Maybe AccountSlots
    , aParent :: Maybe AccountParentId
    } deriving (Show, Eq)

data AccountSlots = AccountSlots
    { asSlot :: [Slot]
    } deriving (Show, Eq)

data Slot = Slot
    { sKey :: T.Text
    , sType :: T.Text
    , sValue :: Either T.Text [Slot] -- NOTE: Text must be on Left (Otherwise xpEither won't work right for recursive pickling)
    } deriving (Show, Eq)


--
-- TODO: Maybe unify these somehow
--
data BookId = BookId
    { idType :: T.Text
    , idValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)

data SpaceId = SpaceId
    { sSpace :: T.Text
    , sId :: T.Text
    } deriving (Show, Eq)

data PriceId = PriceId
    { pIdType :: T.Text
    , pIdValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)

data AccountId = AccountId
    { aIdType :: T.Text
    , aIdValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)

data AccountParentId = AccountParentId
    { apIdType :: T.Text
    , apIdValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)

data TransactionId = TransactionId
    { tIdType :: T.Text
    , tIdValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)
--
-- TODO: Maybe unify these somehow
--

data Commodity = Commodity
    { cVersion :: T.Text -- TODO: ADT this
    , cSId :: SpaceId
    , cSource :: T.Text
    } deriving (Show, Eq)

data PriceDb = PriceDb
    { pVersion :: T.Text -- TODO: ADT this
    , prices :: [Price]
    } deriving (Show, Eq)

data Price = Price
    { pGuid :: PriceId
    , commoditySId :: SpaceId
    , currencySId :: SpaceId
    , pTime :: Date
    , pSource :: T.Text
    , pValue :: T.Text -- TODO: convert to rational
    } deriving (Show, Eq)

data Date = Date
    { date :: T.Text -- TODO: proper type/parsing
    } deriving (Show, Eq)



xpGnuCash :: PU [XT.Node] GnuCash
xpGnuCash =
    xpWrap
        (uncurry GnuCash)
        (\(GnuCash count books) -> (count, books))
        -- TODO: verify type ~= "books"
        (xpElemNodes "gnc-v2" (xpPair xpCountData (xpList xpBooks)))

xpCountData :: PU [XT.Node] CountData
xpCountData =
    xpWrap
        (uncurry CountData)
        (\(CountData cType amount) -> (cType, amount))
        (xpElem "{http://www.gnucash.org/XML/gnc}count-data" (xpAttr "{http://www.gnucash.org/XML/cd}type" xpText) (xpContent xpPrim))

xpBooks :: PU [XT.Node] GnuCashBook
xpBooks =
    xpWrap
        (\(version, (guid, account, transaction, commodity, priceDb, accountList)) -> GnuCashBook version guid account transaction commodity priceDb accountList)
        (\(GnuCashBook version guid account transaction commodity priceDb accountList) -> (version, (guid, account, transaction, commodity, priceDb, accountList)))
        (xpElem "{http://www.gnucash.org/XML/gnc}book" (xpAttr "version" xpText)
            (xp6Tuple xpBookId xpCountData xpCountData (xpList xpCommodity) xpPriceDb (xpList xpAccount))
        )


xpBookId :: PU [XT.Node] BookId
xpBookId =
    xpWrap
        (uncurry BookId)
        (\(BookId idType idValue) -> (idType, idValue))
        (xpElem "{http://www.gnucash.org/XML/book}id" (xpAttr "type" xpText) (xpContent xpText))

xpCommodity :: PU [XT.Node] Commodity
xpCommodity =
    xpWrap
        (\(version, (spaceId, _, source, _)) -> Commodity version spaceId source)
        (\(Commodity version spaceId source) -> (version, (spaceId, (), source, ())))
        (xpElem "{http://www.gnucash.org/XML/gnc}commodity" (xpAttr "version" xpText)
            (xp4Tuple
                xpSpaceId
                (xpElemBlank "{http://www.gnucash.org/XML/cmdty}get_quotes")
                (xpElemText "{http://www.gnucash.org/XML/cmdty}quote_source")
                (xpElemBlank "{http://www.gnucash.org/XML/cmdty}quote_tz")
            )
        )

xpSpaceId :: PU [XT.Node] SpaceId
xpSpaceId =
    xpWrap
        (uncurry SpaceId)
        (\(SpaceId space id) -> (space, id))
        (xp2Tuple (xpElemText "{http://www.gnucash.org/XML/cmdty}space") (xpElemText "{http://www.gnucash.org/XML/cmdty}id"))

xpPriceDb :: PU [XT.Node] PriceDb
xpPriceDb =
    xpWrap
        (uncurry PriceDb)
        (\(PriceDb version prices) -> (version, prices))
        (xpElem "{http://www.gnucash.org/XML/gnc}pricedb" (xpAttr "version" xpText) (xpList xpPrice))

xpPrice :: PU [XT.Node] Price
xpPrice =
    xpWrap
        (\(gid, commodity, currency, time, source, value) -> Price gid commodity currency time source value)
        (\(Price gid commodity currency time source value) -> (gid, commodity, currency, time, source, value))
        (xpElemNodes "price"
            (xp6Tuple
                xpPriceId
                (xpElemNodes "{http://www.gnucash.org/XML/price}commodity" xpSpaceId)
                (xpElemNodes "{http://www.gnucash.org/XML/price}currency" xpSpaceId)
                (xpElemNodes "{http://www.gnucash.org/XML/price}time" xpDate)
                (xpElemText "{http://www.gnucash.org/XML/price}source")
                (xpElemText "{http://www.gnucash.org/XML/price}value")
            )
        )

xpPriceId :: PU [XT.Node] PriceId
xpPriceId =
    xpWrap
        (uncurry PriceId)
        (\(PriceId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/price}id" (xpAttr "type" xpText) (xpContent xpText))

xpDate :: PU [XT.Node] Date
xpDate =
    xpWrap
        Date
        (\(Date date) -> (date))
        (xpElemText "{http://www.gnucash.org/XML/ts}date")

xpAccountId :: PU [XT.Node] AccountId
xpAccountId =
    xpWrap
        (uncurry AccountId)
        (\(AccountId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/act}id" (xpAttr "type" xpText) (xpContent xpText))

xpTransactionId :: PU [XT.Node] TransactionId
xpTransactionId =
    xpWrap
        (uncurry TransactionId)
        (\(TransactionId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/trn}id" (xpAttr "type" xpText) (xpContent xpText))

xpAccountParentId :: PU [XT.Node] AccountParentId
xpAccountParentId =
    xpWrap
        (uncurry AccountParentId)
        (\(AccountParentId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/act}parent" (xpAttr "type" xpText) (xpContent xpText))

xpAccount :: PU [XT.Node] Account
xpAccount =
    xpWrap
        (\(version, ((name, guid, aType), (commodity, commodityScu, slots, parent))) -> Account version name guid aType commodity commodityScu slots parent)
        (\(Account version name guid aType commodity commodityScu slots parent) -> (version, ((name, guid, aType), (commodity, commodityScu, slots, parent))))
        (xpElem "{http://www.gnucash.org/XML/gnc}account" (xpAttr "version" xpText)
            (xp2Tuple
                (xp3Tuple
                    (xpElemText "{http://www.gnucash.org/XML/act}name")
                    xpAccountId
                    (xpElemText "{http://www.gnucash.org/XML/act}type")
                )
                (xp4Tuple
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/act}commodity" xpSpaceId))
                    (xpElemNodes "{http://www.gnucash.org/XML/act}commodity-scu" (xpContent xpPrim))
                    (xpOption xpAccountSlots)
                    (xpOption xpAccountParentId)
                )
            )
        )

xpAccountSlots :: PU [XT.Node] AccountSlots
xpAccountSlots =
    xpWrap
        AccountSlots
        (\(AccountSlots slot) -> (slot))
        (xpElemNodes "{http://www.gnucash.org/XML/act}slots" (xpList xpSlot))

xpSlot :: PU [XT.Node] Slot
xpSlot =
    xpWrap
        (\(sKey, (sType, sValue)) -> Slot sKey sType sValue)
        (\(Slot sKey sType sValue) -> (sKey, (sType, sValue)))
        (xpElemNodes "slot"
            (xp2Tuple
                (xpElemText "{http://www.gnucash.org/XML/slot}key")
                (xpElem "{http://www.gnucash.org/XML/slot}value" (xpAttr "type" xpText) (xpEither (xpContent xpText) (xpList xpSlot)))
            )
        )
