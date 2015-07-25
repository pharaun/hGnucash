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
                    (Just $ CountData "commodity" 2)
                    (CountData "account" 49)
                    (CountData "transaction" 903)
                    (Just $ CountData "price" 2)
                    [Commodity "2.0.0" (SpaceId "ISO4217" "USD") (Just "Yelp") (Just "US9858171054") (Just 1) Nothing (Just "currency") (Just ())]
                    (Just $ PriceDb "2.0.0"
                        [ Price
                            (PriceId "guid" "c6e71724e737f4e5c7f5e1eaa60a1e32")
                            (SpaceId "ISO4217" "CNY")
                            (SpaceId "ISO4217" "USD")
                            (Date "2010-09-29 00:00:00 -0400" Nothing)
                            "user:xfer-dialog"
                            "579/3737"
                        ]
                    )
                    [ Account "2.0.0" "Foobar" (AccountId "guid" "6149d1c96c021e5f5d0ff886718b7f7d") "ROOT" Nothing Nothing Nothing Nothing Nothing
                    , Account "2.0.0" "Foobar"
                        (AccountId "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                        "BANK"
                        (Just $ SpaceId "ISO4217" "USD")
                        (Just 100)
                        (Just "Unknown")
                        (Just $ AccountSlots
                            [ Slot "placeholder" "string" (SVText "true")
                            , Slot "reconcile-info" "frame" (SVSlot
                                [ Slot "list-date" "integer" (SVText "0")
                                , Slot "last-interval" "frame" (SVSlot
                                    [ Slot "days" "integer" (SVText "3")
                                    , Slot "Months" "integer" (SVText "0")
                                    , Slot "date-posted" "gdate" (SVGdate "0")
                                    ]
                                    )
                                ]
                                )
                            ]
                        )
                        (Just $ AccountParentId "guid" "ff9490d50f87adaa01bdaec2119f8a98")
                    ]
                    [ Transaction "2.0.0"
                        (TransactionId "guid" "b9bbc2305a2be67bd1e16c283a6cd1ab")
                        (SpaceId "ISO4217" "USD")
                        (Just $ Just 149)
                        (Date "2010-09-29 00:00:00 -0400" Nothing)
                        (Date "2010-09-29 00:00:00 -0400" (Just "623788000"))
                        "Opening Balance"
                        (Just $ TransactionSlots
                            [ Slot "placeholder" "string" (SVText "true")
                            ]
                        )
                        [ Split
                            (SplitId "guid" "95055fbe5a8b65bc93705d35e7f59636")
                            Nothing
                            Nothing
                            "y"
                            (Just $ Date "2010-09-29 00:00:00 -0400" Nothing)
                            "73923/100"
                            "73923/100"
                            (SplitAccountId "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                        , Split
                            (SplitId "guid" "95055fbe5a8b65bc93705d35e7f59636")
                            (Just "Buy")
                            (Just "Headlight")
                            "n"
                            Nothing
                            "73923/100"
                            "73923/100"
                            (SplitAccountId "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                        ]
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
    , countCommodity :: Maybe CountData
    , countAccount :: CountData
    , countTransaction :: CountData
    , countPrice :: Maybe CountData
    , commoditys :: [Commodity]
    , priceDb :: Maybe PriceDb
    , accounts :: [Account]
    , transaction :: [Transaction]
    } deriving (Show, Eq)

data Account = Account
    { aVersion :: T.Text -- TODO: ADT this
    , aName :: T.Text
    , aGuid :: AccountId
    , aType :: T.Text
    , aCommodity :: Maybe SpaceId
    , aCommodityScu :: Maybe Integer
    , aDescription :: Maybe T.Text
    , aSlots :: Maybe AccountSlots
    , aParent :: Maybe AccountParentId
    } deriving (Show, Eq)

data AccountSlots = AccountSlots
    { asSlot :: [Slot]
    } deriving (Show, Eq)

data SlotValue = SVText T.Text
               | SVSlot [Slot]
               | SVGdate T.Text -- TODO: date
               deriving (Show, Eq)

data Slot = Slot
    { sKey :: T.Text
    , sType :: T.Text
    , sValue :: SlotValue
    } deriving (Show, Eq)

data TransactionSlots = TransactionSlots
    { tsSlot :: [Slot]
    } deriving (Show, Eq)

data Transaction = Transaction
    { tVersion :: T.Text -- TODO: ADT this
    , tGuid :: TransactionId
    , tCurrency :: SpaceId
    , tNum :: Maybe (Maybe Integer) -- No idea what this is
    , tPosted :: Date
    , tEntered :: Date
    , tDescription :: T.Text
    , tSlots :: Maybe TransactionSlots
    , tSplits :: [Split]
    } deriving (Show, Eq)

data Split = Split
    { spId :: SplitId
    , sAction :: Maybe T.Text
    , sMemo :: Maybe T.Text
    , spReconciledState :: T.Text
    , spReconciledDate :: Maybe Date
    , spValue :: T.Text -- TODO: convert to rational
    , spQuantity :: T.Text -- TODO: convert to rational
    , spAccountId :: SplitAccountId
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

data SplitId = SplitId
    { sIdType :: T.Text
    , sIdValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)

data SplitAccountId = SplitAccountId
    { saIdType :: T.Text
    , saIdValue :: T.Text -- TODO: ADT the value/type
    } deriving (Show, Eq)
--
-- TODO: Maybe unify these somehow
--

data Commodity = Commodity
    { cVersion :: T.Text -- TODO: ADT this
    , cSId :: SpaceId
    , cName :: Maybe T.Text
    , cXCode :: Maybe T.Text
    , cFraction :: Maybe Integer
    , cQuote :: Maybe ()
    , cSource :: Maybe T.Text
    , cTz :: Maybe ()
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
    , ns :: Maybe T.Text -- TODO: proper type/parsing (no idea what a ns is)
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
        (\(version, (guid, (comCount, account, transaction, priceCount), commodity, priceDb, accountList, transactionList)) -> GnuCashBook version guid comCount account transaction priceCount commodity priceDb accountList transactionList)
        (\(GnuCashBook version guid comCount account transaction priceCount commodity priceDb accountList transactionList) -> (version, (guid, (comCount, account, transaction, priceCount), commodity, priceDb, accountList, transactionList)))
        (xpElem "{http://www.gnucash.org/XML/gnc}book" (xpAttr "version" xpText)
            (xp6Tuple
                xpBookId
                (xp4Tuple
                    (xpOption xpCountData)
                    xpCountData
                    xpCountData
                    (xpOption xpCountData)
                )
                (xpList xpCommodity)
                (xpOption xpPriceDb)
                (xpList xpAccount)
                (xpList xpTransaction)
            )
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
        (\(version, (spaceId, (name, xcode, fraction), quote, source, tz)) -> Commodity version spaceId name xcode fraction quote source tz)
        (\(Commodity version spaceId name xcode fraction quote source tz) -> (version, (spaceId, (name, xcode, fraction), quote, source, tz)))
        (xpElem "{http://www.gnucash.org/XML/gnc}commodity" (xpAttr "version" xpText)
            (xp5Tuple
                xpSpaceId
                (xp3Tuple
                    (xpOption (xpElemText "{http://www.gnucash.org/XML/cmdty}name"))
                    (xpOption (xpElemText "{http://www.gnucash.org/XML/cmdty}xcode"))
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/cmdty}fraction" (xpContent xpPrim)))
                )
                (xpOption (xpElemBlank "{http://www.gnucash.org/XML/cmdty}get_quotes"))
                (xpOption (xpElemText "{http://www.gnucash.org/XML/cmdty}quote_source"))
                (xpOption (xpElemBlank "{http://www.gnucash.org/XML/cmdty}quote_tz"))
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
        (uncurry Date)
        (\(Date date ns) -> (date, ns))
        (xp2Tuple
            (xpElemText "{http://www.gnucash.org/XML/ts}date")
            (xpOption (xpElemText "{http://www.gnucash.org/XML/ts}ns"))
        )


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

xpSplitId :: PU [XT.Node] SplitId
xpSplitId =
    xpWrap
        (uncurry SplitId)
        (\(SplitId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/split}id" (xpAttr "type" xpText) (xpContent xpText))

xpSplitAccountId :: PU [XT.Node] SplitAccountId
xpSplitAccountId =
    xpWrap
        (uncurry SplitAccountId)
        (\(SplitAccountId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/split}account" (xpAttr "type" xpText) (xpContent xpText))

xpAccountParentId :: PU [XT.Node] AccountParentId
xpAccountParentId =
    xpWrap
        (uncurry AccountParentId)
        (\(AccountParentId pIdType pIdValue) -> (pIdType, pIdValue))
        (xpElem "{http://www.gnucash.org/XML/act}parent" (xpAttr "type" xpText) (xpContent xpText))

xpAccount :: PU [XT.Node] Account
xpAccount =
    xpWrap
        (\(version, ((name, guid, aType), (commodity, commodityScu, desc, slots, parent))) -> Account version name guid aType commodity commodityScu desc slots parent)
        (\(Account version name guid aType commodity commodityScu desc slots parent) -> (version, ((name, guid, aType), (commodity, commodityScu, desc, slots, parent))))
        (xpElem "{http://www.gnucash.org/XML/gnc}account" (xpAttr "version" xpText)
            (xp2Tuple
                (xp3Tuple
                    (xpElemText "{http://www.gnucash.org/XML/act}name")
                    xpAccountId
                    (xpElemText "{http://www.gnucash.org/XML/act}type")
                )
                (xp5Tuple
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/act}commodity" xpSpaceId))
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/act}commodity-scu" (xpContent xpPrim)))
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/act}description" (xpContent xpText)))
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

xpTransactionSlots :: PU [XT.Node] TransactionSlots
xpTransactionSlots =
    xpWrap
        TransactionSlots
        (\(TransactionSlots slot) -> (slot))
        (xpElemNodes "{http://www.gnucash.org/XML/trn}slots" (xpList xpSlot))

xpSlot :: PU [XT.Node] Slot
xpSlot =
    xpWrap
        (\(sKey, (sType, sValue)) -> Slot sKey sType sValue)
        (\(Slot sKey sType sValue) -> (sKey, (sType, sValue)))
        (xpElemNodes "slot"
            (xp2Tuple
                (xpElemText "{http://www.gnucash.org/XML/slot}key")
                (xpElem "{http://www.gnucash.org/XML/slot}value" (xpAttr "type" xpText) xpSlotValue)
            )
        )

-- NOTE: order dependent (text, then gdate, then finally list)
xpSlotValue :: PU [XT.Node] SlotValue
xpSlotValue =
    xpAlt svSel
        [ xpWrap SVText  (\(SVText x) -> (x)) (xpContent xpText)
        , xpWrap SVGdate (\(SVGdate x) -> (x)) (xpElemText "gdate")
        , xpWrap SVSlot  (\(SVSlot x) -> (x)) (xpList xpSlot)
        ]
  where
    svSel :: SlotValue -> Int
    svSel (SVText _)  = 0
    svSel (SVGdate _) = 1
    svSel (SVSlot _)  = 2

xpTransaction :: PU [XT.Node] Transaction
xpTransaction =
    xpWrap
        (\(ver, (guid, currency, (num, posted, entered, desc, slots, splits))) -> Transaction ver guid currency num posted entered desc slots splits)
        (\(Transaction ver guid currency num posted entered desc slots splits) -> (ver, (guid, currency, (num, posted, entered, desc, slots, splits))))
        (xpElem "{http://www.gnucash.org/XML/gnc}transaction" (xpAttr "version" xpText)
            (xp3Tuple
                xpTransactionId
                (xpElemNodes "{http://www.gnucash.org/XML/trn}currency" xpSpaceId)
                (xp6Tuple
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/trn}num" (xpOption (xpContent xpPrim))))
                    (xpElemNodes "{http://www.gnucash.org/XML/trn}date-posted" xpDate)
                    (xpElemNodes "{http://www.gnucash.org/XML/trn}date-entered" xpDate)
                    (xpElemText "{http://www.gnucash.org/XML/trn}description")
                    (xpOption xpTransactionSlots)
                    (xpElemNodes "{http://www.gnucash.org/XML/trn}splits" (xpList xpSplit))
                )
            )
        )

xpSplit :: PU [XT.Node] Split
xpSplit =
    xpWrap
        (\(id, action, (memo, state, date, value, quantity, accountId)) -> Split id action memo state date value quantity accountId)
        (\(Split id action memo state date value quantity accountId) -> (id, action, (memo, state, date, value, quantity, accountId)))
        (xpElemNodes "{http://www.gnucash.org/XML/trn}split"
            (xp3Tuple
                xpSplitId
                (xpOption (xpElemText "{http://www.gnucash.org/XML/split}action"))
                (xp6Tuple
                    (xpOption (xpElemText "{http://www.gnucash.org/XML/split}memo"))
                    (xpElemText "{http://www.gnucash.org/XML/split}reconciled-state")
                    (xpOption (xpElemNodes "{http://www.gnucash.org/XML/split}reconcile-date" xpDate))
                    (xpElemText "{http://www.gnucash.org/XML/split}value")
                    (xpElemText "{http://www.gnucash.org/XML/split}quantity")
                    xpSplitAccountId
                )
            )
        )
