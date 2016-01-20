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
                    (ptBook "guid" "3d2281ed92e804792714679c1b0cab5c")
                    (Just $ sBook
                        [ Slot "placeholder" "string" (SVText "true")
                        , Slot "reconcile-info" "frame" (SVText "hi")
                        ]
                    )
                    (Map.fromList [("account", 49), ("transaction", 903)])
                    [Commodity "2.0.0" (ptSpace "ISO4217" "USD") (Just "Yelp") (Just "US9858171054") (Just 1) Nothing (Just "currency") (Just ())]
                    (Just $ PriceDb "2.0.0"
                        [ Price
                            (ptPrice "guid" "c6e71724e737f4e5c7f5e1eaa60a1e32")
                            (ptSpace "ISO4217" "CNY")
                            (ptSpace "ISO4217" "USD")
                            (Date "2010-09-29 00:00:00 -0400" Nothing)
                            "user:xfer-dialog"
                            (Just "last")
                            "579/3737"
                        ]
                    )
                    [ Account "2.0.0" "Foobar" (ptAccount "guid" "6149d1c96c021e5f5d0ff886718b7f7d") "ROOT" Nothing Nothing Nothing Nothing Nothing
                    , Account "2.0.0" "Foobar"
                        (ptAccount "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                        "BANK"
                        (Just $ ptSpace "ISO4217" "USD")
                        (Just 100)
                        (Just "Unknown")
                        (Just $ sAccount
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
                        (Just $ ptAccountParent "guid" "ff9490d50f87adaa01bdaec2119f8a98")
                    ]
                    [ Transaction "2.0.0"
                        (ptTransaction "guid" "b9bbc2305a2be67bd1e16c283a6cd1ab")
                        (TypedId "ISO4217" "USD")
                        (Just $ Just 149)
                        (Date "2010-09-29 00:00:00 -0400" Nothing)
                        (Date "2010-09-29 00:00:00 -0400" (Just "623788000"))
                        "Opening Balance"
                        (Just $ sTransaction
                            [ Slot "placeholder" "string" (SVText "true")
                            ]
                        )
                        [ Split
                            (ptSplit "guid" "95055fbe5a8b65bc93705d35e7f59636")
                            Nothing
                            Nothing
                            "y"
                            (Just $ Date "2010-09-29 00:00:00 -0400" Nothing)
                            "73923/100"
                            "73923/100"
                            (ptSplitAccount "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                            (Just $ sSplit
                                [ Slot "placeholder" "string" (SVText "true")
                                ]
                            )
                        , Split
                            (ptSplit "guid" "95055fbe5a8b65bc93705d35e7f59636")
                            (Just "Buy")
                            (Just "Headlight")
                            "n"
                            Nothing
                            "73923/100"
                            "73923/100"
                            (ptSplitAccount "guid" "6149d1c96c021e5f5d0ff886718b7f7d")
                            Nothing
                        ]
                    ]
                    (Just $ Budget "2.0.0" (ptBudget "guid" "ce3f353604f5b3d4f6a292bf598eb2d1") "Unnamed Budget" 12
                        (Recurrence "1.0.0" 1 "month" "2013-11-01")
                    )
                ]

    let out = XT.Document (XT.Prologue [] Nothing []) (pickle (xpRoot $ xpUnliftElems xpGnuCash) dat) []
    let iin = unpickle (xpRoot $ xpUnliftElems xpGnuCash) (XT.documentRoot out)

    print dat
    putStrLn ""
    print out
    putStrLn ""
    case (X.fromXMLDocument out) of
        Left _  -> print "Error?"
        Right x -> print $ X.renderText X.def x
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
    , guid :: TypedId PTBook
    , bookSlots :: Maybe (TypedSlots SBook)
    , count :: Map.Map T.Text Integer -- CountData basically
    , commoditys :: [Commodity]
    , priceDb :: Maybe PriceDb -- TODO: outta to be a List perhaps?
    , accounts :: [Account]
    , transaction :: [Transaction]
    , budget :: Maybe Budget
    } deriving (Show, Eq)

data Budget = Budget
    { bVersion :: T.Text -- TODO: ADT this
    , bGuid :: TypedId PTBudget
    , bName :: T.Text
    , bNumPeriods :: Integer
    , bRecurrence :: Recurrence
    } deriving (Show, Eq)

data Recurrence = Recurrence
    { rVersion :: T.Text -- TODO: ADT this
    , rMulti :: Integer
    , rPeriodType :: T.Text
    , rStart :: T.Text -- TODO: date (gdate)
    } deriving (Show, Eq)

data Account = Account
    { aVersion :: T.Text -- TODO: ADT this
    , aName :: T.Text
    , aGuid :: TypedId PTAccount
    , aType :: T.Text
    , aCommodity :: Maybe (TypedId PTSpace)
    , aCommodityScu :: Maybe Integer
    , aDescription :: Maybe T.Text
    , aSlots :: Maybe (TypedSlots SAccount)
    , aParent :: Maybe (TypedId PTAccountParent)
    } deriving (Show, Eq)

data Transaction = Transaction
    { tVersion :: T.Text -- TODO: ADT this
    , tGuid :: TypedId PTTransaction
    , tCurrency :: (TypedId PTSpace)
    , tNum :: Maybe (Maybe Integer) -- No idea what this is
    , tPosted :: Date
    , tEntered :: Date
    , tDescription :: T.Text
    , tSlots :: Maybe (TypedSlots STransaction)
    , tSplits :: [Split]
    } deriving (Show, Eq)

data Split = Split
    { spId :: TypedId PTSplit
    , sAction :: Maybe T.Text
    , sMemo :: Maybe T.Text
    , spReconciledState :: T.Text
    , spReconciledDate :: Maybe Date
    , spValue :: T.Text -- TODO: convert to rational
    , spQuantity :: T.Text -- TODO: convert to rational
    , spAccountId :: TypedId PTSplitAccount
    , spSlots :: Maybe (TypedSlots SSplit)
    } deriving (Show, Eq)

data Commodity = Commodity
    { cVersion :: T.Text -- TODO: ADT this
    , cSId :: TypedId PTSpace
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
    { pGuid :: TypedId PTPrice
    , commoditySId :: TypedId PTSpace
    , currencySId :: TypedId PTSpace
    , pTime :: Date
    , pSource :: T.Text
    , pType :: Maybe T.Text
    , pValue :: T.Text -- TODO: convert to rational
    } deriving (Show, Eq)

--
-- TODO: make these more useful/strict (ie frame, integer, text, etc)
--
data SlotValue = SVText T.Text
               | SVSlot [Slot]
               | SVGdate T.Text -- TODO: date
               deriving (Show, Eq)

data Slot = Slot
    { sKey :: T.Text
    , sType :: T.Text -- TODO: dedup into SlotValue (for the type info)
    , sValue :: SlotValue
    } deriving (Show, Eq)




--
-- Phantom type
--  Id types
--
data PTBook
data PTSpace
data PTPrice
data PTAccount
data PTAccountParent
data PTTransaction
data PTSplit
data PTSplitAccount
data PTBudget

--
-- TODO: Remove access to *This* one so users can't break types
--
data TypedId t = TypedId
    { idType :: T.Text
    , idValue :: T.Text -- TODO: ADT this value/type
    } deriving (Show, Eq)

--
-- Ways to create each phantom type
--
ptBook :: T.Text -> T.Text -> TypedId PTBook
ptBook = TypedId
ptSpace :: T.Text -> T.Text -> TypedId PTSpace
ptSpace = TypedId
ptPrice :: T.Text -> T.Text -> TypedId PTPrice
ptPrice = TypedId
ptAccount :: T.Text -> T.Text -> TypedId PTAccount
ptAccount = TypedId
ptAccountParent :: T.Text -> T.Text -> TypedId PTAccountParent
ptAccountParent = TypedId
ptTransaction :: T.Text -> T.Text -> TypedId PTTransaction
ptTransaction = TypedId
ptSplit :: T.Text -> T.Text -> TypedId PTSplit
ptSplit = TypedId
ptSplitAccount :: T.Text -> T.Text -> TypedId PTSplitAccount
ptSplitAccount = TypedId
ptBudget :: T.Text -> T.Text -> TypedId PTBudget
ptBudget = TypedId

--
-- Generic TypedId
--
xpTypedId :: XT.Name -> PU [XT.Node] (TypedId t)
xpTypedId name =
    xpWrap
        (uncurry TypedId)
        (\(TypedId idType idValue) -> (idType, idValue))
        (xpElem name (xpAttr "type" xpText) (xpContent xpText))

xpBookId :: PU [XT.Node] (TypedId PTBook)
xpBookId = xpTypedId (book "id")

xpPriceId :: PU [XT.Node] (TypedId PTPrice)
xpPriceId = xpTypedId (price "id")

xpBudgetId :: PU [XT.Node] (TypedId PTBudget)
xpBudgetId = xpTypedId (bgt "id")

xpAccountId :: PU [XT.Node] (TypedId PTAccount)
xpAccountId = xpTypedId (act "id")

xpTransactionId :: PU [XT.Node] (TypedId PTTransaction)
xpTransactionId = xpTypedId (trn "id")

xpSplitId :: PU [XT.Node] (TypedId PTSplit)
xpSplitId = xpTypedId (split "id")

xpSplitAccountId :: PU [XT.Node] (TypedId PTSplitAccount)
xpSplitAccountId = xpTypedId (split "account")

xpAccountParentId :: PU [XT.Node] (TypedId PTAccountParent)
xpAccountParentId = xpTypedId (act "parent")

xpSpaceId :: PU [XT.Node] (TypedId PTSpace)
xpSpaceId =
    xpWrap
        (uncurry TypedId)
        (\(TypedId space id) -> (space, id))
        (xp2Tuple (xpElemText (cmdty "space")) (xpElemText (cmdty "id")))

--
-- Phantom type
--
data SAccount
data SBook
data STransaction
data SSplit

--
-- TODO: Remove access to *This* one so users can't break types
-- TODO: any way we can make this less rendundant (we are wrapping a list
-- of slot with a type....) (Can we make the slots itself typed)
--
data TypedSlots t = TypedSlots [Slot]
    deriving (Show, Eq)

--
-- Ways to create each phantom type
--
sAccount :: [Slot] -> TypedSlots SAccount
sAccount = TypedSlots
sBook :: [Slot] -> TypedSlots SBook
sBook = TypedSlots
sTransaction :: [Slot] -> TypedSlots STransaction
sTransaction = TypedSlots
sSplit :: [Slot] -> TypedSlots SSplit
sSplit = TypedSlots

--
-- Generic TypedSlots
--
xpTypedSlots :: XT.Name -> PU [XT.Node] (TypedSlots t)
xpTypedSlots name =
    xpWrap
        TypedSlots
        (\(TypedSlots slot) -> (slot))
        (xpElemNodes name (xpList xpSlot))

xpAccountSlots :: PU [XT.Node] (TypedSlots SAccount)
xpAccountSlots = xpTypedSlots (act "slots")
xpBookSlots :: PU [XT.Node] (TypedSlots SBook)
xpBookSlots = xpTypedSlots (book "slots")
xpTransactionSlots :: PU [XT.Node] (TypedSlots STransaction)
xpTransactionSlots = xpTypedSlots (trn "slots")
xpSplitSlots :: PU [XT.Node] (TypedSlots SSplit)
xpSplitSlots = xpTypedSlots (split "slots")





-- TODO: make this handle <gdata> as well
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
        (xpElem (gnc "count-data") (xpAttr (cd "type") xpText) (xpContent xpPrim))

--
-- Restricted xpMap
--
xpSubsetMap :: Ord k =>
     (XT.Node -> Bool) -- ^ Predicate to select the subset
     -> XT.Name  -- ^ Element name (elt)
     -> XT.Name  -- ^ Attribute name (attr)
     -> PU T.Text k -- ^ Pickler for keys (key)
     -> PU [XT.Node] a  -- ^ Pickler for values (value)
     -> PU [XT.Node] (Map.Map k a)
xpSubsetMap p en an xpk xpv =
    xpWrap
        Map.fromList
        Map.toList
        (xpSubsetAll p (xpElem en (xpAttr an xpk) xpv))


xpBooks :: PU [XT.Node] GnuCashBook
xpBooks =
    xpWrap
        (\(version, (guid, (slots, count, commodity, priceDb, accountList, transactionList), budget)) -> GnuCashBook version guid slots count commodity priceDb accountList transactionList budget)
        (\(GnuCashBook version guid slots count commodity priceDb accountList transactionList budget) -> (version, (guid, (slots, count, commodity, priceDb, accountList, transactionList), budget)))
        (xpElem (gnc "book") xpVersion
            (xp3Tuple
                xpBookId
                (xp6Tuple
                    (xpOption xpBookSlots)
                    (xpSubsetMap countDataNode (gnc "count-data") (cd "type") xpText (xpContent xpPrim))
                    (xpList xpCommodity)
                    (xpOption xpPriceDb)
                    (xpList xpAccount)
                    (xpList xpTransaction)
                )
                (xpOption xpBudget)
            )
        )
  where
    countDataNode :: XT.Node -> Bool
    countDataNode (XT.NodeElement (XT.Element{XT.elementName=XT.Name{XT.nameLocalName="count-data"}})) = True
    countDataNode _ = False

xpCommodity :: PU [XT.Node] Commodity
xpCommodity =
    xpWrap
        (\(version, (spaceId, (name, xcode, fraction), quote, source, tz)) -> Commodity version spaceId name xcode fraction quote source tz)
        (\(Commodity version spaceId name xcode fraction quote source tz) -> (version, (spaceId, (name, xcode, fraction), quote, source, tz)))
        (xpElem (gnc "commodity") xpVersion
            (xp5Tuple
                xpSpaceId
                (xp3Tuple
                    (xpOption (xpElemText (cmdty "name")))
                    (xpOption (xpElemText (cmdty "xcode")))
                    (xpOption (xpElemNodes (cmdty "fraction") (xpContent xpPrim)))
                )
                (xpOption (xpElemBlank (cmdty "get_quotes")))
                (xpOption (xpElemText (cmdty "quote_source")))
                (xpOption (xpElemBlank (cmdty "quote_tz")))
            )
        )

xpPriceDb :: PU [XT.Node] PriceDb
xpPriceDb =
    xpWrap
        (uncurry PriceDb)
        (\(PriceDb version prices) -> (version, prices))
        (xpElem (gnc "pricedb") xpVersion (xpList xpPrice))

xpPrice :: PU [XT.Node] Price
xpPrice =
    xpWrap
        (\(gid, (commodity, currency, time, source, pType, value)) -> Price gid commodity currency time source pType value)
        (\(Price gid commodity currency time source pType value) -> (gid, (commodity, currency, time, source, pType, value)))
        (xpElemNodes "price"
            (xp2Tuple
                xpPriceId
                (xp6Tuple
                    (xpElemNodes (price "commodity") xpSpaceId)
                    (xpElemNodes (price "currency") xpSpaceId)
                    (xpElemNodes (price "time") xpDate)
                    (xpElemText (price "source"))
                    (xpOption (xpElemText (price "type")))
                    (xpElemText (price "value"))
                )
            )
        )

xpDate :: PU [XT.Node] Date
xpDate =
    xpWrap
        (uncurry Date)
        (\(Date date ns) -> (date, ns))
        (xp2Tuple
            (xpElemText (ts "date"))
            (xpOption (xpElemText (ts "ns")))
        )





xpAccount :: PU [XT.Node] Account
xpAccount =
    xpWrap
        (\(version, ((name, guid, aType), (commodity, commodityScu, desc, slots, parent))) -> Account version name guid aType commodity commodityScu desc slots parent)
        (\(Account version name guid aType commodity commodityScu desc slots parent) -> (version, ((name, guid, aType), (commodity, commodityScu, desc, slots, parent))))
        (xpElem (gnc "account") xpVersion
            (xp2Tuple
                (xp3Tuple
                    (xpElemText (act "name"))
                    xpAccountId
                    (xpElemText (act "type"))
                )
                (xp5Tuple
                    (xpOption (xpElemNodes (act "commodity") xpSpaceId))
                    (xpOption (xpElemNodes (act "commodity-scu") (xpContent xpPrim)))
                    (xpOption (xpElemNodes (act "description") (xpContent xpText)))
                    (xpOption xpAccountSlots)
                    (xpOption xpAccountParentId)
                )
            )
        )

xpTransaction :: PU [XT.Node] Transaction
xpTransaction =
    xpWrap
        (\(ver, (guid, currency, (num, posted, entered, desc, slots, splits))) -> Transaction ver guid currency num posted entered desc slots splits)
        (\(Transaction ver guid currency num posted entered desc slots splits) -> (ver, (guid, currency, (num, posted, entered, desc, slots, splits))))
        (xpElem (gnc "transaction") xpVersion
            (xp3Tuple
                xpTransactionId
                (xpElemNodes (trn "currency") xpSpaceId)
                (xp6Tuple
                    (xpOption (xpElemNodes (trn "num") (xpOption (xpContent xpPrim))))
                    (xpElemNodes (trn "date-posted") xpDate)
                    (xpElemNodes (trn "date-entered") xpDate)
                    (xpElemText (trn "description"))
                    (xpOption xpTransactionSlots)
                    (xpElemNodes (trn "splits") (xpList xpSplit))
                )
            )
        )

xpSplit :: PU [XT.Node] Split
xpSplit =
    xpWrap
        (\(id, action, memo, (state, date, value, quantity, accountId, slots)) -> Split id action memo state date value quantity accountId slots)
        (\(Split id action memo state date value quantity accountId slots) -> (id, action, memo, (state, date, value, quantity, accountId, slots)))
        (xpElemNodes (trn "split")
            (xp4Tuple
                xpSplitId
                (xpOption (xpElemText (split "action")))
                (xpOption (xpElemText (split "memo")))
                (xp6Tuple
                    (xpElemText (split "reconciled-state"))
                    (xpOption (xpElemNodes (split "reconcile-date") xpDate))
                    (xpElemText (split "value"))
                    (xpElemText (split "quantity"))
                    xpSplitAccountId
                    (xpOption xpSplitSlots)
                )
            )
        )

xpBudget :: PU [XT.Node] Budget
xpBudget =
    xpWrap
        (\(version, (id, name, _, peroid, recurrence)) -> Budget version id name peroid recurrence)
        (\(Budget version id name peroid recurrence) -> (version, (id, name, (), peroid, recurrence)))
        (xpElem (gnc "budget") xpVersion
            (xp5Tuple
                xpBudgetId
                (xpElemText (bgt "name"))
                (xpElemBlank (bgt "description"))
                (xpElemNodes (bgt "num-periods") (xpContent xpPrim))
                xpRecurrence
            )
        )

xpRecurrence :: PU [XT.Node] Recurrence
xpRecurrence =
    xpWrap
        (\(version, (multi, period, start)) -> Recurrence version multi period start)
        (\(Recurrence version multi period start) -> (version, (multi, period, start)))
        (xpElem (bgt "recurrence") xpVersion
            (xp3Tuple
                (xpElemNodes (recurrence "mult") (xpContent xpPrim))
                (xpElemText (recurrence "period_type"))
                (xpElemNodes (recurrence "start") (xpElemText "gdate"))
            )
        )

xpVersion :: PU [Attribute] T.Text
xpVersion = xpAttr "version" xpText


xpSlot :: PU [XT.Node] Slot
xpSlot =
    xpWrap
        (\(sKey, (sType, sValue)) -> Slot sKey sType sValue)
        (\(Slot sKey sType sValue) -> (sKey, (sType, sValue)))
        (xpElemNodes "slot"
            (xp2Tuple
                (xpElemText (slot "key"))
                (xpElem (slot "value") (xpAttr "type" xpText) xpSlotValue)
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


--
-- "Useful" list of pre-existing XML names
--
gnc n        = XT.Name n (Just "http://www.gnucash.org/XML/gnc") (Just "gnc")
act n        = XT.Name n (Just "http://www.gnucash.org/XML/act") (Just "act")
book n       = XT.Name n (Just "http://www.gnucash.org/XML/book") (Just "book")
cd n         = XT.Name n (Just "http://www.gnucash.org/XML/cd") (Just "cd")
cmdty n      = XT.Name n (Just "http://www.gnucash.org/XML/cmdty") (Just "cmdty")
price n      = XT.Name n (Just "http://www.gnucash.org/XML/price") (Just "price")
slot n       = XT.Name n (Just "http://www.gnucash.org/XML/slot") (Just "slot")
split n      = XT.Name n (Just "http://www.gnucash.org/XML/split") (Just "split")
sx n         = XT.Name n (Just "http://www.gnucash.org/XML/sx") (Just "sx")
trn n        = XT.Name n (Just "http://www.gnucash.org/XML/trn") (Just "trn")
ts n         = XT.Name n (Just "http://www.gnucash.org/XML/ts") (Just "ts")
fs n         = XT.Name n (Just "http://www.gnucash.org/XML/fs") (Just "fs")
bgt n        = XT.Name n (Just "http://www.gnucash.org/XML/bgt") (Just "bgt")
recurrence n = XT.Name n (Just "http://www.gnucash.org/XML/recurrence") (Just "recurrence")
lot n        = XT.Name n (Just "http://www.gnucash.org/XML/lot") (Just "lot")
addr n       = XT.Name n (Just "http://www.gnucash.org/XML/addr") (Just "addr")
owner n      = XT.Name n (Just "http://www.gnucash.org/XML/owner") (Just "owner")
billterm n   = XT.Name n (Just "http://www.gnucash.org/XML/billterm") (Just "billterm")
btDays n     = XT.Name n (Just "http://www.gnucash.org/XML/bt-days") (Just "bt-days")
btProx n     = XT.Name n (Just "http://www.gnucash.org/XML/bt-prox") (Just "bt-prox")
cust n       = XT.Name n (Just "http://www.gnucash.org/XML/cust") (Just "cust")
employee n   = XT.Name n (Just "http://www.gnucash.org/XML/employee") (Just "employee")
entry n      = XT.Name n (Just "http://www.gnucash.org/XML/entry") (Just "entry")
invoice n    = XT.Name n (Just "http://www.gnucash.org/XML/invoice") (Just "invoice")
job n        = XT.Name n (Just "http://www.gnucash.org/XML/job") (Just "job")
order n      = XT.Name n (Just "http://www.gnucash.org/XML/order") (Just "order")
taxtable n   = XT.Name n (Just "http://www.gnucash.org/XML/taxtable") (Just "taxtable")
tte n        = XT.Name n (Just "http://www.gnucash.org/XML/tte") (Just "tte")
vendor n     = XT.Name n (Just "http://www.gnucash.org/XML/vendor") (Just "vendor")
