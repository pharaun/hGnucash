{-# LANGUAGE OverloadedStrings #-}
module Types
    ( GnuCash (..)
    , CountData (..)
    , GnuCashBook (..)
    , Budget (..)
    , Recurrence (..)
    , Account (..)
    , Transaction (..)
    , Split (..)
    , Commodity (..)
    , PriceDb (..)
    , Price (..)
    , Version (..)
    , SlotValue (..)
    , Slot (..)
    , Date (..)

    -- Phantom Types bit n' pieces
    , TypedId (idType, idValue)

    , ptBook
    , ptSpace
    , ptPrice
    , ptAccount
    , ptAccountParent
    , ptTransaction
    , ptSplit
    , ptSplitAccount
    , ptBudget

    , xpBookId
    , xpPriceId
    , xpBudgetId
    , xpAccountId
    , xpTransactionId
    , xpSplitId
    , xpSplitAccountId
    , xpAccountParentId
    , xpSpaceId

    -- Second Phantom Type bit n' pieces
    , TypedSlots (slots)

    , sAccount
    , sBook
    , sTransaction
    , sSplit

    , xpAccountSlots
    , xpBookSlots
    , xpTransactionSlots
    , xpSplitSlots

    -- "Useful" list of pre-existing XML names
    , gnc
    , act
    , book
    , cd
    , cmdty
    , price
    , slot
    , split
    , sx
    , trn
    , ts
    , fs
    , bgt
    , recurrence
    , lot
    , addr
    , owner
    , billterm
    , btDays
    , btProx
    , cust
    , employee
    , entry
    , invoice
    , job
    , order
    , taxtable
    , tte
    , vendor

    ) where

import qualified Data.XML.Types as XT
import Data.XML.Pickle

import qualified Data.Map.Lazy as Map
import Data.Text (Text)

-- Lazy debugging
import Debug.Trace


data GnuCash = GnuCash
    { countBook :: CountData
    , books :: [GnuCashBook]
    } deriving (Show, Eq)

data CountData = CountData
    { countType :: Text
    , countAmount :: Integer
    } deriving (Show, Eq)

data GnuCashBook = GnuCashBook
    { version :: Version
    , guid :: TypedId PTBook
    , bookSlots :: Maybe (TypedSlots SBook)
    , count :: Map.Map Text Integer -- CountData basically
    , commoditys :: [Commodity]
    , priceDb :: Maybe PriceDb -- TODO: outta to be a List perhaps?
    , accounts :: [Account]
    , transaction :: [Transaction]
    , budget :: Maybe Budget
    } deriving (Show, Eq)

data Budget = Budget
    { bVersion :: Version
    , bGuid :: TypedId PTBudget
    , bName :: Text
    , bNumPeriods :: Integer
    , bRecurrence :: Recurrence
    } deriving (Show, Eq)

data Recurrence = Recurrence
    { rVersion :: Version
    , rMulti :: Integer
    , rPeriodType :: Text
    , rStart :: Text -- TODO: date (gdate)
    } deriving (Show, Eq)

data Account = Account
    { aVersion :: Version
    , aName :: Text
    , aGuid :: TypedId PTAccount
    , aType :: Text
    , aCommodity :: Maybe (TypedId PTSpace)
    , aCommodityScu :: Maybe Integer
    , aDescription :: Maybe Text
    , aSlots :: Maybe (TypedSlots SAccount)
    , aParent :: Maybe (TypedId PTAccountParent)
    } deriving (Show, Eq)

data Transaction = Transaction
    { tVersion :: Version
    , tGuid :: TypedId PTTransaction
    , tCurrency :: (TypedId PTSpace)
    , tNum :: Maybe (Maybe Integer) -- No idea what this is
    , tPosted :: Date
    , tEntered :: Date
    , tDescription :: Text
    , tSlots :: Maybe (TypedSlots STransaction)
    , tSplits :: [Split]
    } deriving (Show, Eq)

data Split = Split
    { spId :: TypedId PTSplit
    , sAction :: Maybe Text
    , sMemo :: Maybe Text
    , spReconciledState :: Text
    , spReconciledDate :: Maybe Date
    , spValue :: Text -- TODO: convert to rational
    , spQuantity :: Text -- TODO: convert to rational
    , spAccountId :: TypedId PTSplitAccount
    , spSlots :: Maybe (TypedSlots SSplit)
    } deriving (Show, Eq)

data Commodity = Commodity
    { cVersion :: Version
    , cSId :: TypedId PTSpace
    , cName :: Maybe Text
    , cXCode :: Maybe Text
    , cFraction :: Maybe Integer
    , cQuote :: Maybe ()
    , cSource :: Maybe Text
    , cTz :: Maybe ()
    } deriving (Show, Eq)

data PriceDb = PriceDb
    { pVersion :: Version
    , prices :: [Price]
    } deriving (Show, Eq)

data Price = Price
    { pGuid :: TypedId PTPrice
    , commoditySId :: TypedId PTSpace
    , currencySId :: TypedId PTSpace
    , pTime :: Date
    , pSource :: Text
    , pType :: Maybe Text
    , pValue :: Text -- TODO: convert to rational
    } deriving (Show, Eq)

--
-- Version
--  * Custom read/show because of 2.0.0 -> V200 for ex
--
data Version = V200 | V100 | V1
    deriving (Eq)

instance Show Version where
    show V1   = "1"
    show V100 = "1.0.0"
    show V200 = "2.0.0"

instance Read Version where
    readsPrec _ (v1:'.':v2:'.':v3:rest) =
        if (v1 == '2' && v2 == '0' && v3 == '0')
        then [(V200, rest)]
        else
            if (v1 == '1' && v2 == '0' && v3 == '0')
            then [(V100, rest)]
            else trace (show $ v1:'.':v2:'.':v3:[]) []
    -- V1 is needed for priceDB for a few versions of the file
    readsPrec _ (v1:rest) =
        if v1 == '1'
        then [(V1, rest)]
        else []
    readsPrec _ _ = []

--
-- TODO: make these more useful/strict (ie frame, integer, text, etc)
--
data SlotValue = SVText Text
               | SVSlot [Slot]
               | SVGdate Text -- TODO: date
               deriving (Show, Eq)

data Slot = Slot
    { sKey :: Text
    , sType :: Text -- TODO: dedup into SlotValue (for the type info)
    , sValue :: SlotValue
    } deriving (Show, Eq)

-- TODO: make this handle <gdata> as well
data Date = Date
    { date :: Text -- TODO: proper type/parsing
    , ns :: Maybe Text -- TODO: proper type/parsing (no idea what a ns is)
    } deriving (Show, Eq)


--
-- Phantom Types for Id Types
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

-- Core Type (Cannot access constructors for this)
data TypedId t = TypedId
    { idType :: Text
    , idValue :: Text -- TODO: ADT this value/type
    } deriving (Show, Eq)

--
-- Construct a value for each phantom type
--
ptBook :: Text -> Text -> TypedId PTBook
ptBook = TypedId
ptSpace :: Text -> Text -> TypedId PTSpace
ptSpace = TypedId
ptPrice :: Text -> Text -> TypedId PTPrice
ptPrice = TypedId
ptAccount :: Text -> Text -> TypedId PTAccount
ptAccount = TypedId
ptAccountParent :: Text -> Text -> TypedId PTAccountParent
ptAccountParent = TypedId
ptTransaction :: Text -> Text -> TypedId PTTransaction
ptTransaction = TypedId
ptSplit :: Text -> Text -> TypedId PTSplit
ptSplit = TypedId
ptSplitAccount :: Text -> Text -> TypedId PTSplitAccount
ptSplitAccount = TypedId
ptBudget :: Text -> Text -> TypedId PTBudget
ptBudget = TypedId

--
-- Phantom Type Parsers
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
-- Phantom Types for Slots
--
data SAccount
data SBook
data STransaction
data SSplit

-- Core Type (Cannot access constructors for this)
-- TODO: any way we can make this less rendundant (we are wrapping a list
--       of slot with a type....) (Can we make the slots itself typed)
data TypedSlots t = TypedSlots
    { slots :: [Slot]
    }
    deriving (Show, Eq)

--
-- Construct a value for each phantom type
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
-- Phantom Type Parsers
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

--
-- Leaky separation
-- Here because of Slots phantom type bits
--
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
