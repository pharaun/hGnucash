{-# LANGUAGE OverloadedStrings #-}
module Ledger
    ( accountMap

    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- Lazy debugging
import Debug.Trace

import Types

--
-- TODO:
-- * Map of Account Id -> Account (parent/account lookup)
-- * Code to find all parent of an account
-- * Sanity Checks
--

-- Text is idValue out of TypedId
accountMap :: [Account] -> Map T.Text Account
accountMap accounts = foldl foldAccount Map.empty accounts
  where
    foldAccount :: Map T.Text Account -> Account -> Map T.Text Account
    foldAccount m a = Map.insert (inspectId a) a m

    inspectId :: Account -> T.Text
    inspectId a = if (idType $ aGuid a) /= (T.pack "guid")
                  then trace ("Account IdType not guid! - " ++ (T.unpack $ aName a)) (idValue $ aGuid a)
                  else (idValue $ aGuid a)

-- List is ordered from parent to child
findParents :: Account -> Map T.Text Account -> [Account]
findParents = undefined


--data Account = Account
--    { aVersion :: Text -- TODO: ADT this
--    , aName :: Text
--    , aGuid :: TypedId PTAccount
--    , aType :: Text
--    , aCommodity :: Maybe (TypedId PTSpace)
--    , aCommodityScu :: Maybe Integer
--    , aDescription :: Maybe Text
--    , aSlots :: Maybe (TypedSlots SAccount)
--    , aParent :: Maybe (TypedId PTAccountParent)
--    } deriving (Show, Eq)
--
--data GnuCashBook = GnuCashBook
--    { version :: Text -- TODO: ADT this
--    , guid :: TypedId PTBook
--    , bookSlots :: Maybe (TypedSlots SBook)
--    , count :: Map.Map Text Integer -- CountData basically
--    , commoditys :: [Commodity]
--    , priceDb :: Maybe PriceDb -- TODO: outta to be a List perhaps?
--    , accounts :: [Account]
--    , transaction :: [Transaction]
--    , budget :: Maybe Budget
--    } deriving (Show, Eq)
--
---- Core Type (Cannot access constructors for this)
--data TypedId t = TypedId
--    { idType :: Text
--    , idValue :: Text -- TODO: ADT this value/type
--    } deriving (Show, Eq)
--
--
--
--data Budget = Budget
--    { bVersion :: Text -- TODO: ADT this
--    , bGuid :: TypedId PTBudget
--    , bName :: Text
--    , bNumPeriods :: Integer
--    , bRecurrence :: Recurrence
--    } deriving (Show, Eq)
--
--data Recurrence = Recurrence
--    { rVersion :: Text -- TODO: ADT this
--    , rMulti :: Integer
--    , rPeriodType :: Text
--    , rStart :: Text -- TODO: date (gdate)
--    } deriving (Show, Eq)
--
--data Transaction = Transaction
--    { tVersion :: Text -- TODO: ADT this
--    , tGuid :: TypedId PTTransaction
--    , tCurrency :: (TypedId PTSpace)
--    , tNum :: Maybe (Maybe Integer) -- No idea what this is
--    , tPosted :: Date
--    , tEntered :: Date
--    , tDescription :: Text
--    , tSlots :: Maybe (TypedSlots STransaction)
--    , tSplits :: [Split]
--    } deriving (Show, Eq)
--
--data Split = Split
--    { spId :: TypedId PTSplit
--    , sAction :: Maybe Text
--    , sMemo :: Maybe Text
--    , spReconciledState :: Text
--    , spReconciledDate :: Maybe Date
--    , spValue :: Text -- TODO: convert to rational
--    , spQuantity :: Text -- TODO: convert to rational
--    , spAccountId :: TypedId PTSplitAccount
--    , spSlots :: Maybe (TypedSlots SSplit)
--    } deriving (Show, Eq)
--
--data Commodity = Commodity
--    { cVersion :: Text -- TODO: ADT this
--    , cSId :: TypedId PTSpace
--    , cName :: Maybe Text
--    , cXCode :: Maybe Text
--    , cFraction :: Maybe Integer
--    , cQuote :: Maybe ()
--    , cSource :: Maybe Text
--    , cTz :: Maybe ()
--    } deriving (Show, Eq)
--
--data PriceDb = PriceDb
--    { pVersion :: Text -- TODO: ADT this
--    , prices :: [Price]
--    } deriving (Show, Eq)
--
--data Price = Price
--    { pGuid :: TypedId PTPrice
--    , commoditySId :: TypedId PTSpace
--    , currencySId :: TypedId PTSpace
--    , pTime :: Date
--    , pSource :: Text
--    , pType :: Maybe Text
--    , pValue :: Text -- TODO: convert to rational
--    } deriving (Show, Eq)
--
----
---- TODO: make these more useful/strict (ie frame, integer, text, etc)
----
--data SlotValue = SVText Text
--               | SVSlot [Slot]
--               | SVGdate Text -- TODO: date
--               deriving (Show, Eq)
--
--data Slot = Slot
--    { sKey :: Text
--    , sType :: Text -- TODO: dedup into SlotValue (for the type info)
--    , sValue :: SlotValue
--    } deriving (Show, Eq)
--
---- TODO: make this handle <gdata> as well
--data Date = Date
--    { date :: Text -- TODO: proper type/parsing
--    , ns :: Maybe Text -- TODO: proper type/parsing (no idea what a ns is)
--    } deriving (Show, Eq)
--
---- Core Type (Cannot access constructors for this)
---- TODO: any way we can make this less rendundant (we are wrapping a list
----       of slot with a type....) (Can we make the slots itself typed)
--data TypedSlots t = TypedSlots
--    { slots :: [Slot]
--    }
--    deriving (Show, Eq)
--
--
--
--data GnuCash = GnuCash
--    { countBook :: CountData
--    , books :: [GnuCashBook]
--    } deriving (Show, Eq)
--
--data CountData = CountData
--    { countType :: Text
--    , countAmount :: Integer
--    } deriving (Show, Eq)
