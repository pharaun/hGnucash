{-# LANGUAGE OverloadedStrings #-}
module Ledger
    ( accountMap
    , findParents

    , transactionLine
    ) where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- Lazy debugging
import Debug.Trace

import Types


--
-- Helper functions
--
inspectId :: Account -> T.Text
inspectId a = if (idType $ aGuid a) /= (T.pack "guid")
              then trace ("Account IdType not guid! - " ++ (T.unpack $ aName a)) (idValue $ aGuid a)
              else (idValue $ aGuid a)

inspectParentId :: Account -> Maybe T.Text
inspectParentId a =
    case (aParent a) of
        Nothing -> Nothing
        Just p  -> Just $
                       if (idType p) /= (T.pack "guid")
                       then trace ("Account IdType not guid! - " ++ (T.unpack $ aName a)) (idValue p)
                       else (idValue p)

inspectAccountId :: Split -> T.Text
inspectAccountId s = if (idType $ spAccountId s) /= (T.pack "guid")
                     then trace ("Account IdType not guid! - " ++ (show $ sMemo s)) (idValue $ spAccountId s)
                     else (idValue $ spAccountId s)

--
-- * Map of Account Id -> Account (parent/account lookup)
--

-- Text is idValue out of TypedId
accountMap :: [Account] -> Map T.Text Account
accountMap accounts = foldl foldAccount Map.empty accounts
  where
    foldAccount :: Map T.Text Account -> Account -> Map T.Text Account
    foldAccount m a = Map.insert (inspectId a) a m


-- List is ordered from parent to child, does it include the given Account?
-- * Code to find all parent of an account
--  * For now it does not
findParents :: Account -> Map T.Text Account -> [Account]
findParents account am =
    case (inspectParentId account) of
        Nothing  -> []
        Just pid -> case (Map.lookup pid am) of
            Nothing -> trace ("Account parent not found in map! - " ++ (T.unpack $ aName account)) []
            Just pa -> pa : findParents pa am


--
-- Transaction process
--
transactionLine :: Transaction -> Map T.Text Account -> IO ()
transactionLine t am = do
    putStr "Description: "
    print $ tDescription t
    putStr "\tCurrency: "
    print $ tCurrency t
    putStr "\tPosted: "
    print $ tPosted t
    putStr "\tEntered: "
    print $ tEntered t

    putStrLn "\tAccount Splits:"
    mapM_ (\s -> do
        putStr "\t\tMemo: "
        print $ sMemo s
        putStr "\t\tAction: "
        print $ sAction s
        putStr "\t\tspReconciledState: "
        print $ spReconciledState s
        putStr "\t\tValue: "
        print $ spValue s
        putStr "\t\tQuanity: "
        print $ spQuantity s

        let accountId = inspectAccountId s
        let account = Map.lookup accountId am
        let account' = fromJust account

        let parents = findParents account' am

        putStr "\t\tAccounts:"
        putStrLn $ show $ reverse $ map (T.unpack . aName) (account' : parents)
        ) (tSplits t)


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
---- Core Type (Cannot access constructors for this)
--data TypedId t = TypedId
--    { idType :: Text
--    , idValue :: Text -- TODO: ADT this value/type
--    } deriving (Show, Eq)
--
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
---- TODO: make this handle <gdata> as well
--data Date = Date
--    { date :: Text -- TODO: proper type/parsing
--    , ns :: Maybe Text -- TODO: proper type/parsing (no idea what a ns is)
--    } deriving (Show, Eq)
