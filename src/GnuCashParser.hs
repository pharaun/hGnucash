{-# LANGUAGE OverloadedStrings #-}
module GnuCashParser
    ( xpGnuCash
    ) where

import qualified Data.XML.Types as XT
import Data.XML.Pickle

import qualified Data.Map.Lazy as Map
import Data.Text (Text)

import Types

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
     -> PU Text k -- ^ Pickler for keys (key)
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

xpVersion :: PU [Attribute] Text
xpVersion = xpAttr "version" xpText
