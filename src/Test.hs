{-# LANGUAGE OverloadedStrings #-}
module Test
    ( gnuData
    , roundTrip
    ) where

import qualified Text.XML as X
import qualified Data.XML.Types as XT
import Data.XML.Pickle

import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import Types
import GnuCashParser


gnuData :: GnuCash
gnuData =
    GnuCash (CountData "book" 1)
        [ GnuCashBook V200
            (ptBook "guid" "3d2281ed92e804792714679c1b0cab5c")
            (Just $ sBook
                [ Slot "placeholder" "string" (SVText "true")
                , Slot "reconcile-info" "frame" (SVText "hi")
                ]
            )
            (Map.fromList [("account", 49), ("transaction", 903)])
            [Commodity V200 (ptSpace "ISO4217" "USD") (Just "Yelp") (Just "US9858171054") (Just 1) Nothing (Just "currency") (Just ())]
            (Just $ PriceDb V1
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
            [ Account V200 "Foobar" (ptAccount "guid" "6149d1c96c021e5f5d0ff886718b7f7d") "ROOT" Nothing Nothing Nothing Nothing Nothing
            , Account V200 "Foobar"
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
            [ Transaction V100
                (ptTransaction "guid" "b9bbc2305a2be67bd1e16c283a6cd1ab")
                (ptSpace "ISO4217" "USD")
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
            (Just $ Budget V200 (ptBudget "guid" "ce3f353604f5b3d4f6a292bf598eb2d1") "Unnamed Budget" 12
                (Recurrence V100 1 "month" "2013-11-01")
            )
        ]


roundTrip :: GnuCash -> Either Text (Bool, Text)
roundTrip dat = do
    let out = XT.Document (XT.Prologue [] Nothing []) (pickle (xpRoot $ xpUnliftElems xpGnuCash) dat) []
    let iin = unpickle (xpRoot $ xpUnliftElems xpGnuCash) (XT.documentRoot out)

    case (X.fromXMLDocument out) of
        Left _  -> Left "Error?"
        Right x ->
            case iin of
                Left _  -> Left "Error?!"
                Right y -> Right ((y == dat), toStrict $ X.renderText X.def x)
