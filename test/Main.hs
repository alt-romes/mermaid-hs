{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Mermaid.Diagram.EntityRelationship
import qualified Data.Text.IO as T

main :: IO ()
main = do
  txt <- T.readFile "test/er.mermaid"
  case parseERDiagram txt of
    Left e -> error e
    Right x
      | x == er1 -> print x
      | otherwise -> error ("Expected " ++ show er1 ++ " but got " ++ show x)


er1 :: EntityRelationDiagram
er1 =
  ERDiagram
    { entityAttributes =
        [ EntityAttributes
            { entity = "CUSTOMER"
            , nameAlias = Nothing
            , typeNames =
                [ ( ATString , "name" )
                , ( ATString , "custNumber" )
                , ( ATString , "sector" )
                ]
            }
        , EntityAttributes
            { entity = "ORDER"
            , nameAlias = Nothing
            , typeNames =
                [ ( ATInt , "orderNumber" ) , ( ATString , "deliveryAddress" ) ]
            }
        , EntityAttributes
            { entity = "LINE-ITEM"
            , nameAlias = Nothing
            , typeNames =
                [ ( ATString , "productCode" )
                , ( ATInt , "quantity" )
                , ( ATFloat , "pricePerUnit" )
                ]
            }
        ]
    , entityRelations =
        [ ER
            { firstEntity = "CUSTOMER"
            , relationship =
                Just'
                  Relationship
                    { leftCardinality = ExactlyOne
                    , identification = Identifying
                    , rightCardinality = ZeroOrMore
                    }
            , secondEntity = Just' "ORDER"
            , relationshipLabel = Just' "places"
            }
        , ER
            { firstEntity = "ORDER"
            , relationship =
                Just'
                  Relationship
                    { leftCardinality = ExactlyOne
                    , identification = Identifying
                    , rightCardinality = OneOrMore
                    }
            , secondEntity = Just' "LINE-ITEM"
            , relationshipLabel = Just' "contains"
            }
        ]
    }

