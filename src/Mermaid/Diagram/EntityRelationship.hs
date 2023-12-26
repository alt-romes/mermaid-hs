{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, PolyKinds #-}
  {-# OPTIONS_GHC -Wno-name-shadowing #-}
module Mermaid.Diagram.EntityRelationship
  ( -- * Entity relation diagram
    parseERDiagram
    -- * Entity relations
  , EntityRelationDiagram(..)
  , Entity
  , EntityAttributes(..), AttributeType(..)
  , EntityRelation(..)
  , Relationship(..)
  , Cardinality(..)
  , Identification(..)

    -- * Utils
  , Maybe'(..)
  ) where

import Data.Functor.Identity
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Either
import Control.Monad.Except

-- | Parse an ER diagram
parseERDiagram :: MonadError String m => Text -> m EntityRelationDiagram
parseERDiagram txt = case parse (spaces *> pERDiagram) "<input>" txt of
  Left e -> throwError (errorBundlePretty e)
  Right x -> pure x

data EntityRelationDiagram = ERDiagram
  { entityAttributes :: [EntityAttributes]
  , entityRelations :: [EntityRelation]
  }
  deriving (Show, Eq)

data EntityAttributes = EntityAttributes
  { entity :: Entity
  , nameAlias :: Maybe Text
  , typeNames :: [(AttributeType, Text)]
  }
  deriving (Show, Eq)

data AttributeType
  = ATString
  | ATInt
  | ATFloat
  deriving (Show, Eq)

type Entity = Text

-- | Only the first-entity part of a statement is mandatory. This makes it
-- possible to show an entity with no relationships, which can be useful during
-- iterative construction of diagrams. If any other parts of a statement are
-- specified, then all parts are mandatory.
data EntityRelation = forall exists. ER
  { firstEntity       :: Entity
  , relationship      :: Maybe' exists Relationship
  , secondEntity      :: Maybe' exists Entity
  , relationshipLabel :: Maybe' exists Text
  }
deriving instance Show EntityRelation
instance Eq EntityRelation where
  ER first' rel second' label == ER first'' rel' second'' label'
    = first' == first''
    && case (rel, rel') of
         (Just' rel, Just' rel')
           -> rel == rel'
            && case (second', second'') of
                 (Just' second', Just' second'')
                   -> second' == second''
                    && case (label, label') of
                         (Just' label, Just' label') -> label == label'
         (_, _) -> False


data Relationship = Relationship
  { leftCardinality :: Cardinality
  , identification :: Identification
  , rightCardinality :: Cardinality
  }
  deriving (Show, Eq)

data Cardinality
  = ZeroOrOne
  | ExactlyOne
  | ZeroOrMore
  | OneOrMore
  deriving (Show, Eq)

data Identification
  = Identifying
  | NonIdentifying
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * Parser
--------------------------------------------------------------------------------

type Parser = ParsecT Void Text Identity

pERDiagram :: Parser EntityRelationDiagram
pERDiagram = do
  _ <- string "erDiagram" <* spaces
  (entityAttributes, entityRelations) <-
    partitionEithers <$>
      many (choice
        [ Left <$> try pEntityAttributes
        , Right <$> pEntityRelation
        ]
           )
  return ERDiagram{entityAttributes, entityRelations}

pEntityAttributes :: Parser EntityAttributes
pEntityAttributes = do
  entity <- pEntity <* spaces
  nameAlias <- optional (between (char '[') (char ']') pLabel) <* spaces
  _ <- char '{' <* spaces
  typeNames <- many (pTypeName <* spaces)
  _ <- char '}' <* spaces
  return EntityAttributes{entity, nameAlias, typeNames}
  
pTypeName :: Parser (AttributeType, Text)
pTypeName = (,) <$> pAttributeType <* spaces <*> pLabel

pAttributeType :: Parser AttributeType
pAttributeType =
  choice
  [ ATString <$ string "string"
  , ATInt <$ string "int"
  , ATFloat <$ string "float"
  ]

pEntity :: Parser Entity
pEntity
  = fmap T.pack . (:) <$>
    (letterChar <|> char '_') <*>
    some (alphaNumChar <|> char '_' <|> char '-')

pEntityRelation :: Parser EntityRelation
pEntityRelation = do
  firstEntity <- pEntity <* spaces
  hasRelationship <- optional $ do
    (,,)
    <$> pRelationship <* spaces
    <*> pEntity <* spaces
    <* char ':' <* spaces
    <*> pLabel  <* spaces
  case hasRelationship of
    Nothing -> return
      ER{ firstEntity
        , relationship = Nothing'
        , secondEntity = Nothing'
        , relationshipLabel = Nothing'
        }
    Just (relationship', secondEntity', relationshipLabel') -> return
      ER{ firstEntity
        , relationship = Just' relationship'
        , secondEntity = Just' secondEntity'
        , relationshipLabel = Just' relationshipLabel'
        }

pRelationship :: Parser Relationship
pRelationship = Relationship <$> pCardinality False <*> pIdentification <*> pCardinality True

pCardinality :: Bool
             -- ^ Flipped?
             -> Parser Cardinality
pCardinality flipped =
  choice
  [ ZeroOrOne  <$ try pZeroOrOne
  , ExactlyOne <$ try pExactlyOne
  , ZeroOrMore <$ try pZeroOrMore
  , OneOrMore  <$ pOneOrMore
  ] where
    pZeroOrOne
      =   try (string (if flipped then "o|" else "|o"))
      <|> string "one or zero"
      <|> string "zero or one"
    pExactlyOne
      =   string "||"
      <|> string "one or more"
      <|> string "one or many"
      <|> string "many(1)"
      <|> string "1+"
    pZeroOrMore
      =   string (if flipped then "o{" else "}o")
      <|> string "zero or more"
      <|> string "zero or many"
      <|> string "many(0)"
      <|> string "0+"
    pOneOrMore
      =   string (if flipped then "|{" else "}|")
      <|> string "only one"
      <|> string "1"

pIdentification :: Parser Identification
pIdentification =
  choice
  [ Identifying    <$ pIdentifiying
  , NonIdentifying <$ pNonIdentifiying
  ] where
    pIdentifiying
      =   string "--"
      <|> string "to"
    pNonIdentifiying
      =   string ".."
      <|> string "optionally to"

pLabel :: Parser Text
pLabel = try stringLiteral <|> (T.pack <$> manyTill alphaNumChar newline)

--------------------------------------------------------------------------------
-- * Parse Utils
--------------------------------------------------------------------------------

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '\"' *> manyTill anySingle (char '\"'))

-- space consumer
spaces :: Parser ()
spaces = L.space
  space1
  empty -- (L.skipLineComment "//")
  empty

--------------------------------------------------------------------------------
-- * Type Utils
--------------------------------------------------------------------------------

-- | We use Maybe' to ensure that if a relationship exists then a second entity
-- exists as well without having to put the relationship and the second entity
-- into a separate datatype
data Maybe' exists a where
  Nothing' :: Maybe' 'False a
  Just'    :: a -> Maybe' 'True a

deriving instance Show a => Show (Maybe' exists a)
deriving instance Eq a => Eq (Maybe' exists a)

