{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, PolyKinds #-}
module Mermaid.Diagram.EntityRelationship
  ( -- * Entity relation diagram
    parseERDiagram
    -- * Entity relations
  , Entity
  , EntityRelation(..)
  , Relationship(..)
  , Cardinality(..)
  , Identification(..)
  ) where

import Data.Functor.Identity
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
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
  deriving Show

data EntityAttributes = EntityAttributes
  { entity :: Entity
  , nameAlias :: Maybe Text
  , typeNames :: [(AttributeType, Text)]
  }
  deriving Show

data AttributeType
  = ATString
  | ATInt
  deriving Show

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

data Relationship = Relationship
  { leftCardinality :: Cardinality
  , identification :: Identification
  , rightCardinality :: Cardinality
  }
  deriving Show

data Cardinality
  = ZeroOrOne
  | ExactlyOne
  | ZeroOrMore
  | OneOrMore
  deriving Show

data Identification
  = Identifying
  | NonIdentifying
  deriving Show

--------------------------------------------------------------------------------
-- * Parser
--------------------------------------------------------------------------------

type Parser = ParsecT Void Text Identity

pERDiagram :: Parser EntityRelationDiagram
pERDiagram = do
  _ <- string "erDigram" <* spaces
  (entityAttributes, entityRelations) <-
    partitionEithers <$> many (choice [Left <$> try pEntityAttributes, Right <$> pEntityRelation])
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
    <*> pLabel
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
      =   try (string (flip' "|o"))
      <|> string "one or zero"
      <|> string "zero or one"
    pExactlyOne
      =   string (flip' "||")
      <|> string "one or more"
      <|> string "one or many"
      <|> string "many(1)"
      <|> string "1+"
    pZeroOrMore
      =   string (flip' "}o")
      <|> string "zero or more"
      <|> string "zero or many"
      <|> string "many(0)"
      <|> string "0+"
    pOneOrMore
      =   string (flip' "}|")
      <|> string "only one"
      <|> string "1"
    flip' = if flipped then T.reverse else id

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
pLabel = stringLiteral <|> (T.pack <$> many alphaNumChar)

--------------------------------------------------------------------------------
-- * Parse Utils
--------------------------------------------------------------------------------

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '\"' *> manyTill anySingle (char '\"'))

spaces :: Parser ()
spaces = skipMany space

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

