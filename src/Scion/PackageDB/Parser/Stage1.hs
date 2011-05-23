{-# LANGUAGE RankNTypes #-}

module Scion.PackageDB.Parser.Stage1
( hoogleParser
, HoogleItem (..)
) where

import Control.Monad
import Data.List (intercalate)
import Scion.PackageDB
import Text.Parsec.ByteString as BS
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data HoogleItem = D String    -- Documentation comment
                | P String    -- @package
                | V String    -- @version
                | U String    -- @url
                | M [String]  -- Module declaration
                | I Item      -- Rest of elements
                deriving Show

type HoogleItemParser = forall st. BS.GenParser Char st HoogleItem

hoogleParser :: BS.GenParser Char st [HoogleItem]
hoogleParser = do spaces
                  many initialComment
                  spaces
                  items <- many (do item <- parseItem
                                    many1 eol
                                    return item)
                  spaces
                  eof
                  return items

initialComment :: BS.GenParser Char st String
initialComment = do try $ string "-- " >> notFollowedBy (char '|')
                    restOfLine
                    eol

parseItem :: HoogleItemParser
parseItem =   try parseDoc
          <|> try parsePackage
          <|> try parseVersion
          <|> try parseUrl
          <|> try parseModule
          <|> try parseData
          <|> try parseClass
          <|> try parseInstance
          <|> try parseType
          <|> try parseNewtype
          <|> try parseFunction

parseDoc :: HoogleItemParser
parseDoc = do string "-- | "
              initialLine <- restOfLine
              restOfLines <- many (try (eol >> string "--   ") >> restOfLine)
              return $ D (joinLines (initialLine:restOfLines))

parsePackage :: HoogleItemParser
parsePackage = do string "@package"
                  spaces1
                  name <- restOfLine
                  spaces0
                  return $ P name

parseVersion :: HoogleItemParser
parseVersion = do string "@version"
                  spaces1
                  version <- restOfLine
                  spaces0
                  return $ V version

parseUrl :: HoogleItemParser
parseUrl = do string "@url"
              spaces1
              url <- restOfLine
              return $ U url

parseModule :: HoogleItemParser
parseModule = do string "module"
                 spaces1
                 name <- (many $ noneOf ". \r\n") `sepBy` (char '.')
                 spaces0
                 return $ M name

parseParam :: BS.GenParser Char st String
parseParam = try (do char '('
                     spaces0
                     name <- identifier
                     params <- many (try (spaces1 >> parseParam))
                     spaces0
                     char ')'
                     return $ '(' : (intercalate " " (name:params)) ++ ")")
             <|> identifier

parseNameParams :: BS.GenParser Char st (String, [String])
parseNameParams = do name <- identifier
                     params <- many (try (spaces1 >> parseParam))
                     return (name, params)

parseNameParamsString :: BS.GenParser Char st String
parseNameParamsString = do (name, params) <- parseNameParams
                           return $ intercalate " " (name:params)

parseKind :: BS.GenParser Char st (Maybe String)
parseKind = (do string "::"
                spaces0
                k <- restOfLine
                return $ Just k)
            <|> (return Nothing)
                                
parsePrereqs :: BS.GenParser Char st [String]
parsePrereqs = try (do char '('
                       spaces0
                       firstPrereq <- parseNameParamsString
                       restPrereqs <- many (try (spaces0 >> char ',' >> spaces0 >> parseNameParamsString))
                       spaces0
                       char ')'
                       spaces0
                       string "=>"
                       return $ firstPrereq:restPrereqs
                    )
            <|> try (do prereq <- parseNameParamsString
                        spaces0
                        string "=>"
                        return [prereq])
            <|> return []

parseDataNewtype :: String -> (String -> [String] -> [Doc DataConstructor] -> Maybe String -> [String] -> Item) -> HoogleItemParser
parseDataNewtype kw f = do string kw
                           spaces1
                           prereqs <- parsePrereqs
                           spaces0
                           (name, params) <- parseNameParams
                           spaces0
                           kind <- parseKind
                           spaces0
                           return $ I (f name params [] kind prereqs)

parseData :: HoogleItemParser
parseData = parseDataNewtype "data" Data

parseNewtype :: HoogleItemParser
parseNewtype = parseDataNewtype "newtype" Newtype


parseClassFunDeps :: BS.GenParser Char st String
parseClassFunDeps = do char '|'
                       spaces0
                       fd <- restOfLine
                       return fd

parseBracketed :: BS.GenParser Char st String
parseBracketed = (do char '{'
                     s <- many parseBracketed
                     char '}'
                     return $ '{':(concat s ++ "}")
                  )
              <|> (many1 $ noneOf "{}\r\n")

parseClassTypeFam :: BS.GenParser Char st String
parseClassTypeFam = do string "where"
                       spaces0
                       tf <- parseBracketed
                       spaces0
                       return tf

parseClassExtra :: BS.GenParser Char st (Maybe String, Maybe String)
parseClassExtra = try (do fd <- parseClassFunDeps
                          return (Just fd, Nothing))
              <|> try (do tf <- parseClassTypeFam
                          return (Nothing, Just tf))
              <|> return (Nothing, Nothing)

parseClass :: HoogleItemParser
parseClass = do string "class"
                spaces1
                prereqs <- parsePrereqs
                spaces0
                (name, params) <- parseNameParams
                spaces0
                (fd, tf) <- parseClassExtra
                spaces0
                kind <- parseKind
                spaces0
                return $ I (Class name params prereqs kind fd tf)

parseInstance :: HoogleItemParser
parseInstance = do string "instance"
                   spaces1
                   decl <- restOfLine
                   return $ I (Instance decl)

parseType :: HoogleItemParser
parseType = do string "type"
               spaces1
               name <- identifier
               params <- many (try (spaces1 >> identifier))
               spaces0
               char '='
               spaces0
               decl <- restOfLine
               return $ I (Type name params decl)

parseFunction :: HoogleItemParser
parseFunction = do name <- identifier
                   spaces1
                   string "::"
                   spaces1
                   decl <- restOfLine
                   return $ I (Function name decl)

identifier :: BS.GenParser Char st String
identifier = (do initial <- letter <|> (char '_')
                 rest <- many $ alphaNum <|> oneOf "_'-[]#"
                 let id = initial:rest
                 guard $ id /= "where"
                 return id)
             <|>
             (do char '('
                 inner <- many (oneOf specialCharacters)
                 char ')'
                 return $ '(':(inner ++ ")") )
             <|>
             (do inner <- many1 (oneOf specialCharacters)
                 guard $ (inner /= "..") && (inner /= ":") && (inner /= "::")
                       && (inner /= "=") && (inner /= "\\") && (inner /= "|")
                       && (inner /= "<-") && (inner /= "->") && (inner /= "@")
                       && (inner /= "~") && (inner /= "=>")
                 return inner)

specialCharacters :: String
specialCharacters = ":!#$%&*+./<=>?@\\^|-~"

restOfLine :: BS.GenParser Char st String
restOfLine = many $ noneOf "\r\n"

eol :: BS.GenParser Char st String
eol =   try (string "\r\n")
    <|> try (string "\r")
    <|> string "\n"
    <?> "new line"

spaces0 :: BS.GenParser Char st String
spaces0 = many $ char ' '

spaces1 :: BS.GenParser Char st String
spaces1 = many1 $ char ' '

joinLines :: [String] -> String
joinLines = intercalate "\n"
               