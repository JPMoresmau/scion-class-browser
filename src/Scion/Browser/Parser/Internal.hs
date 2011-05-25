{-# LANGUAGE RankNTypes #-}

module Scion.Browser.Parser.Internal where

import Control.Monad
import Data.List (intercalate)
import Distribution.Version
import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Syntax as UnAnn  -- unannotated syntax elements
import Scion.Browser
import Scion.Browser.Parser.Documentable
import Text.Parsec.ByteString as BS
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

type BSParser a = forall st. BS.GenParser Char st a

hoogleParser :: BSParser (Documented Package)
hoogleParser = undefined

initialComment :: BSParser String
initialComment = do try $ string "-- " >> notFollowedBy (char '|')
                    restOfLine
                    eol

docComment :: BSParser String
docComment = do string "-- | "
                initialLine <- restOfLine
                restOfLines <- many (try (eol >> string "--   ") >> restOfLine)
                return $ intercalate "\n" (initialLine:restOfLines)

documented :: (Doc -> BSParser a) -> BSParser a
documented p =   try (do d <- docComment
                         eol
                         p (docFromString d))
             <|> try (p NoDoc)

package :: BSParser String
package = do string "@package"
             spaces1
             name <- restOfLine
             spaces0
             return name

version :: BSParser Version
version = do string "@version"
             spaces1
             number <- number `sepBy` char '.'
             restOfLine
             return $ Version number []

module_ :: Doc -> BSParser (Documented Module)
module_ doc = do string "module"
                 spaces1
                 name <- moduleName
                 spaces0
                 
                 return $ Module doc
                                 (Just (ModuleHead NoDoc name Nothing Nothing))
                                 []
                                 []
                                 []

moduleName :: BSParser (Documented ModuleName)
moduleName = do cons <- conid `sepBy` char '.'
                let name = intercalate "." (map getid cons)
                return $ ModuleName NoDoc name

functionLike :: BSParser (Documented Name) -> BSParser (Documented Name, Documented Type)
functionLike p = do name <- p
                    spaces0
                    string "::"
                    spaces0
                    rest <- restOfLine
                    let Parser.ParseOk ty = Parser.parseType rest
                        ty' = document NoDoc ty
                    return (name, ty')

function :: Doc -> BSParser (Documented Decl)
function doc = do (name, ty) <- functionLike varid
                  return $ TypeSig doc [name] ty

constructor :: Doc -> BSParser (Documented GadtDecl)
constructor doc = do (name, ty) <- functionLike conid
                     return $ GadtDecl doc name ty

kind :: BSParser (Documented Kind)
kind = try (do k1 <- kindL
               spaces0
               string "->"
               spaces0
               k2 <- kind
               return $ KindFn NoDoc k1 k2)
       <|> kindL

kindL :: BSParser (Documented Kind)
kindL = (do char '('
            spaces0
            k <- kind
            spaces0
            char ')'
            return $ KindParen NoDoc k)
        <|>
        (do char '*'
            return $ KindStar NoDoc)
        <|>
        (do char '!'
            return $ KindBang NoDoc)
        <|>
        (do n <- varid
            return $ KindVar NoDoc n)

instance_ :: Doc -> BSParser (Documented Decl)
instance_ doc = do string "instance"
                   spaces1
                   rest <- restOfLine
                   let Parser.ParseOk ty' = Parser.parseType rest
                       (ctx, ty) = getContextAndType (document NoDoc ty')
                       ((TyCon _ qname):params) = lineariseType ty
                   return $ InstDecl doc ctx (IHead NoDoc qname params) Nothing

type_ :: Doc -> BSParser (Documented Decl)
type_ doc = do string "type"
               spaces1
               con <- conid
               vars <- many (try (spaces1 >> tyVarBind))
               spaces0
               char '='
               spaces0
               rest <- restOfLine
               let Parser.ParseOk ty' = Parser.parseType rest
                   ty = document NoDoc ty'
               return $ TypeDecl doc (DHead NoDoc con vars) ty

tyVarBind :: BSParser (Documented TyVarBind)
tyVarBind = (do char '('
                spaces0
                var <- varid
                spaces0
                string "::"
                spaces0
                k <- kind
                spaces0
                char ')'
                return $ KindedVar NoDoc var k)
            <|>
            (do var <- varid
                return $ UnkindedVar NoDoc var)

dataOrNewType :: String -> (Documented DataOrNew) -> Doc -> BSParser (Documented Decl)
dataOrNewType keyword dOrN doc = do string keyword
                                    spaces0
                                    rest <- many $ allButDoubleColon
                                    k <- optionMaybe (do string "::"
                                                         spaces0
                                                         kind)
                                    let Parser.ParseOk ty' = Parser.parseType rest
                                        ty = document NoDoc ty'
                                        (ctx, head) = typeToContextAndHead ty
                                    cons <- many $ try (eol >> spaces >> documented constructor)
                                    return $ GDataDecl doc dOrN ctx head k cons Nothing

data_ :: Doc -> BSParser (Documented Decl)
data_ = dataOrNewType "data" (DataType NoDoc)

newtype_ :: Doc -> BSParser (Documented Decl)
newtype_ = dataOrNewType "newtype" (NewType NoDoc)

{-
qualifiedVarid :: BSParser [String]
qualifiedVarid =    do id <- varid
                       return [id]
               <|>  do mod <- many1 (do m <- conid
                                        char '.'
                                        return m)
                       id <- varid
                       return $ mod ++ [id]

qualifiedConid :: BSParser [String]
qualifiedConid = conid `sepBy` char '.'
-}

varid :: BSParser (Documented Name)
varid = try (do initial <- lower <|> char '_'
                rest <- many $ alphaNum <|> oneOf allowedSpecialCharactersInIds
                let id = initial:rest
                guard $ not (id `elem` haskellKeywords)
                return $ Ident NoDoc id)
        <|> 
        try (do initial <- oneOf (tail specialCharacters)
                rest <- many (oneOf specialCharacters)
                let id = initial:rest
                guard $ not (id `elem` haskellReservedOps)
                return $ Symbol NoDoc id)
        <|>
        (do char '('
            id <- varid
            char ')'
            return id)

conid :: BSParser (Documented Name)
conid = (do initial <- upper
            rest <- many $ alphaNum <|> oneOf allowedSpecialCharactersInIds
            return $ Ident NoDoc (initial:rest))
        <|> 
        try (do initial <- char ':'
                rest <- many (oneOf specialCharacters)
                let id = initial:rest
                guard $ not (id `elem` haskellReservedOps)
                return $ Symbol NoDoc id)
        <|>
        (do char '('
            id <- conid
            char ')'
            return id)

getid :: Documented Name -> String
getid (Ident _ s)  = s
getid (Symbol _ s) = '(' : (s ++ ")" )

haskellKeywords :: [String]
haskellKeywords = [ "case", "class", "data", "default", "deriving", "do"
                  , "else", "foreign", "if", "import", "in", "infix", "infixl"
                  , "infixr", "instance", "let", "module", "newtype", "of"
                  , "then", "type", "where", "_" ]

haskellReservedOps :: [String]
haskellReservedOps = [ "..", ":",  "::",  "=",  "\\", "|", "<-", "->", "@", "~", "=>" ]

allowedSpecialCharactersInIds :: [Char]
allowedSpecialCharactersInIds = "_'-[]#"

specialCharacters :: [Char]
specialCharacters = ":!#$%&*+./<=>?@\\^|-~"

restOfLine :: BSParser String
restOfLine = many $ noneOf "\r\n"

allButDoubleColon :: BSParser Char
allButDoubleColon = try (do char ':'
                            notFollowedBy $ char ':'
                            return ':')
                    <|> (noneOf ":\r\n")

eol :: BSParser String
eol =   try (string "\r\n")
    <|> try (string "\r")
    <|> string "\n"
    <?> "new line"

number :: BSParser Int
number = do n <- many1 digit
            return $ read n

spaces0 :: BSParser String
spaces0 = many $ char ' '

spaces1 :: BSParser String
spaces1 = many1 $ char ' '

-- working with types

getContextAndType :: (Documented Type) -> (Maybe (Documented Context), Documented Type)
getContextAndType (TyForall _ _ ctx ty) = (ctx, ty)
getContextAndType ty                    = (Nothing, ty)

lineariseType :: Documented Type -> [Documented Type]
lineariseType (TyApp d x y) = (lineariseType x) ++ [y]
lineariseType ty            = [ty]

typeToContextAndHead :: (Documented Type) -> (Maybe (Documented Context), Documented DeclHead)
typeToContextAndHead t = let (ctx, ty) = getContextAndType t
                             ((TyCon _ (UnQual _ name)):params) = lineariseType ty
                             vars = map (\(TyVar d n) -> UnkindedVar d n) params
                         in  (ctx, DHead NoDoc name vars)

