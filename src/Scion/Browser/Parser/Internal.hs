{-# LANGUAGE RankNTypes #-}
module Scion.Browser.Parser.Internal where

import Control.Monad
import Data.Char (isControl)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.String.Utils (replace)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Version
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Extension
import qualified Language.Haskell.Exts.Parser as Parser
import Scion.Browser
import Scion.Browser.Parser.Documentable
import Text.Parsec.String as BS
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

type BSParser a = forall st. BS.GenParser Char st a

hoogleParser :: BSParser (Documented Package)
hoogleParser = do spaces
                  many initialComment
                  spaces
                  pkgDoc <- docComment
                  spacesOrEol1
                  pkgN <- package
                  spacesOrEol1
                  pkgV <- version
                  spaces0
                  modules <- many $ try (spacesOrEol0 >> documented module_)
                  spaces
                  eof
                  return $ Package (docFromString pkgDoc)
                                   (PackageIdentifier (PackageName pkgN)
                                                      pkgV)
                                   (M.fromList $ map (\m -> (getModuleName m, m)) modules)

initialComment :: BSParser String
initialComment = do try $ string "-- " >> notFollowedBy (char '|')
                    restOfLine
                    eol

docComment :: BSParser String
docComment = do string "-- | "
                initialLine <- restOfLine
                restOfLines <- many $ try (eol >> string "--   ") >> restOfLine
                return $ intercalate "\n" (initialLine:restOfLines)

documented :: (Doc -> BSParser a) -> BSParser a
documented p =   try (do d <- try docComment
                         try eol
                         p (docFromString d))
             <|> try (p NoDoc)

package :: BSParser String
package = do string "@package"
             spaces1
             name <- restOfLine
             spaces0
             return name

version :: BSParser Version
version = try (do string "@version"
                  spaces1
                  numbers <- number `sepBy` char '.'
                  restOfLine
                  return $ Version numbers [])
          <|> (return $ Version [] [])

module_ :: Doc -> BSParser (Documented Module)
module_ doc = do string "module"
                 spaces1
                 name <- moduleName
                 spaces0
                 decls <- many $ try (spacesOrEol0 >> documented decl)
                 return $ Module doc
                                 (Just (ModuleHead NoDoc name Nothing Nothing))
                                 []
                                 []
                                 (concat decls)

moduleName :: BSParser (Documented ModuleName)
moduleName = do cons <- conid `sepBy` char '.'
                let name = intercalate "." (map getid cons)
                return $ ModuleName NoDoc name

getModuleName :: Documented Module -> String
getModuleName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = name
getModuleName _ = error "This should never happen"

decl :: Doc -> BSParser [Documented Decl]
decl doc =  choice [ listed $ function doc
                   , listed $ instance_ doc
                   , listed $ class_ doc
                   , listed $ type_ doc
                   , listedPair $ data_ doc
                   , listedPair $ newtype_ doc
                   , lonelyComment
                   ]

listed :: BSParser a -> BSParser [a]
listed p = do result <- try p
              return [result]

listedPair :: BSParser (a, [a]) -> BSParser [a]
listedPair p = do (h, t) <- p
                  return (h:t)

lonelyComment :: BSParser [Documented Decl]
lonelyComment = try (docComment >> return [])

parseTypeMode :: Parser.ParseMode
parseTypeMode = Parser.ParseMode "" knownExtensions False False Nothing

parseType :: String -> BSParser (Documented Type)
parseType st = do     
                  let parseString = eliminateUnwanted st
                      -- Parse using haskell-src-exts
                      parsed = Parser.parseTypeWithMode parseTypeMode parseString
                  case parsed of
                    Parser.ParseFailed _ _ -> 
                      -- HACK: parsing of # fails, try to replace it and parse again
                      do let noHashString = theReplacements parseString 
                             parsed' = Parser.parseTypeWithMode parseTypeMode noHashString
                         case parsed' of
                           Parser.ParseFailed _ _ -> return $ TyVar NoDoc (Ident NoDoc "not parsed")
                           Parser.ParseOk ty -> return $ mapOnNames theInverseReplacements (document NoDoc ty)
                    Parser.ParseOk ty -> return $ document NoDoc ty

theReplacements :: (String -> String)
theReplacements = (replace "#" "__HASH__") . (replace "[:" "__GHC_ARR_OPEN__") . (replace ":]" "__GHC_ARR_CLOSE__")

theInverseReplacements :: (String -> String)
theInverseReplacements = (replace "__HASH__" "#") . (replace "__GHC_ARR_OPEN__" "[:") . (replace "__GHC_ARR_CLOSE__" ":]")

mapOnNames :: (String -> String) -> Documented Type -> Documented Type
mapOnNames f (TyForall doc vars context ty) = TyForall doc
                                                       (fmap (fmap (mapOnNamesTyVar f)) vars)
                                                       (fmap (mapOnNamesContext f) context)
                                                       (mapOnNames f ty)
mapOnNames f (TyFun doc t1 t2) = TyFun doc (mapOnNames f t1) (mapOnNames f t2)
mapOnNames f (TyTuple doc boxed tys) = TyTuple doc boxed (fmap (mapOnNames f) tys)
mapOnNames f (TyList doc ty) = TyList doc (mapOnNames f ty)
mapOnNames f (TyApp doc t1 t2) = TyApp doc (mapOnNames f t1) (mapOnNames f t2)
mapOnNames f (TyVar doc name) = TyVar doc (mapOnNamesName f name)
mapOnNames f (TyCon doc name) = TyCon doc (mapOnNamesQName f name)
mapOnNames f (TyParen doc ty) = TyParen doc (mapOnNames f ty)
mapOnNames f (TyInfix doc t1 name t2) = TyInfix doc (mapOnNames f t1) (mapOnNamesQName f name) (mapOnNames f t2)
mapOnNames f (TyKind doc ty k) = TyKind doc (mapOnNames f ty) k

mapOnNamesTyVar :: (String -> String) -> Documented TyVarBind -> Documented TyVarBind
mapOnNamesTyVar f (KindedVar doc name k) = KindedVar doc (mapOnNamesName f name) k
mapOnNamesTyVar f (UnkindedVar doc name) = UnkindedVar doc (mapOnNamesName f name)

mapOnNamesName :: (String -> String) -> Documented Name -> Documented Name
mapOnNamesName f (Ident doc s)  = Ident doc (f s)
mapOnNamesName f (Symbol doc s) = Symbol doc (f s)

mapOnNamesQName :: (String -> String) -> Documented QName -> Documented QName
mapOnNamesQName f (Qual doc mname name) = Qual doc mname (mapOnNamesName f name)
mapOnNamesQName f (UnQual doc name)     = UnQual doc (mapOnNamesName f name)
mapOnNamesQName _ q@(Special _ _)       = q

mapOnNamesContext :: (String -> String) -> Documented Context -> Documented Context
mapOnNamesContext f (CxSingle doc asst) = CxSingle doc (mapOnNamesAsst f asst)
mapOnNamesContext f (CxTuple doc assts) = CxTuple doc (fmap (mapOnNamesAsst f) assts)
mapOnNamesContext f (CxParen doc ctx)   = CxParen doc (mapOnNamesContext f ctx)         
mapOnNamesContext _ (CxEmpty doc)       = CxEmpty doc

mapOnNamesAsst :: (String -> String) -> Documented Asst -> Documented Asst
mapOnNamesAsst f (ClassA doc name tys) = ClassA doc (mapOnNamesQName f name) (fmap (mapOnNames f) tys)
mapOnNamesAsst f (InfixA doc ty1 name ty2) = InfixA doc (mapOnNames f ty1) (mapOnNamesQName f name) (mapOnNames f ty2)
mapOnNamesAsst f (IParam doc name ty) = IParam doc (mapOnNamesIPName f name) (mapOnNames f ty)
mapOnNamesAsst f (EqualP doc ty1 ty2) = EqualP doc (mapOnNames f ty1) (mapOnNames f ty2)

mapOnNamesIPName :: (String -> String) -> Documented IPName -> Documented IPName
mapOnNamesIPName f (IPDup doc s) = IPDup doc (f s)
mapOnNamesIPName f (IPLin doc s) = IPLin doc (f s)

-- HACK: Types with ! are not parsed by haskell-src-exts
-- HACK: Control characters (like EOF) may appear
-- HACK: {-# UNPACK #-} comments and greek letters may appear
-- HACK: Greek letters may appear
eliminateUnwanted :: String -> String
eliminateUnwanted "" = ""
eliminateUnwanted ('{':('-':('#':(' ':('U':('N':('P':('A':('C':('K':(' ':('#':('-':('}': xs)))))))))))))) = eliminateUnwanted xs
eliminateUnwanted (x:xs) | x == '!'    = eliminateUnwanted xs
                         | isControl x = eliminateUnwanted xs
                         | x == 'α'    = 'a' : (eliminateUnwanted xs)
                         | x == 'β'    = 'b' : (eliminateUnwanted xs)
                         | x == 'γ'    = 'c' : (eliminateUnwanted xs)
                         | x == 'δ'    = 'd' : (eliminateUnwanted xs)
                         | otherwise   = x : (eliminateUnwanted xs)

multipleNames :: BSParser (Documented Name) ->BSParser [Documented Name]
multipleNames p=sepBy1 p (try $ do
                        spaces0
                        char ','
                        spaces0)

functionLike :: BSParser (Documented Name) -> BSParser ([Documented Name], Documented Type)
functionLike p = do names <- choice [
                        (try $ do
                                char '(' 
                                ns<-multipleNames p
                                char ')'
                                return ns),
                        (multipleNames p)
                        ]
                    spaces0
                    string "::"
                    spaces0
                    rest <- restOfLine
                    ty <- parseType rest
                    return (names, ty)

function :: Doc -> BSParser (Documented Decl)
function doc = do (names, ty) <- functionLike varid
                  return $ TypeSig doc names ty

constructor :: Doc -> BSParser (Documented GadtDecl)
constructor doc = do (names, ty) <- functionLike conid
                     return $ GadtDecl doc (head names) ty

constructorOrFunction :: Doc -> BSParser (Either (Documented Decl) (Documented GadtDecl))
constructorOrFunction doc = do f <- function doc
                               return $ Left f
                            <|>
                            do c <- constructor doc
                               return $ Right c

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
                   -- HACK: in some Hoogle files things like [overlap ok] appear
                   optional $ try (do spaces0
                                      char '['
                                      many $ noneOf "]\r\n"
                                      char ']')
                   spaces1
                   rest <- restOfLine
                   ty' <- parseType rest
                   let (ctx, ty) = getContextAndType ty'
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
               ty <- parseType rest
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

-- Here we return not only the datatype or newtype,
-- but also functions around them, that are put
-- between constructors when using record syntax.
dataOrNewType :: String -> (Documented DataOrNew) -> Doc -> BSParser (Documented Decl, [Documented Decl])
dataOrNewType keyword dOrN doc = do string keyword
                                    spaces0
                                    rests <- many1 possibleKind
                                    let rest = concat $ map fst rests
                                        k = snd (last rests)
                                    {- rest <- many $ allButDoubleColon
                                    k <- optionMaybe (do string "::"
                                                         spaces0
                                                         kind) -}
                                    ty <- parseType rest
                                    let (ctx, hd) = typeToContextAndHead ty
                                    consAndFns <- many $ try (spacesOrEol0 >> documented constructorOrFunction)
                                    let (fns, cons) = divideConstructorAndFunctions consAndFns
                                    return $ (GDataDecl doc dOrN ctx hd k cons Nothing, fns)

divideConstructorAndFunctions :: [Either (Documented Decl) (Documented GadtDecl)] -> ([Documented Decl], [Documented GadtDecl])
divideConstructorAndFunctions []     = ([], [])
divideConstructorAndFunctions (x:xs) = let (fns, cons) = divideConstructorAndFunctions xs
                                       in  case x of
                                             Left fn   -> (fn:fns, cons)
                                             Right con -> (fns, con:cons)

possibleKind :: BSParser (String, Maybe (Documented Kind))
possibleKind = do rest <- many1 $ allButDoubleColon
                  k <- optionMaybe (do string "::"
                                       spaces0
                                       kind)
                  return (rest, k)

allButDoubleColon :: BSParser Char
allButDoubleColon = try (do char ':'
                            notFollowedBy $ char ':'
                            return ':')
                    <|> (noneOf ":\r\n")

data_ :: Doc -> BSParser (Documented Decl, [Documented Decl])
data_ = dataOrNewType "data" (DataType NoDoc)

newtype_ :: Doc -> BSParser (Documented Decl, [Documented Decl])
newtype_ = dataOrNewType "newtype" (NewType NoDoc)

dataOrNewTypeHead :: String -> (Documented DataOrNew) -> Doc -> BSParser (Documented Decl)
dataOrNewTypeHead keyword dOrN doc = do string keyword
                                        spaces0
                                        rests <- many1 possibleKind
                                        let rest = concat $ map fst rests
                                            k = snd (last rests)
                                        {- rest <- many $ allButDoubleColon
                                        k <- optionMaybe (do string "::"
                                                             spaces0
                                                             kind) -}
                                        ty <- parseType rest
                                        let (ctx, hd) = typeToContextAndHead ty
                                        return $ GDataDecl doc dOrN ctx hd k [] Nothing

dataHead :: Doc -> BSParser (Documented Decl)
dataHead = dataOrNewTypeHead "data" (DataType NoDoc)

newtypeHead :: Doc -> BSParser (Documented Decl)
newtypeHead = dataOrNewTypeHead "newtype" (NewType NoDoc)

class_ :: Doc -> BSParser (Documented Decl)
class_ doc = do string "class"
                spaces0
                rest <- many $ allButWhereColonPipe
                fd' <- optionMaybe (do string "|"
                                       spaces0
                                       iFunDep <- funDep
                                       rFunDep <- many $ try (spaces0 >> char ',' >> spaces0 >> funDep)
                                       return $ iFunDep:rFunDep)
                -- HACK: if a type family is introduced here, just discard it
                optional $ string "where" >> restOfLine
                -- HACK: in some Hoogle files, kinds are added to the class
                optional $ string "::" >> restOfLine
                ty <- parseType rest
                let (ctx, hd) = typeToContextAndHead ty
                    fd = maybe [] id fd'
                return $ ClassDecl doc ctx hd fd Nothing

allButWhereColonPipe :: BSParser Char
allButWhereColonPipe = try (do char ':'
                               notFollowedBy $ char ':'
                               return ':')
                        <|>
                        try (do char 'w'
                                notFollowedBy $ string "here"
                                return 'w')
                        <|> (noneOf "w|:\r\n")               

funDep :: BSParser (Documented FunDep)
funDep = do iVarLeft <- varid
            rVarLeft <- many $ try (spaces1 >> varid)
            spaces0
            string "->"
            spaces0
            iVarRight <- varid
            rVarRight <- many $ try (spaces1 >> varid)
            return $ FunDep NoDoc (iVarLeft:rVarLeft) (iVarRight:rVarRight)

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
                let var = initial:rest
                guard $ not (var `elem` haskellKeywords)
                return $ Ident NoDoc var)
        <|> 
--        try (do --initial <- oneOf (tail specialCharacters)
--                var <- many1 (oneOf specialCharacters)
--                --let var = initial:rest
--                guard $ not (var `elem` haskellReservedOps)
--                return $ Symbol NoDoc var)
--        <|>
        try (do string "()"
                return $ Symbol NoDoc "()")
        <|>
        try (do char '('
                s<-many1 (char ',')
                char ')'
                return $ Symbol NoDoc s)
        <|>
        try (do char '('
                var <- varid
                char ')'
                return var)
        <|>
        try (do var <- many1 (noneOf [',',')','(',' ','\r','\n','\t'])
                guard $ not (var `elem` haskellReservedOps)
                return $ Symbol NoDoc var)
 

conid :: BSParser (Documented Name)
conid = (do initial <- upper
            rest <- many $ alphaNum <|> oneOf allowedSpecialCharactersInIds
            return $ Ident NoDoc (initial:rest))
        <|> 
        try (do initial <- char ':'
                rest <- many (oneOf specialCharacters)
                let con = initial:rest
                guard $ not (con `elem` haskellReservedOps)
                return $ Symbol NoDoc con)
        <|>
        try (do char '('
                con <- conid
                char ')'
                return con)

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

eol :: BSParser String
eol =   try (string "\r\n")
    <|> try (string "\r")
    <|> string "\n"
    -- <|> (lookAhead eof >> return "\n")
    <?> "new line"

number :: BSParser Int
number = do n <- many1 digit
            return $ read n

spaces0 :: BSParser String
spaces0 = many $ char ' '

spaces1 :: BSParser String
spaces1 = many1 $ char ' '

spacesOrEol0 :: BSParser String
spacesOrEol0 = many $ oneOf " \r\n\t"

spacesOrEol1 :: BSParser String
spacesOrEol1 = many1 $ oneOf " \r\n\t"

-- working with types

getContextAndType :: (Documented Type) -> (Maybe (Documented Context), Documented Type)
getContextAndType (TyForall _ _ ctx ty) = (ctx, ty)
getContextAndType ty                    = (Nothing, ty)

lineariseType :: Documented Type -> [Documented Type]
lineariseType (TyApp _ x y) = (lineariseType x) ++ [y]
lineariseType ty            = [ty]

typeToContextAndHead :: (Documented Type) -> (Maybe (Documented Context), Documented DeclHead)
typeToContextAndHead t = let (ctx, ty) = getContextAndType t
                             (name,vars) = case lineariseType ty of
                                ((TyCon _ (UnQual _ name)):params) -> (name,toKindedVars params)  
                                ((TyCon _ (Qual _ _ name)):params) -> (name,toKindedVars params) 
                                ((TyCon _ (Special l _)):params) -> (Symbol l "",toKindedVars params)  
                         in  (ctx, DHead NoDoc name vars)

toKindedVars :: [Type Doc] -> [TyVarBind Doc]
toKindedVars []         = []
toKindedVars ((TyVar d (Ident _ n1)):( (TyList _ (TyVar _ (Ident _ n2))): xs )) =
  (UnkindedVar d (Ident NoDoc $ n1 ++ "[" ++ n2 ++ "]")) : toKindedVars xs
toKindedVars ((TyVar d n):xs) = (UnkindedVar d n) : toKindedVars xs
toKindedVars (x:_)            = error $ show x

