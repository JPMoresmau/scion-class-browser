{-# LANGUAGE TemplateHaskell #-}

module Scion.Browser.Parser.Stage2
( convertHoogleToPackage
) where

import Control.DeepSeq
import qualified Control.Exception as E
import Control.Monad.State
import Data.Char (isUpper)
import Data.DeriveTH
import Data.List (last)
import Data.Maybe (fromJust, isNothing)
import Scion.Browser
import Scion.Browser.Parser.Stage1
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec.Error (newErrorMessage, Message(..))
import Text.ParserCombinators.Parsec.Pos (newPos)

data ConversionState = CS (Maybe String)        -- current documentation
                          (Maybe (Doc Package)) -- current package
                          (Maybe [String])      -- current module name
                          (Maybe (Doc Module))  -- current module
                          (Maybe (Doc Item))    -- current item
                          String                -- file name



convertHoogleToPackage :: String -> [HoogleItem] -> Either ParseError (Doc Package)
convertHoogleToPackage fname items = 
  case fst $ runState (cHToP items) (emptyConversionState fname) of
    Nothing  -> Left (newErrorMessage (Message "error converting file") (newPos fname 0 0))
    Just pkg -> Right (deepseq pkg pkg)

cHToP :: [HoogleItem] -> State ConversionState (Maybe (Doc Package))
cHToP []         = do saveItem
                      saveModule
                      CS _ p _ _ _ _ <- get
                      return p
-- documentation
cHToP ((D d):is) = do put_ pDoc d
                      cHToP is
-- package
cHToP ((P p):is) = do doc <- get_ gDoc
                      empty_ pDoc
                      put_ pPkg $ Doc doc (Package p "" [])
                      cHToP is
-- version
cHToP ((V v):is) = do Doc doc (Package p _ m) <- get_ $ gPkg ("version " ++ v)
                      put_ pPkg $ Doc doc (Package p v m)
                      cHToP is
-- module
cHToP ((M m):is) = do saveItem
                      saveModule
                      doc <- get_ gDoc
                      empty_ pDoc
                      put_ pModName $ m
                      put_ pMod $ Doc doc (Module (last m) [] [])
                      cHToP is
-- items
cHToP ((I i):is) = do doc <- get_ gDoc
                      empty_ pDoc
                      case i of
                        Function n t ->
                          if isUpper (head n)
                             then addConstructor doc n t
                             else do saveItem
                                     put_ pItem $ Doc doc (Function n t)
                                     saveItem
                        otherwise ->
                          do saveItem
                             case i of
                               Data n p _ k pr      -> put_ pItem $ Doc doc (Data n p [] k pr)
                               Newtype n p _ k pr   -> put_ pItem $ Doc doc (Newtype n p [] k pr)
                               Class n p pr k fd tf -> do put_ pItem $ Doc doc (Class n p pr k fd tf)
                                                          saveItem
                               Instance d           -> do put_ pItem $ Doc doc (Instance d)
                                                          saveItem
                               Type n p d           -> do put_ pItem $ Doc doc (Type n p d)
                                                          saveItem
                      cHToP is
-- this should not happen
cHToP (_:is)     = cHToP is


-- Helper functions

saveModule :: State ConversionState ()
saveModule = do empty <- isEmpty_ eMod
                if empty
                   then return ()
                   else do Doc d p <- get_ $ gPkg "*not possible*"
                           mod     <- get_ $ gMod "*not possible*"
                           modName <- get_ $ gModName "*not possible*"
                           put_ pPkg $ Doc d p { pkgModules = addModuleToTree (pkgModules p) mod modName }
                           empty_ pMod

addModuleToTree :: [Doc Module] -> Doc Module -> [String] -> [Doc Module]
addModuleToTree mods mod [_]    = mods ++ [mod]
addModuleToTree mods mod (x:xs) = case findModule mods x of
                                    Just (Doc d m) ->
                                      replaceModule mods (Doc d m { modModules = (addModuleToTree (modModules m) mod xs) })
                                    Nothing -> 
                                      let newMods = addModuleToTree [] mod xs
                                      in  mods ++ [Doc "" (Module x [] newMods)]

findModule :: [Doc Module] -> String -> Maybe (Doc Module)
findModule []                            _                = Nothing
findModule (m@(Doc d (Module n _ _)):xs) name | n == name = Just m
                                              | otherwise = findModule xs name

replaceModule :: [Doc Module] -> Doc Module -> [Doc Module]
replaceModule [] mod = [mod]
replaceModule (m1@(Doc _ (Module n1 _ _)):xs) m2@(Doc _ (Module n2 _ _)) =
  if n1 == n2
     then m2:xs
     else m1:(replaceModule xs m2)

addConstructor :: String -> String -> String -> State ConversionState ()
addConstructor doc name ty = do e <- isEmpty_ eItem
                                if e 
                                   then return ()   -- HACK: fail silently if no item is available
                                   else do Doc d item <- get_ $ gItem ("constructor " ++ name)
                                           let con = Doc doc (DataConstructor name ty)
                                           let item' = item { dataConstructors = (dataConstructors item) ++ [con] }
                                           put_ pItem $ Doc d item'

saveItem :: State ConversionState ()
saveItem = do empty <- isEmpty_ eItem
              if empty
                 then return ()
                 else do Doc d (Module n is ms) <- get_ $ gMod "*not possible*"
                         item <- get_ $ gItem "*not possible*"
                         put_ pMod $ Doc d (Module n (item:is) ms) 
                         empty_ pItem

-- Functions for easier access to state

get_ :: (ConversionState -> a) -> State ConversionState a
get_ f = do s <- get
            return (f s)

isEmpty_ :: (ConversionState -> Maybe a) -> State ConversionState Bool
isEmpty_ f = do s <- get
                return (isNothing (f s))

put_ :: (ConversionState -> Maybe a -> ConversionState) -> a -> State ConversionState ()
put_ f x = do s <- get
              put $ f s (Just x)

empty_ :: (ConversionState -> Maybe a -> ConversionState) -> State ConversionState ()
empty_ f = do s <- get
              put $ f s Nothing

emptyConversionState :: String -> ConversionState
emptyConversionState fname = CS Nothing Nothing Nothing Nothing Nothing fname

gDoc :: ConversionState -> String
gDoc (CS Nothing  _ _ _ _ _) = ""
gDoc (CS (Just d) _ _ _ _ _) = d

pDoc :: ConversionState -> Maybe String -> ConversionState
pDoc (CS _ p mn m i f) d = CS d p mn m i f

gPkg :: String -> ConversionState -> Doc Package
gPkg w (CS _ p _ _ _ f) = case p of
                            Nothing -> error $ "gPkg: Nothing in " ++ w ++ " file: " ++ f
                            Just p' -> p'

pPkg :: ConversionState -> Maybe (Doc Package) -> ConversionState
pPkg (CS d _ mn m i f) p = CS d p mn m i f

gModName :: String -> ConversionState -> [String]
gModName w (CS _ _ mn _ _ f) = case mn of
                                 Nothing  -> error $ "gModName:  Nothing in " ++ w ++ " file: " ++ f
                                 Just mn' -> mn'

pModName :: ConversionState -> Maybe [String] -> ConversionState
pModName (CS d p _ m i f) mn = CS d p mn m i f

gMod :: String -> ConversionState -> Doc Module
gMod w (CS _ _ _ m _ f) = case m of
                           Nothing -> error $ "gMod:  Nothing in " ++ w ++ " file: " ++ f
                           Just m' -> m'

eMod :: ConversionState -> Maybe (Doc Module)
eMod (CS _ _ _ m _ _) = m

pMod :: ConversionState -> Maybe (Doc Module) -> ConversionState
pMod (CS d p mn _ i f) m = CS d p mn m i f

gItem :: String -> ConversionState -> Doc Item
gItem w (CS _ _ _ _ i f) = case i of
                             Nothing -> error $ "gItem:  Nothing in " ++ w ++ " file: " ++ f
                             Just i' -> i'

eItem :: ConversionState -> Maybe (Doc Item)
eItem (CS _ _ _ _ i _) = i

pItem :: ConversionState -> Maybe (Doc Item) -> ConversionState
pItem (CS d p mn m _ f) i = CS d p mn m i f

