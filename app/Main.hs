{-# LANGUAGE RecordWildCards #-}


module Main where

import Language.Haskell.Exts (parseFile,
                              Module (..),
                              ImportDecl(..),
                              ModuleName(..),
                              ImportSpecList (ImportSpecList), knownExtensions, parseFileWithExts, Extension(..), KnownExtension (TypeOperators), SrcInfo (fileName), parseFileContentsWithExts)
import Language.Haskell.Exts.Parser (ParseResult(..))
import System.Environment ( getArgs )
import Control.Monad (forM_, (<=<), (>=>))
import Text.ParserCombinators.ReadP (string, 
                                    satisfy, 
                                    many, 
                                    skipMany, 
                                    get, 
                                    (<++), 
                                    ReadP, 
                                    readP_to_S)
import Data.Char (isAlpha)
import Data.List (isSubsequenceOf)


main :: IO ()
main = do
    files <- getArgs
    let exts = EnableExtension <$> [TypeOperators]
    forM_ files $ \x -> do
        res <- hasGoodImports <$> parseFileWithExts exts x
        case res of
            Left _ -> handlePostQualified exts x
            Right x -> print x


importFromModule :: ParseResult (Module l) -> Either (IO ()) [ImportDecl l]
importFromModule pmod = case pmod of
    -- the error handling here should be better 
    -- and not have IO invade 
    (ParseFailed err _) -> Left $ print $ "Parsing of file failed: " <> show err
    (ParseOk XmlPage{}) -> Left $ print "Not expecting Xml"
    (ParseOk XmlHybrid{}) -> Left $ print "Not expecting XmlHybrid"
    (ParseOk (Module _ _ _ importdecls _)) -> Right importdecls

-- this really should be changed but unfortunately ModuleName is taken by imports, which have srcspaninfo
newtype ModuleTitle = ModuleTitle { getModuleTitle :: String } deriving (Eq, Show)
newtype Qualified = Qualified { getQualified :: Bool } deriving (Eq, Show)

-- n.b. I'm using "hidden" to mean `import F hiding (a)` this does not count `import F hiding ()`
newtype MassImporting = MassImporting { getMassImporting :: Bool } deriving (Eq, Show)

removeUnnec :: ImportDecl l -> (ModuleTitle, Qualified, MassImporting)
removeUnnec ImportDecl{..} = (ModuleTitle $ getName importModule, Qualified importQualified, MassImporting $ massImporting importSpecs)
  where
    getName :: ModuleName l -> String
    getName (ModuleName _ s) = s

    massImporting Nothing = True
    massImporting (Just (ImportSpecList _ False _)) = False
    massImporting (Just (ImportSpecList _ True _)) = True

-- empty list means no offending imports 
-- if there's just one then it's fine 
-- if there's two or more...not good
findOffendingImports :: [(ModuleTitle, Qualified, MassImporting)] -> [ModuleTitle]
findOffendingImports imports = go imports []
  where
      -- would be neat if someone could turn this into a fold
      go :: [(ModuleTitle, Qualified, MassImporting)] -> [ModuleTitle] -> [ModuleTitle]
      go [] bads = bads
      go (importDatum@(name, _, _):importData) bads = if isGood importDatum then go importData bads else go importData (name:bads)
      -- if it's qualified or hidden, it's fine 
      isGood (_, Qualified b, MassImporting b2) = b || not b2

-- contract: when Left xs, length xs > 1 
hasGoodImports :: ParseResult (Module l) -> Either (IO ()) (Either [ModuleTitle] ())
hasGoodImports pmod = do
      imports <- importFromModule pmod
      case findOffendingImports $ removeUnnec <$> imports of
        [] -> return $ Right ()
        [_] -> return $  Right ()
        imports -> return $ Left imports


-- for fixing ImportQualifiedPost 
-- this is not good please fix this better if you know how 
modname :: ReadP Char
modname = satisfy isAlpha <++ satisfy (=='.')

lineMatch = do
    string "import "
    x <- many modname
    string " qualified as "
    y <- many modname
    skipMany get
    return $ "import qualified " <> x <> " as " <> y

-- filter to get as much consumption as possible
-- the last is just a heuristic and is too shotgunny
-- this will probably blow up 
getCorrectMatch line = case filter (\(_, y) -> y == "") $ readP_to_S lineMatch line of
    [] -> Left $ line
    xs -> Right $ fst $ last xs
handlePostQualified :: [Extension] -> FilePath -> IO ()
handlePostQualified exts fileName = do
    contents <- readFile fileName
    -- surely there's a better way to do this error handling 
    let newContents = flip concatMap (lines contents) $ \line ->
         if "qualified as" `isSubsequenceOf` line then case getCorrectMatch line of
                Left errline -> error $  "Error handling ImportPostQualified, with file " <> fileName <> " and line " <> line
                Right correctLine -> correctLine
         else line
    case hasGoodImports $ parseFileContentsWithExts exts newContents of
        Left x -> x
        Right x -> print x