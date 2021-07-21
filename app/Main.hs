{-# LANGUAGE RecordWildCards #-}

module Main where

import Language.Haskell.Exts (Module (..),
                              ImportDecl(..),
                              ModuleName(..),
                              ImportSpecList (ImportSpecList), 
                              parseFileWithExts, 
                              Extension(..), 
                              KnownExtension (..), 
                              parseFileContentsWithExts)
import Language.Haskell.Exts.Parser (ParseResult(..))
import System.Environment (getArgs)
import Control.Monad (forM_)
import Text.ParserCombinators.ReadP (string,
                                    satisfy,
                                    many,
                                    skipMany,
                                    get,
                                    (<++),
                                    ReadP,
                                    readP_to_S)
import Data.Char (isAlphaNum)
import Data.List (isInfixOf)
import Data.Foldable (foldl')


main :: IO ()
main = do
    files <- getArgs
    let exts = EnableExtension <$> extensions 
    forM_ files $ \file -> do
        retryOrResult <- hasGoodImports <$> parseFileWithExts exts file
        case retryOrResult of
            Left _ -> handlePostQualified exts file 
            Right result -> print result 


importFromModule :: ParseResult (Module l) -> Either (IO ()) [ImportDecl l]
importFromModule pmod = case pmod of
    -- the error handling here should be better 
    -- and not have IO invade 
    -- have too much of a headache to fix this 
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
        manyImports -> return $ Left manyImports 


-- for fixing ImportQualifiedPost 
-- this is not good please fix this better if you know how 
modname :: ReadP Char
modname = satisfy isAlphaNum <++ satisfy (=='.')

lineMatch :: ReadP String 
lineMatch = do
    _ <- string "import "
    x <- many modname
    _ <- string " qualified as "
    y <- many modname
    skipMany get
    return $ "import qualified " <> x <> " as " <> y

-- filter to get as much consumption as possible
-- the last is just a heuristic and is too shotgunny
-- this will probably blow up
getCorrectMatch :: String -> Either String String  
getCorrectMatch line = case filter (\(_, y) -> y == "") $ readP_to_S lineMatch line of
    [] -> Left line
    xs -> Right $ fst $ last xs


handlePostQualified :: [Extension] -> FilePath -> IO ()
handlePostQualified exts fileName = do
    contents <- readFile fileName
    -- surely there's a better way to do this error handling 
    let newContents = reassembleFiles fileName $ lines contents {-flip concatMap (lines contents) $ \line ->
         if "qualified as" `isInfixOf` line then case getCorrectMatch line of
                Left errline -> error $  "Error handling ImportPostQualified, with file " <> fileName <> " and line " <> line
                Right correctLine -> correctLine 
         else line -} 
    case hasGoodImports $ parseFileContentsWithExts exts newContents of
        Left x -> x
        Right x -> print x
        
-- fileName is exclusively for error handling 
-- used foldl' because of guessing there might be performance issues
-- coming from reassembling lazily 
-- I don't know that foldl' ameliorates that for sure but it seems more likely than not
-- The error handling could probably be removed? but then whenever it fails, if it fails, you'll want to put it back in 
reassembleFiles :: FilePath -> [String] -> String
reassembleFiles fileName fileLines = foldl' (++) [] $ transformLine <$> fileLines 
  where 
      transformLine line = if " qualified as " `isInfixOf` line 
                           then case getCorrectMatch line of 
                               Left _ -> error $  "Error handling ImportPostQualified, with file " <> fileName <> " and line " <> line
                               Right correctLine -> correctLine 
                           else line 

-- maybe fine tuning this gets more things to compile?
-- this is how hlint does it 
-- except they by default turn offextensions they consider to be 
-- "really bad" 
extensions :: [KnownExtension]
extensions = [minBound..maxBound]