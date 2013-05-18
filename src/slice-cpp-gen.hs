{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>),mempty,mconcat)
import qualified Data.ByteString as BS
import           Data.Char (toUpper,isUpper)
import           Data.List (intercalate)
import           Data.String (fromString)
import           Data.String.Utils (endswith)
import qualified System.Console.CmdArgs as CA
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           System.Exit (exitSuccess, exitFailure)
import           System.FilePath.Posix (replaceExtension, takeFileName, (</>))

import qualified Language.Slice.Syntax.Parser as SlcP
import           Language.Slice.Syntax.AST    as AST

data CppGenArgs = CppGenArgs { icefile   :: FilePath
                             , targetDir :: FilePath
                             , overwrite :: Bool
                             , cpp98     :: Bool
                             , longguard :: Bool
                             , fwdFctMthds :: Bool
                             } deriving (Show, CA.Data, CA.Typeable)
                                
defaultArgs = CppGenArgs { icefile   = CA.def CA.&= CA.args CA.&= CA.typ "INPUTFILE"
                         , targetDir = ""
                         , overwrite = False
                         , cpp98     = False
                         , longguard = False
                         , fwdFctMthds = True
                         }

camelSplit :: String -> [String]
camelSplit s = let (wrd,lst) = foldl (\(wrd,lst) c -> if isUpper c 
                                                      then ([c],if null wrd then lst else reverse wrd:lst) 
                                                      else (c:wrd,lst)) ([],[]) s
               in reverse (reverse wrd:lst)

replaceTail :: String -> String -> String -> String
replaceTail what with target = let n              = length what
                                   m              = length target - n
                                   (thead, ttail) = splitAt m target
                               in if ttail == what then thead ++ with else target
    
-- namespaces and names are in reverse order (e.g ["InterfaceName", "InnerNamespace", "OuterNamespace"])
findInterfaces :: [String] -> AST.SliceDecl -> Map.Map [String] AST.SliceDecl
findInterfaces ns (ModuleDecl nm decls)    = Map.unions $ map (findInterfaces (nm:ns)) decls
findInterfaces ns i@(InterfaceDecl nm _ _) = Map.singleton (nm:ns) i
findInterfaces _  _                        = Map.empty

findIfcsWFactory :: AST.SliceDecl -> Map.Map [String] AST.SliceDecl
findIfcsWFactory ast = Map.foldlWithKey (\acc k v -> if pred k then Map.insert (reverse k) v acc else acc) Map.empty interfaces
  where
    interfaces = findInterfaces [] ast
    pred k   = let k' = (head k ++ "Factory") : tail k in Map.member k' interfaces
    
findFctyMthds :: [SliceDecl] -> Map.Map [String] [MethodDecl]
findFctyMthds asts = mapMaybeReverseKey (\k v -> mFctyMthds k) interfaces
  where
    interfaces = Map.unions . map (findInterfaces []) $ asts
    
    isFcty k = endswith "Factory" (head k) && Map.member (replaceTail "Factory" "" (head k) : tail k) interfaces
    
    mFctyMthds k = if isFcty k 
                   then Map.lookup k interfaces >>= \(InterfaceDecl _ _ mthds) -> Just mthds
                   else Nothing
                                              
    mapMaybeReverseKey :: ([String] -> a -> Maybe b) -> Map.Map [String] a -> Map.Map [String] b 
    mapMaybeReverseKey f m = go Map.empty (Map.toList m)
      where
        go acc []         = acc
        go acc ((k,v):xs) = case f k v of (Just x) -> go (Map.insert (reverse k) x acc) xs
                                          Nothing  -> go acc xs
    
sliceCppGen :: Map.Map [String] [MethodDecl] -> CppGenArgs -> SliceDecl -> [(FilePath,BS.ByteString)]
sliceCppGen fctyMthds args decl = go [] decl
  where
    go ns (ModuleDecl mName decls)     = concatMap (go (ns++[mName])) decls
    go ns (InterfaceDecl nm ext mthds) = [ (targetDir args </> nm ++ "I.h",   genH   ns nm mthds)
                                         , (targetDir args </> nm ++ "I.cpp", genCpp ns nm mthds)]
    go ns _                            = []
    
    genH :: [String] -> String -> [MethodDecl] -> BS.ByteString
    genH ns nm mthds = let staticMthds = fromMaybe [] $ Map.lookup (ns ++ [nm++"Factory"]) fctyMthds
                           hnames      = [replaceExtension (takeFileName $ icefile args) ".h"]
                           classCont   = \indent -> genClass indent nm mthds staticMthds
                           ctnt        =  genInclds hnames <> genNs "" ns classCont
                           guardItems  = if longguard args then ns ++ [nm] else [nm]
                       in genIfDef guardItems ctnt
    
    genCpp ns nm mthds = let fwdMthds    = fromMaybe [] $ Map.lookup (ns++[nm]) fctyMthds
                             staticMthds = fromMaybe [] $ Map.lookup (ns ++ [nm++"Factory"]) fctyMthds
                             mthdsCont   = \indent -> genMethods indent nm (mthds ++ staticMthds) fwdMthds
                             hnames      = (targetDir args </> nm ++ "I.h")
                                           : if null fwdMthds then [] else [targetDir args </> replaceTail "Factory" "" nm ++ "I.h"]
                         in genInclds hnames <> genNs "" ns mthdsCont
                            
    genIfDef tkns ctnt = let ident = fromString $ intercalate "_" $ map (map toUpper) (concatMap camelSplit tkns) ++ ["I","H"]
                         in ("#ifndef " <> ident <>"\n#define " <> ident <> "\n\n") <> ctnt <> "\n#endif"
                            
    genInclds xs = mconcat (map (\x -> "#include <" <> fromString x <> ">\n") xs) <> "\n"
    
    genNs :: String -> [String] -> (String -> BS.ByteString) -> BS.ByteString
    genNs indent (ns:nss) cont = let indent' = fromString indent 
                                 in indent' <> "namespace " <> fromString ns <> "\n" <> indent' <> "{\n"
                                    <> genNs ('\t':indent) nss cont <> indent' <> "};\n"
    genNs indent []       cont = cont indent
    
    genClass :: String -> String -> [MethodDecl] -> [MethodDecl] -> BS.ByteString
    genClass indent nm  mthds staticMthds = let indent'  = fromString indent
                                                nm'      = fromString nm
                                                override = if cpp98 args then "" else " override"
                                            in indent' <> "class " <> nm' <> "I: public " <> nm' <> "\n" <> indent' <> "{\n" <> indent'<> "public:\n" 
                                               <> genMethodHeads override ('\t':indent) mthds  
                                               <> (if null staticMthds then "" 
                                                   else "\n" <> genMethodHeads "" ('\t':indent ++ "static ") staticMthds)
                                               <> indent' <> "};\n"
                                                                                                                                              
                                   
    genMethods indent nm mthds fwdMthds = let indent'  = fromString indent
                                              nm'      = fromString nm
                                              bodyStub = "\n" <> indent' <> "{\n" <> indent' <> "}\n\n" <> indent'
                                          in if null fwdMthds
                                             then indent' <> (BS.intercalate bodyStub (map (genMethodHead (nm' <> "I::")) mthds) 
                                                   <> "\n" <> indent' <> "{\n" <> indent' <> "}\n") 
                                             else BS.intercalate "\n" (map (genFwdMethod indent' nm) fwdMthds)
    
    genMethodHeads override indent mthds = fromString indent <> BS.intercalate (override <> ";\n" <> fromString indent) (map (genMethodHead "") mthds) <> override <> ";\n"
    
    genMethodHead :: BS.ByteString -> MethodDecl -> BS.ByteString
    genMethodHead scope (MethodDecl tp nm flds _ _) = genType tp <> " " <> scope <> fromString nm <> "(" <> genFields flds <> (if null flds then "" else ", ") <> "const Ice::Current& current)"
    
    fs = fromString
    
    genFwdMethod indent scope mdecl@(MethodDecl tp nm flds _ _) = 
      let scope' = fs $ replaceTail "Factory" "" scope
          vals   = map (\(FieldDecl _ fnm _) -> fs fnm) flds ++ ["current"]
          ret    = if tp == STVoid then "" else "return "
      in indent <> genMethodHead (fs scope <> "I::") mdecl <> "\n" <> indent <> "{\n" <> indent <> "\t" <>
         ret <> scope' <> "I::" <> fs nm <> "(" <> BS.intercalate ", " vals <> ");\n" <> indent <> "}\n"
    
    genField (FieldDecl tp nm _) = passRefOrVal tp <> " " <> fromString nm
    
    genFields flds = BS.intercalate ", " $ map genField flds
    
    genType :: SliceType -> BS.ByteString
    genType STVoid                = "void"
    genType STBool                = "bool"
    genType STByte                = "Ice::Byte"
    genType STShort               = "Ice::Short"
    genType STInt                 = "Ice::Int"
    genType STLong                = "Ice::Long"
    genType STFloat               = "float"
    genType STDouble              = "double"
    genType STString              = "std::string"
    genType (STUserDefined nm)    = fromString nm
    genType (STUserDefinedPrx nm) = fromString nm <> "Prx"
    
    passRefOrVal tp@(STString)        = "const " <> genType tp <> "&"
    passRefOrVal tp@(STUserDefined _) = "const " <> genType tp <> "&"
    passRefOrVal tp                   = genType tp


main = CA.cmdArgs defaultArgs >>= \args@(CppGenArgs icef trgtD ovrw _ _ _) -> do
  slcData <- BS.readFile icef
  createDirectoryIfMissing True trgtD
  case SlcP.parseSlice slcData of
    Left  err -> putStrLn (err ++ "\nexiting...") >> exitFailure
    Right []  -> putStrLn ("Parsing '" ++ icef ++ "' didn't produce any output, probably because the Slice parser is deficient.\nTo improve it, please report your slice file to paul.koerbitz@gmail.com") 
                 >> exitFailure 
    Right asts -> do
      let fctyMthds = if fwdFctMthds args then findFctyMthds asts else Map.empty
          fileData = concatMap (sliceCppGen fctyMthds args) asts
          wrtr     = \(fn,ctnt) -> do putStrLn $ "generating '" ++ fn ++ "'"
                                      (BS.writeFile fn ctnt)
          chkwrtr  = if ovrw 
                       then wrtr
                       else \(fn,ctnt) -> do p <- doesFileExist fn
                                             if p 
                                               then putStrLn ('\'' : fn ++ "' aready exists. To overwrite use '--overwrite=True'")
                                               else wrtr (fn,ctnt)
      mapM_ chkwrtr fileData