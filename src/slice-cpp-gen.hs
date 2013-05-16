{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Main (main) where

import           Data.Monoid ((<>),mempty,mconcat)
import qualified Data.ByteString as BS
import           Data.Char (toUpper)
import           Data.List (intercalate)
import           Data.String (fromString)
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
                             } deriving (Show, CA.Data, CA.Typeable)
                                
defaultArgs = CppGenArgs { icefile   = CA.def CA.&= CA.args CA.&= CA.typ "INPUTFILE"
                         , targetDir = ""
                         , overwrite = False
                         , cpp98     = False
                         }

-- generate cpp skeletons
sliceCppGen :: FilePath -> FilePath -> Bool -> AST.SliceDecl -> [(FilePath,BS.ByteString)]
sliceCppGen icefile trgtdir cpp98 decl = go [] decl
  where
    override = if cpp98 then "" else " override"
    go ns (ModuleDecl mName decls)     = concatMap (go (ns++[mName])) decls
    go ns (InterfaceDecl nm ext mthds) = [ (trgtdir </> nm ++ "I.h",   genH   ns nm mthds)
                                         , (trgtdir </> nm ++ "I.cpp", genCpp ns nm mthds)]
    go ns _                            = []
    
    genH :: [String] -> String -> [MethodDecl] -> BS.ByteString
    genH   ns nm mthds = let hname = replaceExtension (takeFileName icefile) ".h"
                             classCont = \indent -> genClass indent nm mthds
                             ctnt      =  genInclds [hname] <> genNs "" ns classCont
                         in genIfDef (ns ++ [nm]) ctnt
    
    genCpp ns nm mthds = let mthdsCont = \indent -> genMethods indent nm mthds
                         in genInclds [trgtdir </> nm ++ "I.h"] <> genNs "" ns mthdsCont
    
    genIfDef tkns ctnt = let ident = fromString $ intercalate "_" $ map (map toUpper) tkns ++ ["H"]
                         in ("#ifndef " <> ident <>"\n#define " <> ident <> "\n\n") <> ctnt <> "\n#endif"
                            
    genInclds xs = mconcat (map (\x -> "#include <" <> fromString x <> ">\n") xs) <> "\n"
    
    genNs :: String -> [String] -> (String -> BS.ByteString) -> BS.ByteString
    genNs indent (ns:nss) cont = let indent' = fromString indent 
                                 in (indent' <> "namespace " <> fromString ns <> "\n" <> indent' <> "{\n") 
                                    <> genNs ('\t':indent) nss cont <> (indent' <> "};\n")
    genNs indent []       cont = cont indent
    
    genClass :: String -> String -> [MethodDecl] -> BS.ByteString
    genClass indent nm  mthds = let indent' = fromString indent
                                    nm'     = fromString nm
                                in (indent' <> "class " <> nm' <> "I: public " <> nm' <> "\n" <> indent' <> "{\n" <> indent'<> "public:\n") 
                                   <> genMethodHeads ('\t':indent) mthds <> (indent' <> "};\n")
                                   
    genMethods indent nm mthds = let indent' = fromString indent
                                     nm'     = fromString nm
                                 in indent' <> 
                                    BS.intercalate ("\n" <> indent' <> "{\n" <> indent' <> "}\n\n" <> indent') (map (genMethodHead (nm' <> "I::")) mthds) 
                                    <> "\n" <> indent' <> "{\n" <> indent' <> "}\n"
    
    genMethodHeads indent mthds = fromString indent <> BS.intercalate (override <> ";\n" <> fromString indent) (map (genMethodHead "") mthds) <> override <> ";\n"
    
    genMethodHead :: BS.ByteString -> MethodDecl -> BS.ByteString
    genMethodHead scope (MethodDecl tp nm flds _ _) = genType tp <> " " <> scope <> fromString nm <> "(" <> genFields flds <> (if null flds then "" else ", ") <> "const Ice::Current& current)"
    
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


main = CA.cmdArgs defaultArgs >>= \args@(CppGenArgs icef trgtD ovrw cpp98') -> do
  slcData <- BS.readFile icef
  createDirectoryIfMissing True trgtD
  case SlcP.parseSlice slcData of
    Left  err -> putStrLn (err ++ "\nexiting...") >> exitFailure
    Right []  -> putStrLn ("Parsing '" ++ icef ++ "' didn't produce any output, probably because the Slice parser is deficient.\nTo improve it, please report your slice file to paul.koerbitz@gmail.com") 
                 >> exitFailure 
    Right ast -> do
      let fileData = concatMap (sliceCppGen icef trgtD cpp98') ast
          wrtr     = \(fn,ctnt) -> do putStrLn $ "generating '" ++ fn ++ "'"
                                      (BS.writeFile fn ctnt)
          chkwrtr  = if ovrw 
                       then wrtr
                       else \(fn,ctnt) -> do p <- doesFileExist fn
                                             if p 
                                               then putStrLn ('\'' : fn ++ "' aready exists. To overwrite use '--overwrite=True'")
                                               else wrtr (fn,ctnt)
      mapM_ chkwrtr fileData
