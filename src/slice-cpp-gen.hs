{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Main (main) where

import           Data.Monoid ((<>),mempty,mconcat)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BLB
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
                             } deriving (Show, CA.Data, CA.Typeable)
                                
defaultArgs = CppGenArgs { icefile   = CA.def CA.&= CA.args CA.&= CA.typ "INPUTFILE"
                         , targetDir = ""
                         , overwrite = False
                         }

-- generate cpp skeletons
sliceCppGen :: FilePath -> FilePath -> AST.SliceDecl -> [(FilePath,BL.ByteString)]
sliceCppGen icefile trgtdir decl = go [] decl
  where
    go ns (ModuleDecl mName decls)     = concatMap (go (ns++[mName])) decls
    go ns (InterfaceDecl nm ext mthds) = [ (trgtdir </> nm ++ "I.h",   BLB.toLazyByteString $ genH   ns nm mthds)
                                         , (trgtdir </> nm ++ "I.cpp", BLB.toLazyByteString $ genCpp ns nm mthds)]
    go ns _                            = []
    
    genH :: [String] -> String -> [MethodDecl] -> BLB.Builder
    genH   ns nm mthds = let hname = replaceExtension (takeFileName icefile) ".h"
                             classCont = \indent -> genClass indent nm mthds
                             ctnt      =  genInclds [hname] <> genNs "" ns classCont
                         in genIfDef (ns ++ [nm]) ctnt
    
    genCpp ns nm mthds = let mthdsCont = \indent -> genMethods indent nm mthds
                         in genInclds [trgtdir </> nm ++ "I.h"] <> genNs "" ns mthdsCont
    
    genIfDef tkns ctnt = let ident = fromString $ intercalate "_" $ map (map toUpper) tkns ++ ["H"]
                         in BLB.byteString ("#ifndef " <> ident <>"\n#define " <> ident <> "\n\n") <> 
                            ctnt <> BLB.byteString "\n#endif"
                            
    genInclds xs = mconcat (map (\x -> BLB.byteString $ "#include <" <> fromString x <> ">\n") xs) <>  BLB.byteString "\n"
    
    genNs :: String -> [String] -> (String -> BLB.Builder) -> BLB.Builder
    genNs indent (ns:nss) cont = BLB.byteString (fromString indent <> "namespace " <> fromString ns <> "\n" <> fromString indent <> "{\n") <>
                                 genNs ('\t':indent) nss cont <>
                                 BLB.byteString (fromString indent <> "};\n")
    genNs indent []       cont = cont indent
    
    genClass :: String -> String -> [MethodDecl] -> BLB.Builder
    genClass indent nm  mthds = let indent' = fromString indent
                                    nm'     = fromString nm
                                in BLB.byteString $ 
                                   indent' <> "class " <> nm' <> "I: public " <> nm' <> "\n" <> indent' <> "{\n" <> indent'<> "public:\n" <>
                                   genMethodHeads ('\t':indent) mthds <>
                                   indent' <> "};\n"
                                   
    genMethods indent nm mthds = let indent' = fromString indent
                                     nm'     = fromString nm
                                 in BLB.byteString $ indent' <> 
                                                     BS.intercalate ("\n" <> indent' <> "{\n" <> indent' <> "}\n\n" <> indent') (map (genMethodHead (nm' <> "I::")) mthds) <>
                                                     ("\n" <> indent' <> "{\n" <> indent' <> "}\n")
    
    genMethodHeads indent mthds = fromString indent <> BS.intercalate (";\n" <> fromString indent) (map (genMethodHead "") mthds) <> ";\n"
    
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


main = CA.cmdArgs defaultArgs >>= \args@(CppGenArgs icef trgtD ovrw) -> do
  slcData <- BS.readFile icef
  createDirectoryIfMissing True trgtD
  case SlcP.parseSlice slcData of
    Left  err -> putStrLn (err ++ "\nexiting...") >> exitFailure
    Right ast -> do
      let fileData = concatMap (sliceCppGen icef trgtD) ast
          wrtr     = if ovrw 
                     then uncurry BL.writeFile 
                     else \(fn,ctnt) -> do p <- doesFileExist fn
                                           putStrLn $ '\'' : fn ++ "' aready exists. To overwrite use --overwrite=True"
                                           if p then return () else (BL.writeFile fn ctnt)
      mapM_ wrtr fileData
