module Main where

import Lexer
import Parser
import Codegen
import Typechecker
import Types

import LLVM.Pretty (ppllvm)
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Map as Map

main :: IO()
main = do
    contents <- Prelude.readFile "hello.c"
    let parserOutput = runParser parseFile (ParserState (assignKeywords (tokenize contents)) VoidType [] Map.empty Map.empty)
    case parserOutput of
        Left err -> error err
        Right (ast, (ParserState _ _ globals _ _)) -> do
            let ast' = globals ++ ast
            let typechecked = typecheckFile ast'
            let llvm = unpack (BS.concat (BL.toChunks(TLE.encodeUtf8 (ppllvm (codegenFile typechecked)))))
            Prelude.writeFile "hello.ll" (T.unpack (TE.decodeUtf8 (BS.pack llvm)))
            print ast