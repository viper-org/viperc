module Main where

import Lexer
import Parser
import Codegen

import LLVM.Pretty (ppllvm)
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main :: IO()
main = do
    contents <- Prelude.readFile "hello.c"
    let parserOutput = runParser parseFile (ParserState (assignKeywords (tokenize contents)))
    case parserOutput of
        Left err -> print err
        Right (ast, state) -> do
            let llvm = unpack (BS.concat (BL.toChunks(TLE.encodeUtf8 (ppllvm (codegenFile ast)))))
            Prelude.writeFile "hello.ll" (T.unpack (TE.decodeUtf8 (BS.pack llvm)))
            print ast