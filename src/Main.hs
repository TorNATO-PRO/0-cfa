module Main where

import Syntax.Parser (parseLambdaExpr)
import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Data.Text (unpack)
import Syntax.Ast (runLabel, varFacts, appFacts, lambdaFacts, LabelError (UnboundVariable))
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> processFile filePath
    _          -> putStrLn "Usage: cfa <path-to-lambda-file>"

processFile :: FilePath -> IO ()
processFile fp = do
  input <- T.readFile fp
  case parseLambdaExpr fp input of
    Left  (src, err) -> do
      putStrLn $ "Parse error in " ++ unpack src ++ ":"
      putStrLn err
    Right ast -> do
      let
        labeledASTE = runLabel ast
      
      case labeledASTE of
        Left (UnboundVariable err) ->
          putStrLn $ "Error encountered while labeling AST: " <> err
        Right labeledAST -> do
          let        
            vFacts = varFacts labeledAST
            aFacts = appFacts labeledAST
            lFacts = lambdaFacts labeledAST

            writeFacts name facts = do
              let outFile = name <> ".facts" 
              T.writeFile outFile
                ( T.unlines facts )
              
              putStrLn $ "Wrote " <> show (length facts) <> " facts to " <> outFile
          
          putStrLn "Successfully parsed lambda expression, now writing fact files:"
          writeFacts "Var" vFacts
          writeFacts "App" aFacts
          writeFacts "Lambda" lFacts
