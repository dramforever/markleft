{-# LANGUAGE NoMonomorphismRestriction #-}
{- MKL for MarKLeft -}

module Markleft where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*))
import Data.List
import System.Exit

type MKLDocs = [MKLDoc]

data MKLDoc = MKLText String
            | MKLMarco String [MKLDocs]
            deriving Show

type MKLMT = [( String, ( [String] -> String ) )]

ident = many1 $ noneOf "{"
text  = ( many1 $ noneOf "#}") >>= return.MKLText

marco = do char '#'
           name <- ident
           arg <- many $ do char '{'
                            a <- document
                            char '}'
                            return a

           return $ MKLMarco name arg

document = do res <- many $ marco <|> text
              return res

makeDocs = parse ( document <* eof ) "(input)"

renderDoc :: MKLMT -> MKLDoc -> String
renderDoc _ (MKLText a) = a

renderDoc table (MKLMarco name args) =
  case lookup name table of
    Just func -> func rArgs
    Nothing ->
      "#" ++ name ++ "{" ++ (intercalate "}{" rArgs) ++ "}"
  where rArgs = map (renderDocs table) args

renderDocs t = concat . (map (renderDoc t))

std_table = [ ( "concat", concat ) ]

main = do let test = "#concat{a}{b#concat{c}{d}}"
              Right d = makeDocs test
              out = renderDocs std_table d

          putStrLn out
