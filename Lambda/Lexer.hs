module Lambda.Lexer(Token(..),TokenPos, tokenize) where

import Text.ParserCombinators.Parsec hiding (token, tokens, space, spaces)
import Control.Applicative ((<*), (*>), (<$>), (<*>))
data Token = Let | Lambda | Id [Char] | Lparen | Rparen | Period | EOL deriving (Show, Eq)
type TokenPos = (Token, SourcePos)

idchars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    ++ ['+','-','/', '^', '$', '&', '*']


ide :: Parser TokenPos
ide = do
    pos <- getPosition
    fc  <- oneOf idchars
    r   <- optionMaybe (many $ oneOf idchars)
    spaces
    return $ flip (,) pos $ case r of
                 Nothing -> Id [fc]
                 Just s  -> Id $ [fc] ++ s

space :: Parser Char
space = oneOf [' ', '\t']

spaces = skipMany space

parsePos p = (,) <$> p <*> getPosition

lambda, period, assign, eol, lparen, rparen :: Parser TokenPos
period  = parsePos $ char '.' >> return Period
lambda  = parsePos $ (char '\\' <|> char 'λ') >> return Lambda
assign  = parsePos $ string ":=" >> return Let
eol     = parsePos $ char '\n' >> return EOL
lparen  = parsePos $ char '(' >> return Lparen
rparen  = parsePos $ char ')' >> return Rparen

token = choice [
    eol,
    assign,
    lambda,
    lparen, rparen,
    period,ide]

--tokens = spaces *> many1 (token <* spaces)
--tokens = spaces *> many1 (token <* spaces)
tokens = spaces *> many1 (token <* spaces) <* eof

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()
