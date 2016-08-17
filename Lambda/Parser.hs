module Lambda.Parser(parserTest,lparse) where

import Text.Parsec as P
import Control.Monad.Identity
import Lambda.Stmt
import Lambda.Lexer

type Parser e = Parsec [TokenPos] () e


parserTest  :: Show a => Parser a -> String -> IO ()
parserTest p s =
    case tokenize "test" s of
        Left e    -> putStrLn $ show e
        Right ts' -> P.parseTest p ts'

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

satisfy :: (TokenPos -> Bool) -> Parser Token
satisfy f = tokenPrim show
                      advance
                      (\c -> if f c then Just (fst c) else Nothing)

tok :: Token -> ParsecT [TokenPos] () Identity Token
tok t = (Lambda.Parser.satisfy $ (== t) . fst) <?> show t

tok_identifier ::Parser Token
tok_identifier = (Lambda.Parser.satisfy $ (identifier') . fst) <?> show (Id "<name>")
    where
        identifier' :: Token -> Bool
        identifier' s  = case s of Id _ -> True
                                   _    -> False

str = do
    i <- tok_identifier
    return $ case i of Id name -> name
var = do
    i <- tok_identifier
    return $ case i of Id name -> V name

funct = do
    tok Lambda
    v <- str
    tok Period
    b <- expr
    return $ L v b

assign = do
    v <- str
    tok Let
    e <- expr
    return $ (v, e)

parens = (tok Lparen) *> expr <* (tok Rparen)

term = parens <|> funct <|> var

expr = do
    es <- many1 term
    return (foldl1 A es)


exprs = many (assign <* (optional $ (many $ tok EOL)))

lparse f s = case tokenize f s of
    Left e -> Left e
    Right t -> runParser exprs () f t

