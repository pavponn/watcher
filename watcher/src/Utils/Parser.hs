module Utils.Parser
  ( Parser(..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , splitArguments
  ) where

import Control.Applicative
import Data.Char (isSpace)

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f p =  Parser fun where
    fun dat =
      case (runParser p dat) of
        (Just (a, dat')) -> Just (f a, dat')
        Nothing         -> Nothing

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)
  pf <*> pa =
    Parser $ \dat -> case (runParser pf dat) of
      Just (f, dat') -> runParser (f <$> pa) dat'
      Nothing        -> Nothing

instance Alternative (Parser s) where
  empty     = Parser $ \_  -> Nothing
  pf <|> pa =
    Parser $ \dat ->
      let res = runParser pf dat
      in case res of
        Just _  -> res
        Nothing -> runParser pa dat

instance Monad (Parser s) where
  return = pure
  pa >>= pf =
    Parser $ \dat -> case runParser pa dat of
      Just (a, dat') -> runParser (pf a) dat'
      Nothing        -> Nothing

-- |Parser that never fails and doesn't conusme input.
ok :: Parser s ()
ok = Parser $ \dat -> Just ((), dat)

-- |Parser that checks whether it's the end of the input.
eof :: Parser s ()
eof = Parser $ \dat -> case dat of
  [] -> Just ((), [])
  _  -> Nothing

-- |Parser that checks whether stream's element satisfies predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy pr = Parser $ \dat -> case dat of
  []     -> Nothing
  (x:xs) -> if pr x then Just (x, xs) else Nothing

-- |Parser that checks wheter stream's element is equal to specified element.
element :: (Eq s) => s -> Parser s s
element e = satisfy (== e)

-- |Parser that checks wheter stream's element is not equal to specified element.
notElement :: (Eq s) => s -> Parser s s
notElement e = satisfy (not . (==) e)

-- |Parser that checks wheter stream's elements are equal to specified elements.
stream :: (Eq s) => [s] -> Parser s [s]
stream st = case st of
  []     -> return []
  (x:xs) -> pure (:) <*> element x <*> stream xs

-- | Split command line arguments.
splitArguments :: String -> Maybe [String]
splitArguments str =
  case runParser input str of
    Just (res, _) -> Just res
    Nothing       -> Nothing

argInQuotes :: Parser Char String
argInQuotes = many (notElement '\"')

quote :: Parser Char String
quote = (element '\"') *> argInQuotes <* (element '\"')

textChar :: Parser Char Char
textChar = satisfy (\c -> not (isSpace c) && not (c == '\"'))

text :: Parser Char String
text = some textChar

spaces :: Parser Char ()
spaces = many (satisfy isSpace) *> ok

input :: Parser Char [String]
input = many (spaces *> text <|> spaces*> quote) <* spaces <* eof
