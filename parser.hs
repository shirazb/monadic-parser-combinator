{-
  Monadic parser combinator based on http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf.
  Paper by Graham Hutton and Eric Meijer.

  Shiraz Butt
-}

type Parser a = String -> [(a, String)]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Primitive Parsers

result :: a -> Parser a
-- The parser 'result v' consumes none of the input and has v as the parsed section
result v
  = \inp -> [(v, inp)]

zero :: Parser a
-- Always returns failure
zero
  = \inp -> []

item :: Parser Char
-- The item parser parses the first character of the input, or fails if given an already failed parse (empty list)
item
  = \inp -> case inp of
      []      -> []
      (x:xs)  -> [(x, xs)]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Parser Combinators

seq' :: Parser a -> Parser b -> Parser (a, b)
-- Returns a parser that performs the sequencing of the two given parsers,
-- returning the result of the two parses as a pair
p `seq'` q
  = \inp -> [ ((v, w), inp'') | (v, inp') <- p inp, (w, inp'') <- q inp' ]

bind :: Parser a -> (a -> Parser b) -> Parser b
-- Binds two parsers together: Gets the result of the first parse and passes it
-- to a function that does the second pass, returning the overall result
p `bind` f
  = \inp -> concat [ f v inp' | (v, inp') <- p inp ]

seq :: Parser a -> Parser b -> Parser (a, b)
-- Rewrite of seq using bind
p `seq` q
  =  p `bind` (\x ->
     q `bind` (\y ->
     result (x, y)))

sat :: (Char -> Bool) -> Parser Char
-- Parser that consumes a single character iff it satisfies the predicate
sat p
  = item `bind` (\x -> if p x then result x else zero)

char :: Char -> Parser Char
-- Parser that consumes a single char iff it matches the given char x
char x
  = sat (\y -> x == y)
