module JSONParser (JSON, parseJSON) where

--imports
import Parsing
import Control.Applicative
import qualified Data.Map as M

-- JSON data type
data JSON =
      JObject (M.Map String JSON)
    | JArray [JSON]
    | JString String
    | JInt Int
    | JFloat Float
    | JBool Bool
    | JNull
    deriving (Eq, Show)

-- JSON parser function for public use
parseJSON :: String -> Maybe JSON
parseJSON inp = case parse json inp of
   Nothing -> Nothing
   Just (x, "") -> Just x
   Just (_, _) -> Nothing 

-- parsers
json :: Parser JSON
json = jElement

jValue :: Parser JSON
jValue = jObject <|> jArray <|> jString <|> jNumber <|> jBool <|> jNull

jObject :: Parser JSON
jObject = do char '{' >> space >> char '}'
             return (JObject M.empty)
          <|>
          do char '{'
             members <- jMembers
             char '}'
             return (JObject (M.fromList members))

jMembers :: Parser [(String, JSON)]
jMembers = repeatParser jMember ','

jMember :: Parser (String, JSON)
jMember = do space
             key <- jText
             space
             char ':'
             val <- jElement
             return (key, val)

jArray :: Parser JSON
jArray = do char '['
            space
            char ']'
            return $ JArray []
         <|>
         do char '['
            jElems <- jElements
            char ']'
            return $ JArray jElems

jElements :: Parser [JSON]
jElements = repeatParser jElement ','

jElement :: Parser JSON
jElement = do space
              jVal <- jValue
              space
              return jVal

jString :: Parser JSON
jString = JString <$> jText

jText :: Parser String
jText = do char '"'
           s <- many $ parseIf (/= '"')
           char '"'
           return s

jNumber :: Parser JSON
jNumber = do i <- integer
             f <- jFraction
             e <- jExponent
             return $ JFloat (parseFE i f e)
          <|>
          do i <- integer 
             f <- jFraction
             return $ JFloat (stitch i f)
          <|>
          do i <- integer 
             e <- jExponent
             return $ JInt (parseIE i e)
          <|>
          do i <- integer 
             return (JInt i)

jFraction :: Parser Int
jFraction = char '.' >> natural

jExponent :: Parser Int
jExponent = (char 'E' <|> char 'e') >> integer

jNull :: Parser JSON
jNull = string "null" >> return JNull

jBool :: Parser JSON
jBool = (string "true" >> return (JBool True))
        <|>
        (string "false" >> return (JBool False))

-- utility functions
stitch :: Int -> Int -> Float
stitch i f = read $ show i ++ "." ++ show f

parseFE :: Int -> Int -> Int -> Float
parseFE i f e = stitch i f * (10^e)

parseIE :: Int -> Int -> Int
parseIE i e = i * (10^e)

repeatParser :: Parser a -> Char -> Parser [a]
repeatParser parser sep = do item <- parser
                             char sep
                             items <- repeatParser parser sep
                             return (item:items)
                          <|>
                          do item <- parser
                             return [item]