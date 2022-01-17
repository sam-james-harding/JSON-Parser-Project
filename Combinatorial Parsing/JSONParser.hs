module JSONParser (JSON, parseJSON) where

--imports
import Parsing
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
jObject = do char '{'
             space
             char '}'
             return $ JObject M.empty
          <|>
          do char '{'
             members <- jMembers
             char '}'
             return $ JObject (M.fromList members)

jMembers :: Parser [(String, JSON)]
jMembers = do member <- jMember
              char ','
              members <- jMembers
              return (member:members)
           <|>
           do member <- jMember
              return [member]

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
jElements = do jElem <- jElement
               char ','
               jElems <- jElements
               return (jElem:jElems)
            <|>
            do jElem <- jElement
               return [jElem]

jElement :: Parser JSON
jElement = do space
              jVal <- jValue
              space
              return jVal

jString :: Parser JSON
jString = JString <$> jText

jText :: Parser String
jText = do char '"'
           char '"'
           return ""
        <|>
        do char '"'
           s <- some nonQuoteChar
           char '"'
           return s
        where nonQuoteChar = parseIf (/= '"')

jNumber :: Parser JSON
jNumber = do i <- integer
             f <- jFraction
             e <- jExponent
             return $ JFloat (parseFloatExp i f e)
          <|>
          do i <- integer 
             f <- jFraction
             return $ JFloat (stitchNumber i f)
          <|>
          do i <- integer 
             e <- jExponent
             return $ JInt (parseIntExp i e)
          <|>
          do i <- integer 
             return (JInt i)
    where
        stitchNumber :: Int -> Int -> Float
        stitchNumber i f = read $ show i ++ "." ++ show f

        parseFloatExp i f e = stitchNumber i f * (10^e)
        parseIntExp i e = i * (10^e)

jFraction :: Parser Int
jFraction = do char '.'
               natural

jExponent :: Parser Int
jExponent = do char 'E' <|> char 'e'
               integer

jNull :: Parser JSON
jNull = do string "null"
           return JNull

jBool :: Parser JSON
jBool = do string "true"
           return $ JBool True
        <|>
        do string "false"
           return $ JBool False