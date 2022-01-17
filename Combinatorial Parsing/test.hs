import JSONParser (JSON, parseJSON)

main = do
    rawText <- readFile "test_input.json"
    let parsed = parseJSON rawText

    case parsed of
        Just x -> print x
        Nothing -> print "Error parsing JSON"