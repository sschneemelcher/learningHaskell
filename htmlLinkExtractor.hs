startOfLink :: String -> String
startOfLink document
    | document == "" = ""
    | head document == '\"' = ""
    | otherwise = [head document] ++ startOfLink (tail document)

extractLinks :: String -> [String]
extractLinks document
    | document == "" = [""]
    | take 6 document == "href=\"" = [startOfLink (drop 6 document)] ++ (extractLinks (tail document))
    | otherwise = extractLinks (tail document)

main :: IO ()
main = do
  print "Name of html file:"
  file <- getLine 
  contents <- readFile file
  print $ init $ extractLinks contents
