import System.IO


main = do
  s <- getContents
  let l = lines s
  let l2 = last l
  putStrLn . dropWhiteSpace $ l2


dropWhiteSpace [] = []
dropWhiteSpace (' ':xs) = dropWhiteSpace xs
dropWhiteSpace (x:xs)   = x:xs
