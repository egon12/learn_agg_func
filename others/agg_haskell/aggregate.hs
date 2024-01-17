import Data.Function ((&))
import Data.List (filter, group, groupBy, sort, sortBy)

data P = P
  { id :: Int
  , name :: String
  , age :: Int
  , salary :: Int
  , dept :: String
  } deriving (Show)

peoples :: [P]
peoples =
  [ P 1 "Egon" 50 1000 "Finance"
  , P 2 "Gilbert" 50 2000 "Finance"
  , P 3 "Nova" 37 1500 "HR"
  , P 4 "Ryan" 78 1800 "HR"
  , P 5 "Gone" 10 400 "Tech"
  ]

main :: IO ()
main = do
  -- show list
  putStrLn "show list"
  print (peoples)
  -- show all name
  {-
  putStrLn ""
  putStrLn "show all names"
  print (map name peoples)
  -- sum all salary
  putStrLn ""
  putStrLn "sum all salary"
  print (sum (map salary peoples))
  -- without parenthesis
  let mapSalary = map salary

  peoples 
    & mapSalary
    & sum 
    & print

  -- show  all name for people with age > 40
  putStrLn ""
  putStrLn "show all names for people with age > 40"
  peoples & filter (\person -> age person > 40) & map name & print
  -- sum salary for people with age > 40
  -}
  putStrLn ""
  putStrLn "sum salary for people with age > 40"
  peoples & filter (\person -> age person > 40) & map salary & sum & print
  peoples & sumSalaryWhenAgeAbove 40 & print
  

-- another function
ageAbove :: Int -> [P] -> [P]
ageAbove age_input = filter (\person -> age person > age_input)

sumSalary :: [P] -> Int
sumSalary = sum . map salary

sumSalaryWhenAgeAbove :: Int -> [P] -> Int
sumSalaryWhenAgeAbove age_input = sumSalary . ageAbove age_input
