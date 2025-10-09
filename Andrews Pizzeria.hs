-- Pizzeria Mamma Mia
substr :: String -> Int -> Int -> String
substr str start end = take (end - start) (drop start str)

-- Find
find :: [Char] -> Char -> Int
find (x:xs) char = if x == char then 0 else 1 + find xs char

-- List out all the elements of a list in one string
listToString :: [String] -> String
listToString [] = ""
listToString [x] = x
listToString [x, y] = x ++ " and " ++ y
listToString (x:xs) = x ++ ", " ++ listToString xs

-- Cook a delicious pizza
createPizza :: Int -> String -> [String] -> String
createPizza size pizza_type [] = show(size) ++ "' " ++ pizza_type ++ " Pizza"
createPizza size pizza_type ingredients = show(size) ++ "' " ++ pizza_type ++ " Pizza with " ++ listToString ingredients

-- Calculate price decided by the pizza size, 20 dollars for a 12 inch
calculatePrice :: [Char] -> Float
calculatePrice pizza = do
    let pizza_size = read (substr pizza 0 (find pizza '\'')) :: Float in pizza_size * 20 / 12


main :: IO ()
main = do
    let pizza = createPizza 24 "Meat" ["pepperoni", "salami", "cheese"]
    putStrLn(show(find pizza '\''))
    putStrLn ("The " ++ pizza ++ " costs " ++ show(calculatePrice pizza) ++ "$")