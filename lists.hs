-- question 1
-- find the last element of a list
-- lastElement [1..3] => 3
lastElement :: [a] -> a
lastElement = head . reverse

-- question 2
-- find the last but one element of a list.
-- lastButOneElement [1..3] => 2
lastButOne :: [a] -> a
lastButOne [] = error "No elements"
lastButOne xs = (reverse xs) !! 2

-- question 3
-- find the nth element of a list
-- nth 2 [1..10] => 2
nth :: (Num a, Ord a) => a -> [b] -> b
nth n [] = 
    if n < 1
       then error "Illegal index"
       else error "No elements"
nth n (x:xs) = 
    if n == 1
       then x
       else nth (n - 1) xs

-- question 4
-- find the number of elements in a list
len' :: (Num a) => a -> [b] -> a
len' n [] = n
len' n (x:xs) = len' (n + 1) xs

len :: (Num b) => [a] -> b
len xs = len' 0 xs

-- question 5
-- reverse a list
rev' :: [a] -> [a] -> [a]
rev' [] ys = ys
rev' (x:xs) ys = rev' (xs) (x:ys)

rev :: [a] -> [a]
rev xs = rev' xs []

-- question 6
-- determine if is a palindrome
isPalindrome' :: (Eq a) => [a] -> [a] -> Bool
isPalindrome' [] [] = True
isPalindrome' (x:xs) (y:ys) = 
    if x == y
       then isPalindrome' xs ys
       else False

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = isPalindrome' xs (reverse xs)


