
import Data.Char


-- Learn You A Haskell

-- CHAPTER 1 - INTRO

-- Division returns a float in haskell
-- Booleans are either True or False
-- The not equal operator is: /=
-- Haskell natively supports very large integers
-- Prefix operator: not True == True

-- Functions have higher precedence than mathematical operators 
-- Therefore max 3 5 * 2 == (max 3 5) * 2 == 10

-- If you want to make a prefix operator an infix operator use the `:
-- Therefore max 3 5 == 3 `max` 5

-- CHAPTER 2 - FUNCTIONS

-- Functions (cannot begin with uppercase letters):
hypotenuse a b = sqrt (a^2 + b^2)

-- If we want to load a haskell file we can use :load FileName   >>> abbreviated to :l FileName

-- An if statement always needs an else, otherwise the output woudl be undefined.
testIf x = if x == 1
	then "that's a 1"
	else "that's not a 1"

-- Use let statements to define functions "on the fly", ex. let triple x = x*3
-- We can also do this to define simple values, ex. let x = 4.2

-- We can define lists as such
numbers = [1, 2, 3]

-- Head list, gives first element
-- Tail list, gives rest of list (which is also a list)

-- Haskell data structures are IMMUTABLE. They cannot change.


-- The colon operator is used to construct a new list node >>> 5 : [] == [5]
-- Can do this with longer lists as follows, 1 : 2 : 3 : [] == [1, 2, 3]


-- We can therefore take elements of a list to create a new list, 99 : (tail firstList) >>> this adds 99 to the head of the list
-- Since lists are repesented by linked lists, we don't have to recopy each element to get a list with a new head as we would have to with an array.


-- Array functins:
-- length numbers >>> O(n) since we need to go through the whole list to find out it's length.
-- reverse numbers >>> O(n) since we need to create new nodes
-- numbers !! 2 >>> O(n) since we need an element at the nth position (walk down the list)
-- last numbers >>> O(n) since we need to walk down the list to get to the last position
-- init numbers >>> O(n) we get everything BUT the last element, linear time (we need to rebuild all of the nodes)
	-- The second last node needs to be new (doesn't have same pointer as last), then so does previous, and previous, and so on.
-- null numbers >>> determines if a list is empty
-- elem 15 numbers >>> O(n) tells us if an element is in the list
-- list1 ++ list2 >>> O(n) in the length of the first list (this needs to be rebuilt since last node points to head of second list now)


-- "adam" < "ant" == True >>> check the lexicographical order of words. Since strings are list of characters, this is a really a comparison on a list.
	-- This takes O(n) time since it goes through each element until there is a difference


-- Math operations on lists: maximum, minimum, sum, product >>> these are all O(n)
-- Can create long lists by: sum [1..100], this creates a list from 1 to 100 and sums it.
	-- This also works for characters, ['f'..'h'] == 'fgh'
	-- Increment by a different type of number, [2, 4..10] == [2,4,6,8,10]
	-- [1..] creates an infinitely long list >> since haskell is lazy this is okay


-- List comprehensions:
-- Get all powers of two, 		[2^n | n <- [1..10]]
	-- We have defined what the list is and we give the list an input. This is 2^n where n is in the range of 1->10 		>>> very mathematical syntax

-- Adding additional constraints [2^n | n <- [1..20], 2^n > 1000 || 2^n / 2 == 32]		>>> will print 64 and all powers of two higher than 1000

-- The first | is the vertical pipe symbol since we pipe in n as the input.
-- Strings are lists so we can remove all of the vowels from a word:
--		[x | x <-"this is used to have vowels", not (elem x "aeiou")]
--		[x | x <-"this is used to have vowels", not (x `elem` "aeiou")]			We have made this operation infix to make it more obvious.
--		This is saying that the new value x is made up of the characters of the string literal given, and for each character it is not an element of the list "aeiou"
-- Lets make this a function.

noVowels :: String -> String
noVowels x = [new | new <- x, not (elem new "aeiou")]

noVowelsList :: [String] -> [String]
noVowelsList x = [[new | new <- word, not (new `elem` "aeiou")] | word <- x]


-- Haskell is very concise, it requires much less code to perform certain tasks than other languages.
-- For instance, a multiplication table.

multTable :: Integer -> [[Integer]]
multTable n = [[x * y | y <- [1..n]] | x <- [1..n]]
-- We break down a complicated expression into nested parts.
-- In the inner part we multiple x by y for a range of y, and in the outer part we define the range of x with which to perform this inner comprehension.




-- Tuples: (1, 2)
-- The length of a tuple is apart of it's type >>> doesn't make sense to compare tuples of different sizes.
-- The elements of a tuple need not be of the same type.
	-- A tuple is like a struct/object therefore, collecting various objects of different types.
tupleExample = ("This", "tuple", "is", "number", 1)

-- Pair functions:
	-- fst gives first element, snd gives second element
	-- zip makes two lists into a list of pairs (first elements are pairs, and second element are pairs, and so on..)

-- Problem, numbers 1-8, when does the smaller number have a longer word.

numInt = [1..8]
numWord = ["one", "two", "three", "four", "five", "six", "seven", "eight"]
together = zip numInt numWord

findLessLength :: [(Integer, String)] -> [(Integer, Integer)]
findLessLength allWords = [(fst p, fst q) | p <- allWords, q <- allWords, fst p < fst q, length (snd p) > length (snd q)]


-- So what we have done is first gotten every possible pair of words.
-- We then filter this to only show the pairs with the first number being less.
-- We next finally filter to only show the pairs with the first numbers word being longer than the second.


-- Example: Find letter that is a vowel and has an ascii code that is even


lowerCase = ['a'..'z']

findOrder :: Char -> Int
findOrder c = ord(c)

findVowelAndEven :: [Char] -> [Char]
findVowelAndEven ch = [c | c <- ch, (findOrder(c) `mod` 2) == 0, c `elem` "aeiou"]
-- It turns out that there aren't any but I am starting to understand hwo you need to think to code in Haskell.
-- The idea of filtering is striking me as fundamental.



-- 	CHAPTER 3 - TYPES

-- Typeclasses: sets of types
-- 		 Types: sets of values

-- For instance, the type Integer includes 3, 4, 99, 201
-- while the typeclass Num includes Integer, Int, Float, Double

-- Use :t to find the type of a value
-- type names and type class names begin with uppercase letters.

-- If we try to do :t 3 we get
--		3 :: Num a => a
-- Since 3 is a member of every type in the Num typeclass
-- Haskell is being lazy and not committing to a specific type yet since it does not need to.
-- When we bind the value to a name, Haskell needs to commit to a type.

-- We can also see the type of functions
-- Example the type of head is:
-- 		head :: [a] -> a
-- Since it takes in an array of any type and returns one element of that type

-- We can do this with the + operator as well,
--		(+) :: Num a => a -> a -> a
-- There is a Num type a that it takes in. It takes in two of these and produces a third of the same type.

-- :t zip
--		zip :: [a] -> [b] -> [(a, b)]
-- Takes a list of a and a's list of b's, returns a list of a b pairs.


-- Haskell is smart in figuring out which types are needed; however, we can also specify the type of a function along with it's definition.
typeEx :: [Int] -> Int 				-- This line therefore is not necessary; however, it is useful to know which types we are expecting (documentation and bug finding).
typeEx x = head x + length x


dividesEvenly :: Int -> Int -> Bool
dividesEvenly x y = (y `div` x) * x == y
-- The / operator only works on fractionals so we cannot use it here with inputs of type Int.
-- The operator we really want is div, we use it here as an infix.
-- By specifying the type we are therein able to find bugs that would have possibly been overlooked.

-- Int,		 is bounded
-- Integer,	 is unbounded
-- Float,	 single precision
-- Double,	 is double precision
-- Eq, 	     constraint means that either == or /= is used on a variable
-- Ord,	 	 constraint used for types that have an ordering (< <= > >=)
-- Show, 	 can be presented as strings
-- Read, 	 is the opposite of Show, takesa string and returns a Read (takes a string and makes it into a non-string)
				-- example: read "True" || False == True, sicne the True is read as a Bool (destringifies)
				-- read :: (Read a) => String -> a 
					-- returns a tyep that is apart of read, but does not specifically define the type
-- Enum, 	 are ordered type, can be enumerated >> can use it's types in list ranges such as [3 .. 5], or we can get the next element such as succ 'B' = 'C' (successor)
-- Bounded,  have upper and lower bound (minBound and maxBound functions) >> (Bounded a) => a
-- Integral, only whole numbers, this is a member of Num and contains Int and Integer
				-- fromIntegral :: (Num b, Integral a) => a -> b
				-- Takes an Integral number and turns it into a more general number, useful for working with floating and integral numbers together.
-- Floating, contains Float and Double


-- 	CHAPTER 4 - FUNCTION SYNTAX

increasing :: (Ord a) => [a] -> Bool
increasing x = if x == []
	then True
	else if tail x == []
		then True
		else if head x <= head (tail x)
			then increasing (tail x)
			else False

-- Using recursion, base case is an empty list or list with 1 element is always increasing.
-- We get the second element of the list by taking the head of the tail.

increasing2 :: (Ord a) => [a] -> Bool
increasing2 [] = True
increasing2 [a] = True
increasing2 (x:y:ys) = x <= y && increasing(y:ys)
-- We define the bases cases, then say that for other cases the list must have a first, second, and third (containg the rest of the items) item.
-- We are making use of the way that Haskell reads in parameters here.
-- Parenthese required when matching several thigns at once.

-- In the below example we use pattern matching again.
-- We can avoid using head and tail since the pattern matcher has already broken the word into pieces.
noVowels2 :: [Char] -> [Char]
noVowels2 "" = ""
noVowels2 (x:ys) = if x `elem` "aeiouAEIOU"
	then noVowels2 ys
	else x : noVowels2 ys


noVowels3 :: [Char] -> [Char]
noVowels3 "" = ""
noVowels3 (x:ys)
	| x `elem` "aeiouAEIOU" = noVowels3 ys
	| otherwise             = x : noVowels3 ys
-- This is an alternatie way to write this function, pipes with cases define the diferent types of behaviours.
-- PIPES USED FOR IF-ELSE STATEMENTS


convertToString :: Int -> [Char]
convertToString n = show n ++ " we add the case: " ++ message n
    where message 7 = "Special Case" 
          message _ = "Normal case" 
-- Where statements used for CASES
-- !!CAUTION!! the where statement cares about whitespaces, so tabs are not acceptable.




convertToString2 :: Int -> [Char]
convertToString2 n = show n ++ " we add the case: " ++ case n of 7 -> "Special case"
                                                                 _ -> "Normal case"
-- This is a case statement that has a similar problem --> need whitespace instead of tabs



gravity :: (Fractional a) => a -> a
gravity r = 6.674e-11 * 5.972e24 / (r^2)

-- Make clearer with a let expression:
gravity2 :: (Fractional a) => a -> a
gravity2 r = let g = 6.674e-11
                 earthMass =5.972e24
             in g * earthMass / (r^2)
-- In this situation as well white space plays an important role. 
-- Let binding keeps scope specific to the function (local)



-- CHAPTER 5 - RECURSION

lengthFxn :: [a] -> Int
lengthFxn [] = 0
lengthFxn x = 1 + lengthFxn (tail x)
-- lengthFxn (_:x) = 1 + lengthFxn x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:y) = x + sum(y)

max' :: (Ord a) => [a] -> a
max' [] = error "maximum of empty list"		-- Errors in Haskell
max' [x] = x
max' (x:xs)
    | x > mx    = x
    | otherwise = mx
	where mx = max' xs


-- CHAPTER 6 - HIGHER ORDER FUNCTIONS

-- Functions that theythemself can take functions as agruements.

add1 :: Int -> Int
add1 x = x + 1

f :: (Int -> Int) -> Int
f x = (x 3) + 3

-- We can therefor pass add1 to f and have it evaluated within f.
-- Similarily, we can also return functions


-- A useful higer order functions is map >>> it takes a function and a list and applies the funtion to each element fo the list returning a new list.
tuplize :: Int -> (Int, Int)
tuplize x = (x, x)
-- In this case map tuplize [1, 2, 3] would return [(1,1),(2,2),(3,3)]
-- Map allows us to remove a lot of redundant code.

-- When we call functions with multiple variables the function really only uses one variable at a time.
-- add1 x y really first returns add1 x and applies this function to y
-- So type definitions are right associative.
-- We could actually store this returned value and then apply it.
--		let stored = add1 x
--		stored y 				Yields the same result as add1 x y

-- WAY TO CALL SAME FUNCTION WITH SAME PARTIAL PARAMETERS
--		If a parameter was going to stay the same this saves time.

-- Example map (max 3) [1, 2, 3, 4, 5] == [3, 3, 3, 4, 5], since it takes the max of 3 and another value, which is being mapped to it one at a time.

-- We can do currying in such a way then using infix operators:
-- 		map (/10) [1, 2, 3, 4, 5] >> number divided by 10
-- 		map (10/) [1, 2, 3, 4, 5] >> 10 divided by number


-- zipWith is a higher order function that can do the following:
--		zipWith (+) [1, 2, 3] [4, 5, 6] >> [5, 7, 9]

-- filter takes a function and a list, returning the elements for which the function returns true.
--		filter (<4)[1..300] >> [1, 2, 3]

-- takeWhile takea function and a list returns while a function is satisfied
--		takeWhile (<100)[1..] >> returns numbers from 1 to 99





-- Lambda expressions allow us to define functions "on the fly"
-- For isntance, the add1 function is equivalent to (\x -> x + 1)
-- So we could do something like: map (\x -> x + 1) [1, 2, 3]				We treat the backslash like it is a lambda
-- Functions without names (anon functions)

-- foldl (+) 0 [1, 2, 3]
-- This function sums the numbers, starting at 0
-- foldl (*) 1 [1, 2, 3]
-- This functions defines the product, starting at 1

	-- We could also do foldr to start from the end of a list.

	-- foldl (-) 0 [1, 2, 3] >>> 0 + (1 + (2 + (3)))
	-- foldr (-) 0 [1, 2, 3] >>> 1 - (2 - (3 - (0)))

