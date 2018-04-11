import System.Environment
import System.IO
import Utilities
-- Want vector instead of list. Qualified because many of the function names for
-- Vector and List operations are the same. So use a short prefix (V) for those
import qualified Data.Vector as V
import Data.Char
import Debug.Trace
import Control.Monad
import Data.List

-- Just a constant (size of tape)
memorySize = 30000

-- |A "record" data type with two fields
data Memory = Memory { memoryData :: V.Vector Word
                     , dataPointer :: Int
                     -- Can be printed/turned into a string. Haskell generates
                     -- the actual specialization automatically
                     } deriving (Show)

-- There are no "methods", just functions. There's constructors for every data
-- type, but I want to do something different than just provide values for each
-- field, so I have to make a new function.

-- |Create a Memory with the given tape size and initial pointer 0
createMemory :: Int -> Memory
createMemory size = Memory {memoryData=(V.replicate size 0), dataPointer=0}

-- |This gets the value of the tape at the point given by the current pointer
  -- location. This is a somewhat confusing function from the standpoint of OOP.
-- There's pretty much two things going on here:
--
-- One, member accessors are functions. That is, with Memory instance m, to get
-- the memoryData of m, you call the automatically generated memoryData function
-- on m, i.e. `memoryData m` (same for `dataPointer` etc)
--
-- Two: This is written in the point-free style using a combinator. You could
-- rewrite this function in a more familiar fashion as the following:
--
-- memoryValue memory = (memoryData memory) V.! (dataPointer memory)
--
-- Instead, this uses the "phoenix combinator" (or phi), which is basically just:
--
-- pheo f g h x = f (g x) (h x)
--
-- i.e. given a binary function, two unary functions, and an input, it feeds the
-- input into each of the two unary functions, then feeds the results into the
-- binary function. Using currying, by just passing in the functions and not the
-- input, you get a function that will do this on any input you pass.
memoryValue :: Memory -> Word
memoryValue = pheo (V.!) memoryData dataPointer
-- + 2 3
-- pheo (V.!) memoryData dataPointer
-- memoryValue x = (V.!) (memoryData x) (dataPointer x)
-- memoryValue x = (memoryData x) V.! (dataPointer x)

-- |There's no mutation in Haskell (mostly). So instead when you want to "set"
-- something, you make a copy with something changed. This next line "sets" the value of
-- the cell at the current pointer location to the passed in Word, but of course
-- since Memories aren't mutable, just returns a new version with the changes.
-- It does this using the // operator for vectors, which returns a new vector
-- with the values certain indexes replaced. i.e.
--
-- <1, 2, 3> V.// [(2, 99), (0, -123)] == <-123, 2, 99>
--
-- Then it uses record "set" syntax to "modify" the tape in the memory instance.
-- Record set syntax is pretty simple. Given some record type A {foo, bar}, and
-- an instance a {foo=12, bar="test"}, then a {foo=2} is {foo=2, bar="test"}. That is,
--
-- instance {field1=value, fieldk=valuek, ...}
--
-- returns a new instance where all fields have the same values as the previous
-- instance, except for those specified in the brackets.
setMemoryValue :: Memory -> Word -> Memory
setMemoryValue memory value =
  let replaced = (memoryData memory) V.// [(dataPointer memory, value)]
  in memory { memoryData=replaced }

-- |This is basically the same as above, but it's just "setting" the data pointer
setPointerValue :: Memory -> Int -> Memory
setPointerValue memory pointer = memory { dataPointer=pointer }

applyToOldValue :: (a -> b) -> (Memory -> a) -> (Memory -> b -> Memory) -> Memory -> Memory
applyToOldValue modifierFunction propertyFunction setFunction memory =
  setFunction memory (modifierFunction $ propertyFunction memory)

incrModify :: (Integral a) => (Memory -> a) -> (Memory -> a -> Memory) -> Memory -> Memory
incrModify = applyToOldValue (+ 1)

decrModify :: (Integral a) => (Memory -> a) -> (Memory -> a -> Memory) -> Memory -> Memory
decrModify = applyToOldValue (subtract 1)

-- |Uses the above helper and currying to increment the pointer value. Here the
-- function is the increment function, i.e. (+) 1. Since (+) is a function that
-- takes two values and returns them added, (+ 1) is a function that takes one
-- value and returns that added to 1. The important thing to remember is in
-- Haskell, (+) doesn't have the type signature (Num, Num) -> Num, that is a
-- pair of Nums to a new Num, but rather Num -> Num -> Num. A function that
-- takes a Num and returns another function from Num -> Num. It's just
-- syntactical sugar that 1 + 1 is the same as ((+) 1) 1
incrPtr :: Memory -> Memory
incrPtr = incrModify dataPointer setPointerValue

-- |Same as above but subtracts one
decrPtr :: Memory -> Memory
decrPtr = decrModify dataPointer setPointerValue

-- |Basically the same as above but modifying the value at the current pointer
-- location. I don't bother with a helper function here (due to feeling like
-- neither way is pretty enough)
incrValue :: Memory -> Memory
incrValue = incrModify memoryValue setMemoryValue

-- |And the same but decrementing
decrValue :: Memory -> Memory
decrValue = decrModify memoryValue setMemoryValue

-- |This is one of the more "traditional" data types in Haskell. The simplest
-- way to declare a type is like this, which is pretty much just an Enum
-- (although strongly typed of course, not just Ints). The idea is there's a
-- type Direction, which can either be Forward or Backward. You can easily do
-- something like this as well:
--
-- data Stoplight = Green Float | Red Float | Yellow Float | Off
--
-- Where each of the options also holds a float value (for intensity perhaps).
-- These are sort of like enums combined with tuples.
--
-- But in this instance, I'm just using it instead of a boolean for a function
-- behavior switch, because it's more specific and self documenting than a
-- "forward" parameter being true or false. Plus it works better with pattern matching
data Direction = Forward | Backward

-- |Actually being used in this bracket-matching function. This takes an index
-- and a string, assumes that the index is at a bracket, and either searches
-- forward for a matching close bracket or backwards for a matching open.
--
-- The interesting aspect of the types here is the "Maybe". Maybe is a
-- typeclass, something that can apply to other types. It's basically the same
-- thing as optionals in C++ or Swift, and is generally one of the most common
-- monads seen in other languages. The idea is, unlike many other languages
-- where variables can be null or None or nullptr, in Haskell you can't have an
-- Int that is actually Nothing. Instead, you can have a Maybe Int. A Maybe a is basically:
--
-- data Maybe a = Just a | Nothing
--
-- Where `a` is a type. So it's a box that either holds something of that type,
-- or doesn't have anything.
--
-- It's used here because the bracketMatch can either find a matching bracket,
-- or not find one. If it doesn't, returning something like -1 to say "not
-- found" is both ugly and unsafe/impossible to typecheck. Using a Maybe means the
-- compiler requires and enforces dealing with the possibility that nothing
-- (Nothing) is found.
bracketMatch :: Int -> String -> Direction -> Maybe Int
-- This is using Haskell pattern matching. The parameters are an Int, String,
-- and Direction. Here I'm binding the Int to the name index, and the string to
-- code. However the Direction doesn't get a name, in fact I'm just writing a
-- Direction value. That's because this is a pattern match. This "version" of
-- the function will execute if the Direction is Forward.
bracketMatch index code Forward =
  -- Pretty much just calls a helper.
  bracketMatchImpl code '[' ']' [index + 1..(length code) - 1] 1
-- Same as above but searching backward
bracketMatch index code Backward =
  bracketMatchImpl code ']' '[' [index - 1,index - 2..0] 1

-- |The helper. Just takes the code, the open and close brackets, a list of
-- indexes to check (in order), and the current depth of bracket nesting.
-- There's no loops in Haskell. Recursion only. Loops wouldn't be too useful due
-- to immutability anyway.
bracketMatchImpl :: String -> Char -> Char -> [Int] -> Int -> Maybe Int
-- If there's no indexes left to check, we didn't find any match. Nothing is the result.
bracketMatchImpl _ _ _ [] _ = Nothing
-- Otherwise, if we can pattern match the indexes to a head and tail, binding
-- the head to the name i
bracketMatchImpl code open close (i:restRange) nestCount
  -- If the index corresponds to an open bracket, do a recursive call with the
  -- tail (the rest of the indexes) and a 1 higher nesting count
  | (code !! i) == open = bracketMatchImpl code open close restRange (nestCount + 1)
  -- If it's a close bracket, first check if we were at a nest level of 1.
  | (code !! i) == close =
      -- If so, we just found our match. Return Just i, i.e. the value of the
      -- current index.
      if nestCount == 1 then Just i
      -- Otherwise, recursive call with tail and one-lower nest count
      else bracketMatchImpl code open close restRange (nestCount - 1)
  -- If it's not a bracket at all, just do a recursive call with the tail and
  -- same nesting
  | otherwise = bracketMatchImpl code open close restRange nestCount

-- |For the actual evaluation of Brainfuck, we need a "context" to change as we
-- go through the code. The relevant aspects are which command (i.e. character)
-- we're currently running, what the current state of the memory is, and the
-- output produced so far
data Context = Context { commandIndex :: Int
                       , memory :: Memory
                       , output :: String }

bfForward :: Context -> Context
bfForward context = context { memory = incrPtr $ memory context }

bfBackward :: Context -> Context
bfBackward context = context { memory = decrPtr $ memory context }

bfPlus :: Context -> Context
bfPlus context = context { memory = incrValue $ memory context }

bfMinus :: Context -> Context
bfMinus context = context { memory = decrValue $ memory context }

bfOpenClose :: (Word -> Bool) -> Direction -> Context -> String -> Context
bfOpenClose pred direction context code
  | pred (memoryValue $ memory context) =
      -- Gets the Maybe match, and does a pattern match.
      case bracketMatch (commandIndex context) code direction of
        -- If an index was found, just change the command index to it.
        Just index -> context { commandIndex = index }
        -- Otherwise we have an error
        Nothing -> error "No matching bracket"
  | otherwise = context

bfOpen :: Context -> String -> Context
bfOpen = bfOpenClose (== 0) Forward

bfClose :: Context -> String -> Context
bfClose = bfOpenClose (/= 0) Backward

bfOutput :: Context -> IO Context
bfOutput context =
  let intValue = (fromIntegral $ memoryValue $ memory context) :: Int
      newCharacter = chr intValue
      -- Note: the (:) operator *prepends*, not appends. So the output will be
      -- reversed. Just the nature of cons lists.
      newContext = context { output = newCharacter:(output context) }
  in do
    putChar newCharacter
    return newContext

bfInput :: Context -> IO Context
bfInput context = do
  inputChar <- getChar
  let newValue = fromIntegral $ ord inputChar :: Word
      newMemory = setMemoryValue (memory context) newValue
      newContext = context { memory = newMemory }
  return newContext

-- |Using that data type, we can recursively evaluate Brainfuck code.
evalRecursive :: Context -> String -> IO Context
evalRecursive context code =
  -- If the index is past the end of the code, we're done.
  if (commandIndex context) == length code then return context
  else
    let i = commandIndex context
        -- Otherwise, let's build a new context based on the code and the
        -- current command index
        ioContext =
          case code !! i of -- Switching on the character at the command index in the code
            -- setting the memory of the context to one in which the data pointer is one higher
            '>' -> return $ bfForward context
            '<' -> return $ bfBackward context -- and one lower
            -- basically the same but incrementing the value the pointer is currently at
            '+' -> return $ bfPlus context
            -- and decrementing
            '-' -> return $ bfMinus context
            -- outputting the value of the current cell (converts the Word to
            -- a char and appends it to the Context's output)
            '.' -> bfOutput context
            ',' -> bfInput context
            -- Jumps to the matching close bracket if the current cell is 0. Otherwise nothing.
            '[' -> return $ bfOpen context code
            -- Very similar, but jumps backward to matching open bracket if
            -- the cell value is non-zero.
            ']' -> return $ bfClose context code
          -- Then recursivly calls the function again with the new context. Note
          -- that the command index is incremented as well, on top of whatever
          -- other changes were made to the context. This works in all cases, as
          -- for [ and ], we actually want to jump to the instruction directly
          -- *after* the matching bracket (otherwise potentially infinite loops)
    in do
      newContext <- ioContext
      evalRecursive newContext { commandIndex = (commandIndex newContext) + 1 } code

emptyContext = Context {commandIndex=0, memory=createMemory memorySize, output=""}

-- |This function produces the output of a given Brainfuck program
-- values needed: command "index", index of last bracket, total output
eval :: String -> IO String
eval code = do
  finalContext <- evalRecursive emptyContext code
  return $ reverse $ output finalContext


allMatched :: String -> Bool
allMatched = (== 0) . sum . map value
  where
    value '[' = 1
    value ']' = -1
    value _ = 0

-- |This function runs an interactive Brainfuck REPL
-- Nonfunctional at the moment because IO is hard
repl :: IO ()
repl = do
  putStrLn "Entering Brainfuck REPL..."
  replRecurse emptyContext

replRecurse :: Context -> IO ()
replRecurse context = do
  putStrLn $ replMemoryString $ memory context
  putStr "| "
  inputCode <- getLine
  let untilMatching code =
        if not (allMatched code) then do
          putStr "..| "
          more <- getLine
          untilMatching $ code ++ more
        else return code
  finalCode <- untilMatching inputCode
  resultContext <- evalRecursive context finalCode
  let nextContext = emptyContext { memory=(memory resultContext) }
  replRecurse nextContext


replMemoryString :: Memory -> String
replMemoryString memory =
  let lastNonzero = case filter nonzeroTape [0..tapeLength] of
        [] -> 0
        (x:xs) -> x
      end = max (dataPointer memory) lastNonzero
  in intercalate " " $ map valueString [0..end]
  where
    tape = memoryData memory
    tapeLength = (length $ tape) - 1
    nonzeroTape = (/= 0) . (tape V.!)
    valueAt i = (show $ tape V.! i)
    sandwich outer inner = outer ++ inner ++ outer
    valueString i
      | i == (dataPointer memory) = sandwich "*" (valueAt i)
      | otherwise = valueAt i

-- The somewhat weird part: do syntax. This all looks pretty normal, but is
-- actually doing some very weird stuff that I can only partially explain, and
-- very poorly in a comment.
--
-- Basically... check out maybe-do.hs for a terrible but somewhat illuminating
-- example involving the Maybe monad.
--
-- This uses the IO monad however. I'll just describe what's happening rather
-- than about the monads. This is the main function, which pretty much has to be
-- an IO-monad using function (otherwise the program can't do anything)
main = do
  -- Just getting the command line arguments
  args <- getArgs
  case length args of
    0 -> repl -- Defaults to running the (nonfunctional) REPL
    1 -> do -- Otherwise (have to DO again)
      let fileName = args !! 0
      brainfuckCode <- readFile fileName -- Reads the from the filename
      output <- eval brainfuckCode -- Evals it
      putChar '\n'
    2 -> do -- Or
      putStrLn "From command line" -- Reads the code from the command line
      if (args !! 0) == "-s" then -- But only if there's a certain flag
        do
          output <- eval $ args !! 1 -- Evals and outputs
          putChar '\n'
      else return ()
  putStrLn "Done"
