-- Demonstration of Monads and dos

-- Here's a simple function that takes an Int and returns a Maybe Int (so either
-- Just an Int or Nothing)
maybeGetValue :: Int -> Maybe Int
maybeGetValue value
  | (even value) && (value `mod` 5 /= 0) = Just value
  | otherwise = Nothing

-- Imagine adding an extra level of computation to this, i.e. sending the last
-- value through one more function that could either return something or
-- nothing. You'd have to do yet another level of nesting and case checking, to
-- either return Nothing or process it correctly, and wrap that in Just. This
-- gets very bad very fast.
maybeProcessUgly :: Maybe Int -> Maybe Int
maybeProcessUgly Nothing = Nothing
maybeProcessUgly Just value =
  let larger = value + 299
      newValue = maybeGetValue larger
  in case newValue of
       Nothing -> Nothing
       Just new -> Just (new * 32)
       

-- Here's a function that makes use of do syntax to handle Maybe's cleanly
maybeDo :: Maybe Int -> Maybe Int
maybeDo value = do
  containedValue <- value
  let larger = containedValue + 299
  newValue <- maybeGetValue larger
  return $ newValue * 32

-- What's happening? Well, due to "Monad Magic", Haskell knows that if you have
-- a value `x` of type `Maybe a`, and a function `f` with type `a -> a`, or `a
-- -> Maybe a`, then the "right thing to do" to "call" `f` on `x` (even though
-- they have incompatible types) is to just return Nothing if x is Nothing, and
-- if x is Just y, return (f y). Specifically there are overloaded operators for
-- the Maybe type, and do is implicitly adding these operators. Then <- is a
-- sort-of name binding. Once you do that (with the right side being a monad),
-- then you treat the left side as if it was the value "contained" in that monad
-- (here a Maybe). Any functions you call on this value "behave right", i.e. for
-- Maybe's will work if the value wasn't Nothing, and won't even be called if it was.
--
-- So what you get at the end of this is either Nothing, if the original value
-- or the result from maybeGetValue was Nothing, or (value + 299) * 32 if it
-- wasn't.
--
-- There's a general but not easily describable pattern here, which as it turns
-- out applies to IO operations, lists, functions, state, so on so on.

main = print $ maybeDo $ Just 13
