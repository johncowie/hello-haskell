-- Example of pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7!"
lucky x = "Would be better if it was a 7."

-- recursive factorial
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)