data IOTerm = PutChar Char IOTerm
            | GetChar (Char->IOTerm)
            | Exit

newtype MyIO x = MyIO { runMyIO :: ((x -> IOTerm) -> IOTerm) }

instance Monad MyIO where
  m >>= f = MyIO (\k -> (runMyIO m) (\x -> runMyIO (f x) k))
  return x = MyIO (\k -> k x)

instance Functor MyIO where
  fmap = undefined

instance Applicative MyIO where
  pure = undefined
  (<*>) = undefined


myMain :: MyIO ()
myMain = do
  xs <- myGetStr
  myPutStr ("Hello, " ++ xs ++ "!")


myPutChar :: Char -> MyIO ()
myPutChar ch = MyIO $ \k -> PutChar ch (k ())

myPutStr :: [Char] -> MyIO ()
myPutStr (x:xs) = do { myPutChar x; myPutStr xs }
myPutStr [] = myPutChar '\n'

myGetChar :: MyIO Char
myGetChar = MyIO GetChar

myGetStr :: MyIO [Char]
myGetStr = myGetChar >>= read []
  where read res '\n' = return res
        read res ch   = myGetChar >>= read (res ++ [ch])


    
runIOTerm :: IOTerm -> IO ()

runIOTerm (PutChar ch cont) = do
  putChar ch
  runIOTerm cont

runIOTerm (GetChar fn) = do
  c <- getChar
  runIOTerm (fn c)

runIOTerm Exit = return ()


main = do
  runIOTerm (runMyIO myMain $ \_ -> Exit)
