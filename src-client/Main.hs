module Main where
import Client (runClient)
import Utils (helloWorld)
import Header (bitsToInt, Bit (..))

main :: IO ()
main = do
    print $ bitsToInt [O, I, O, I, O, O, I, O]