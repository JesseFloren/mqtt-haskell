module Main where
import Client (runClient)
import Utils (helloWorld)
import Header (bitsToInt, Bit (..))

main :: IO ()
main = do
    runClient