{-# LANGUAGE TypeApplications #-}

module Main where

import AOC.Foldable (howMany)
import AOC.Main (simpleMain)

main :: IO ()
main = simpleMain $ \input ->
  let ns = map (read @Int) $ lines input
      solve n = howMany @Int (uncurry (<)) $ zip ns (drop n ns)
   in (solve 1, solve 3)
