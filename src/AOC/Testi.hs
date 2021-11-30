module AOC.Testi where

(@?=) :: (Eq a, Show a) => a -> a -> IO ()
actual @?= expected
    | actual == expected = pure ()
    | otherwise          = fail $
        mconcat [ "AOC.Testi: Expected "
                , show expected
                , " but got "
                , show actual
                ]
