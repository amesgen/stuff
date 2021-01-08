module MyLib
  ( -- * Foo
    -- $foo

    -- * Blub

    -- |
    -- >>> 1 + 1
    -- 2
    someFunc,
  )
where

-- $foo
-- >>> 1 + 1
-- 3

someFunc :: IO ()
someFunc = putStrLn "someFunc"
