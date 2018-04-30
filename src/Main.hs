import           Criterion.Main

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bench "10" $ whnf fib 10
        , bench "20" $ whnf fib 20
        , bench "30" $ whnf fib 30
        ]
    , bgroup
        "listfib"
        [ bench "10" $ whnf listF 10
        , bench "20" $ whnf listF 20
        , bench "30" $ whnf listF 30
        ]
    , bgroup
        "scanfib"
        [ bench "10" $ whnf scanF 10
        , bench "20" $ whnf scanF 20
        , bench "30" $ whnf scanF 30
        ]
    ]

listFib = 0 : 1 : [a + b | (a, b) <- zip listFib (tail listFib)]

scanFib = 0 : scanl (+) 1 scanFib

scanF n = scanFib !! n

listF n = listFib !! n

fib m
  | m < 0 = error "negative!"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)
