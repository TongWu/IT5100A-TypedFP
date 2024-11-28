f :: Int -> String
f x =
    let sx :: String = show x
        y :: Int = x * 2
        sy :: String = show y
    in sx ++ " * 2 = " ++ sy

main :: IO ()
main = print (f 52)