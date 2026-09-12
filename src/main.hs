-- (taken, rest)
takeWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile _ "" = ("", "")
takeWhile cond (c:cs)
    | cond c =
        let (taken, rest) = takeWhile cond cs in
            (c:taken, rest)
    | _ = ("", c:cs)

main :: IO ()
main = do
    print $ span (\c -> c < 'e') "abcdef"