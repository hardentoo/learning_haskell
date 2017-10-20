import Data.Char (ord)
import Data.Char (chr)

p  = 313:: Integer
q  = 223:: Integer

n = p * q
phi = eulersTotient p q

--e = 65537 alternately
e =     let     phiRange = [2..(pred phi)]
                coPrime a b = gcd a b == 1
                coprimes = [x | x<-phiRange, coPrime x phi, coPrime x n]
        in last coprimes

d =    [x | x<- [1..] , mod (x * e) phi ==1]!!3

publicKey = (e,n)
privateKey = (d,n)

encrypt :: (Integer,Integer) -> Integer -> Integer
encrypt (e,n) message = mod (message ^ e) n

decrypt :: (Integer,Integer) -> Integer -> Integer
decrypt (d,n) message = mod (message ^ d) n

eulersTotient :: Integer -> Integer -> Integer
eulersTotient a b = (a - 1) * (b - 1)

main = do
        putStrLn "Enter string:"
        message <- getLine
        let encoded = toInteger.ord <$> message
        let encrypted = (encrypt publicKey) <$> encoded
        let decrypted = (decrypt privateKey) <$> encrypted
        let decoded = chr.fromIntegral <$> decrypted
        putStrLn  $ "encoded: "  ++ ( show encoded)
        putStrLn  $ "encrypted: "  ++  (show encrypted)
        putStrLn  $ "decrypted: "  ++  (show decrypted)
        putStrLn  $ "decoded: "  ++ decoded
