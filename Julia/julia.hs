import Data.Complex
import Data.Array
import Data.List

c = (-0.4):+0.6
f n z0 = (take n $ iterate (\z -> z^2+c) z0) !! (n-1)

plane = [ [(i/25:+j/25) | j <- [-50..50]] | i <- [-50..50]]
image = [ intercalate " " ([if magnitude(f(60)(plane!!i!!j))<2 then "0" else " " | i <- [0..100]]) | j <- [0..100]]

main = mapM_ putStrLn image


