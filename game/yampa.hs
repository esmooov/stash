import FRP.Yampa
import System.Random

initialization = return 1

input _ = return (1, Nothing)

actuation :: Bool -> Int -> IO Bool
actuation _ n = do
    print n
    return False

main = do
    gen <- getStdGen
    reactimate initialization input actuation (noise gen)
