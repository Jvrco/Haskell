import Control.Concurrent.STM

type Buffer a = TVar [a]

newBuffer :: [a] -> IO (Buffer a)
newBuffer = newTVarIO

put :: Buffer a -> a -> STM ()
put buffer item = do
    currentItems <- readTVar buffer
    writeTVar buffer (currentItems ++ [item])


get :: Buffer a -> STM a
get buffer = do
    items <- readTVar buffer
    case items of
        [] -> retry  
        (x:xs) -> do
            writeTVar buffer xs
            return x
