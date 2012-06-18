module StringUtil(simpleRead, cheatConcat, (++), stringStep)
where

import Prelude hiding((++))
import Data.ByteString.Internal as BSI
import System.IO.Posix.MMap(unsafeMMapFile)
import qualified Control.Exception as Exc
import Data.ByteString.Char8 as BSC
import Control.Exception(assert)

simpleRead fname = unsafeMMapFile fname `Exc.catch` \e -> do reportError (e :: IOError)
                                                             BSC.readFile fname
  where
    reportError e = do Prelude.putStrLn $ Prelude.concat [show e, "while trying to mmap('", fname, "')"]

cheatConcat (PS x1 s1 l1) (PS x2 s2 l2) = assert (x1 == x2) $ PS x1 s1 (s2+l2-s1)

(++) = BSC.append

stringStep (PS x s l) i = assert (si >  0) $ PS x si l
  where si = s+i

