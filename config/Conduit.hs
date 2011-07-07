{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withTap)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withTap $ run 3000
#else
import Controller (withTap)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withTap $ run port . debug
#endif
