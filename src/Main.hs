{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq
import Control.Lens
import Control.Monad (when)
import Data.Map as Map
import Data.Aeson (Value, Array)
import Data.Aeson.Lens (key, _String, nth)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 as B8
import Data.Text
import Network.HTTP.Client (ManagerSettings (..))
import Network.HTTP.Client.TLS (tlsManagerSettings)

kipptAPIEndPoint = "https://kippt.com/"

kipptId = "objectxtreme@gmail.com"
kipptToken = "536ca0f4ae8a4dad89e64b741e50909587aab9c1"

extraHeader = defaults & header "X-Kippt-Username" .~ [kipptId] & header "X-Kippt-API-Token" .~ [kipptToken]

type Resp = Response (Map String Value)

main :: IO ()
main = do
    grabPage opts $ kipptAPIEndPoint ++ "/api/clips"
  where
    managerSettings = tlsManagerSettings { managerResponseTimeout = Just (30 * 1000 * 1000) }
    opts = defaults & manager .~ Left (managerSettings)
                    & header "X-Kippt-Username" .~ [kipptId]
                    & header "X-Kippt-API-Token" .~ [kipptToken]

grabPage :: Options -> String -> IO ()
grabPage opts url = do
    r <- getWith opts url
    let next = r ^. responseBody . key "meta" . key "next" . _String
    let obj = r ^? responseBody . key "objects" . nth 0
    let body = r ^. responseBody
    Prelude.putStrLn ","
    B8.putStrLn body
    case obj of
        Nothing -> return ()
        Just _ -> grabPage opts $ kipptAPIEndPoint ++ (Data.Text.unpack next)
