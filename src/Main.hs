{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson                 (Value)
import           Data.Aeson.Lens            (key, nth, _String)
import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy.Char8 as B8
import           Data.Map                   as Map
import           Data.Text
import           Network.HTTP.Client        (ManagerSettings (..))
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wreq

kipptAPIEndPoint = "https://kippt.com/"

kipptId = "objectxtreme@gmail.com"
kipptToken = "536ca0f4ae8a4dad89e64b741e50909587aab9c1"

main :: IO ()
main = do
    grabPage opts $ kipptAPIEndPoint ++ "/api/clips"
  where
    managerSettings = tlsManagerSettings { managerResponseTimeout = Just (60 * 1000 * 1000) }
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
