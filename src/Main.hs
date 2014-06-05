{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import           Control.Lens               ((&), (.~), (^.), (^?))
import           Data.Aeson.Lens            (key, nth, _String)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text                  as T
import           Network.HTTP.Client        (ManagerSettings (..))
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wreq               (responseBody)
import qualified Network.Wreq               as Wreq
import           Options.Applicative        ((<$>), (<**>), (<*>), (<>))
import qualified Options.Applicative        as OA

kipptAPIEndPoint :: String
kipptAPIEndPoint = "https://kippt.com/"

data Config = Config
    { cfg_user   :: String
    , cfg_token  :: String
    , cfg_offset :: Integer
    } deriving Show

main :: IO ()
main = do
    OA.execParser progopts >>= grabBookmarks

progopts :: OA.ParserInfo Config
progopts = OA.info (config <**> OA.helper) (OA.fullDesc <> OA.progDesc "Grab kippt bookmarks")

config :: OA.Parser Config
config = Config <$> OA.strOption (  OA.long "user"
                                 <> OA.metavar "USER"
                                 <> OA.help "Kippt user name"
                                 <> OA.value "example@example.com"
                                 )
                <*> OA.strOption (  OA.long "token"
                                 <> OA.metavar "TOKEN"
                                 <> OA.help "Kippt access token"
                                 <> OA.value "123456789"
                                 )
                <*> OA.option    (  OA.long "offset"
                                 <> OA.metavar "OFFSET"
                                 <> OA.help "Star offset"
                                 <> OA.value 0
                                 )

grabBookmarks :: Config -> IO ()
grabBookmarks cfg = do
    go ""
       (Wreq.defaults & Wreq.manager .~ Left (managerSettings)
                 & Wreq.header "X-Kippt-Username" .~ [C8.pack $ cfg_user cfg]
                 & Wreq.header "X-Kippt-API-Token" .~ [C8.pack $ cfg_token cfg])
       (kipptAPIEndPoint ++ "/api/clips?offset=" ++ (show $ cfg_offset cfg))
  where
    managerSettings = tlsManagerSettings { managerResponseTimeout = Just (60 * 1000 * 1000) }
    go :: String -> Wreq.Options -> String -> IO ()
    go !sep !httpOpts !url = do
        r <- Wreq.getWith httpOpts url
        let obj = r ^? responseBody . key "objects" . nth 0
        case obj of
            Nothing -> return ()
            Just _ -> do
                let !body = r ^. responseBody
                let !next = r ^. responseBody . key "meta" . key "next" . _String
                Prelude.putStrLn sep
                C8L.putStrLn body
                go ",\n" httpOpts $ kipptAPIEndPoint ++ (T.unpack next)
