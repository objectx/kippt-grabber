{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

import           Control.Exception          (finally)
import           Control.Lens               (makeLenses, to, (&), (.~), (^.),
                                             (^?))
import           Control.Monad              (unless)
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
import qualified System.IO                  as IO

kipptAPIEndPoint :: String
kipptAPIEndPoint = "https://kippt.com/"

data Config = Config
    { _user    :: String
    , _token   :: String
    , _offset  :: Integer
    , _timeout :: Integer
    } deriving Show

makeLenses ''Config

main :: IO ()
main =
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
                <*> OA.option    (  OA.long "timeout"
                                 <> OA.metavar "TIMEOUT"
                                 <> OA.help "Request timeout (in sec)"
                                 <> OA.value (60 :: Integer))

grabBookmarks :: Config -> IO ()
grabBookmarks cfg = do
    IO.putStrLn "["
    let !opt = Wreq.defaults & Wreq.manager .~ Left (managerSettings $ cfg ^. timeout)
                             & Wreq.header "X-Kippt-Username" .~ [cfg ^. user . to C8.pack]
                             & Wreq.header "X-Kippt-API-Token" .~ [cfg ^. token . to C8.pack]
    let !firstUrl = kipptAPIEndPoint ++ "/api/clips?offset=" ++ (cfg ^. offset . to show)
    finally (go "" opt firstUrl) (IO.putStrLn "]")
  where
    managerSettings :: Integer -> ManagerSettings
    managerSettings tmout = tlsManagerSettings { managerResponseTimeout = Just (fromInteger $ tmout * 1000 * 1000) }
    go :: String -> Wreq.Options -> String -> IO ()
    go !sep !httpOpts !url = do
        r <- Wreq.getWith httpOpts url
        unless (r ^? responseBody . key "objects" . nth 0 == Nothing) $ do
            IO.putStrLn sep
            C8L.putStrLn $ r ^. responseBody
            let !nextUrlFragment = T.unpack (r ^. responseBody . key "meta" . key "next" . _String)
            go ",\n" httpOpts $ kipptAPIEndPoint ++ nextUrlFragment
