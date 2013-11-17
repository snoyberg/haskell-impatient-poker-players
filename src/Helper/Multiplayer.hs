{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Helper.Multiplayer where

import           ClassyPrelude.Yesod
import           Text.Blaze.Html     (ToMarkup)
import Prelude (Show (..))

data Game a = Game

data App = App
    { games   :: !(IORef (HashMap GameName GameState))
    , title   :: !Text
    , players :: !(IORef (HashSet PlayerName))
    , game    :: !([Player] -> Game ())
    , pcount  :: !Int
    }

newtype GameName = GameName { unGameName :: Text }
    deriving (PathPiece, Show, Read, Eq, Hashable)

newtype PlayerName = PlayerName { unPlayerName :: Text }
    deriving (PathPiece, Show, Read, Eq, ToMarkup, Hashable)

data Player = Player
    { playerName :: !PlayerName
    }
instance Show Player where
    show = unpack . unPlayerName . playerName
data GameState = GameState
    {}

mkYesod "App" [parseRoutes|
/ HomeR GET
/set-player-name SetPlayerNameR GET POST
/join-game JoinGameR GET
/game/#GameName GameR GET
|]

instance Yesod App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

withPlayerName :: (PlayerName -> Handler a) -> Handler a
withPlayerName f = do
    mpn <- lookupSession "player-name"
    case mpn of
        Nothing -> do
            setUltDestCurrent
            redirect SetPlayerNameR
        Just pn -> f $ PlayerName pn

getHomeR :: Handler Html
getHomeR = withPlayerName $ \pn -> do
    App {..} <- getYesod
    defaultLayout $ do
        setTitle $ toHtml title
        [whamlet|
            <h1>#{title}
            <p>
                Hello #{pn}. You can
                <a href=@{JoinGameR}>join a game
                at any time.
        |]

getSetPlayerNameR, postSetPlayerNameR :: Handler Html
getSetPlayerNameR = do
    ((res, form), enctype) <- runFormPost $ renderDivs $ areq playerNameField "Enter your codename" Nothing
    case res of
        FormSuccess name -> do
            setSession "player-name" name
            setMessage $ toHtml $ "Your codename is now: " ++ name
            redirectUltDest HomeR
        _ -> defaultLayout $ do
            setTitle "Set your name"
            [whamlet|
                <h1>Set your name
                <form method=post enctype=#{enctype}>
                    ^{form}
                    <input type=submit value="Set my name">
            |]
  where
    playerNameField = checkBool validName (asText "Your codename must be at least 3 characters long and consist of A-Z, a-z, 0-9, - and _") textField
    validName t = length t >= 3 && all validChar t
    validChar c =
        ('A' <= c && c <= 'Z') ||
        ('a' <= c && c <= 'z') ||
        ('0' <= c && c <= '9') ||
        (c == '-') ||
        (c == '_')

postSetPlayerNameR = getSetPlayerNameR

getJoinGameR :: Handler ()
getJoinGameR = withPlayerName $ \pn -> error "getJoinGameR"

getGameR :: GameName -> Handler ()
getGameR gn = withPlayerName $ \pn -> do
    App {..} <- getYesod
    gss <- readIORef games
    gs <- maybe notFound return $ lookup gn gss
    error "getGameR"

playGame :: String
         -> Int -- ^ player count
         -> ([Player] -> Game ())
         -> IO ()
playGame t players g = do
    app <- App <$> newIORef mempty
               <*> pure (pack t)
               <*> newIORef mempty
               <*> pure g
               <*> pure players
    warpEnv app

main :: IO ()
main = playGame "Test game" 2 (const Game)
