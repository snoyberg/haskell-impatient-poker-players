{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Helper.Multiplayer
    ( tellAllPlayers
    , tellPlayer
    , askPlayer
    , getPlayers
    , playGame
    ) where

import           ClassyPrelude.Yesod hiding (split, show)
import           Control.Concurrent.STM
import           Control.Monad          (replicateM)
import           Prelude                (Show (..))
import           System.Random
import           Text.Blaze.Html        (ToMarkup)
import Control.Monad.Random.Class

data GameData = GameData
    { gdPlayers :: !(Vector Player)
    , gdMessages :: !(HashMap Player (Vector Message))
    , gdNextMessageId :: !Word32
    , gdRandomGen :: !StdGen
    }
newtype Game a = Game
    { unGame :: GameData -> (GameData, NextMove a)
    }
data NextMove a = NMAskPlayer Player String (String -> Game a)
                | NMDone a
instance Functor Game where
    fmap = liftM
instance Applicative Game where
    pure = return
    (<*>) = ap
instance Monad Game where
    return a = Game $ \gd -> (gd, NMDone a)

    Game f >>= g = Game $ \gd1 -> do
        let (gd2, nm) = f gd1
         in case nm of
              NMDone a -> unGame (g a) gd2
              NMAskPlayer p s f' -> (gd2, NMAskPlayer p s $ f' >=> g)
instance MonadRandom Game where
    getRandom = liftStateG random
    getRandoms = liftStateG $ first randoms . split
    getRandomR (x,y) = liftStateG $ randomR (x,y)
    getRandomRs (x,y) = liftStateG $
                            first (randomRs (x,y)) . split
liftStateG :: (StdGen -> (a, StdGen)) -> Game a
liftStateG f = Game $ \gd ->
    let (a, gen') = f $ gdRandomGen gd
     in (gd { gdRandomGen = gen' }, NMDone a)

tellAllPlayers :: String -> Game ()
tellAllPlayers msg = do
    gd <- Game $ \gd -> (gd, NMDone gd)
    forM_ (gdPlayers gd) $ flip tellPlayer msg

tellPlayer :: Player -> String -> Game ()
tellPlayer player content = Game $ \gd -> do
    let msgs = fromMaybe mempty $ lookup player $ gdMessages gd
        msg = Message (pack content) (gdNextMessageId gd)
        gd' = gd { gdMessages = insert player (msgs ++ singleton msg) $ gdMessages gd
                 , gdNextMessageId = succ $ gdNextMessageId gd
                 }
     in (gd', NMDone ())

askPlayer :: Player -> String -> Game String
askPlayer p s = Game $ \gd -> (gd, NMAskPlayer p s return)

getPlayers :: Game [Player]
getPlayers = Game $ \gd -> (gd, NMDone $ unpack $ gdPlayers gd)

data App = App
    { games   :: !(TVar (HashMap GameName GameState))
    , title   :: !Text
    , players :: !(TVar (HashSet Player))
    , game    :: !(Game ())
    , pcount  :: !Int
    }

newtype GameName = GameName { unGameName :: Text }
    deriving (PathPiece, Show, Read, Eq, Hashable)

newtype Player = Player { playerName :: Text }
    deriving (PathPiece, Eq, ToMarkup, Hashable, Ord)

data Message = Message
    { messageText :: !Text
    , messageId :: !Word32
    }

instance Show Player where
    show = unpack . playerName

data GameState = GSNeedPlayers !(Vector Player)
               | GSRunning !GameData !(NextMove ())

mkYesod "App" [parseRoutes|
/ HomeR GET
/set-player-name SetPlayerNameR GET POST
/join-game JoinGameR GET
/game/#GameName GameR GET POST
|]

instance Yesod App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

withPlayerName :: (Player -> Handler a) -> Handler a
withPlayerName f = do
    mpn <- lookupSession "player-name"
    case mpn of
        Nothing -> do
            setUltDestCurrent
            redirect SetPlayerNameR
        Just pn -> f $ Player pn

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
getJoinGameR = withPlayerName $ \pn -> do
    App {..} <- getYesod
    newName <- fmap (GameName . pack)
             $ liftIO
             $ Control.Monad.replicateM 10
             $ randomRIO ('A', 'Z')
    randomGen <- liftIO getStdGen
    let loop [] = (newName, addPlayer mempty)
        loop ((name, GSNeedPlayers players):_) = (name, addPlayer players)
        loop (_:rest) = loop rest
        addPlayer v
            | length v' >= pcount =
                let (gd, nm) = unGame game GameData
                                { gdPlayers = v'
                                , gdMessages = mempty
                                , gdNextMessageId = minBound
                                , gdRandomGen = randomGen
                                }
                 in GSRunning gd nm
            | otherwise = GSNeedPlayers v'
          where
            v' = v ++ singleton pn
    join $ liftIO $ atomically $ do
        gs <- readTVar games
        let (gameName, newGameState) = loop $ unpack gs
        writeTVar games $ insert gameName newGameState gs
        return $ redirect $ GameR gameName

getGameR :: GameName -> Handler Html
getGameR gn = withPlayerName $ \pn -> do
    App {..} <- getYesod
    gss <- liftIO $ readTVarIO games
    gs <- maybe notFound return $ lookup gn gss
    case gs of
        GSNeedPlayers players -> defaultLayout $ do
            setTitle "Waiting for players"
            [whamlet|
                <p>Waiting for players to join. We need #{show pcount} total, currently waiting:
                <ul>
                    $forall p <- players
                        <li>#{p}
            |]
        GSRunning gd nm -> defaultLayout $ do
            setTitle $ toHtml $ "Playing a game of " ++ title
            [whamlet|
                <ul>
                    $forall Message msg _ <- fromMaybe mempty $ lookup pn $ gdMessages gd
                        <li>#{msg}
            |]
            case nm of
                NMDone () -> [whamlet|<p>Game over.|]
                NMAskPlayer p msg _
                    | p == pn ->
                        [whamlet|
                            <form method=post>
                                <p>#{msg}
                                <input type=text name=input>
                                <input type=submit>
                        |]
                    | otherwise -> [whamlet|<p>Waiting for response from #{p}.|]

postGameR :: GameName -> Handler ()
postGameR gn = withPlayerName $ \pn -> do
    App {..} <- getYesod
    minput <- lookupPostParam "input"
    input <- maybe (invalidArgs ["Missing input"]) return minput
    join $ liftIO $ atomically $ do
        gss <- readTVar games
        case lookup gn gss of
            Nothing -> return notFound
            Just (GSRunning gd (NMAskPlayer p _ f)) | p == pn -> do
                let (gd', nm) = unGame (f $ unpack input) gd
                writeTVar games $ insert gn (GSRunning gd' nm) gss
                return $ redirect $ GameR gn
            Just _ -> return badMethod

playGame :: String
         -> Int -- ^ player count
         -> Game ()
         -> IO ()
playGame t players g = do
    app <- App <$> newTVarIO mempty
               <*> pure (pack t)
               <*> newTVarIO mempty
               <*> pure g
               <*> pure players
    warpEnv app
