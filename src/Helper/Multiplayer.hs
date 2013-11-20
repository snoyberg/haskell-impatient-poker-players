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
    , playMultiplayerGame
    , Player
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
    , gdToken :: !Word32
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
        msg = pack content
        gd' = gd { gdMessages = insert player (msgs ++ singleton msg) $ gdMessages gd
                 , gdToken = gdToken gd + 1
                 }
     in (gd', NMDone ())

askPlayer :: Player -> String -> Game String
askPlayer p s = Game $ \gd -> (gd { gdToken = gdToken gd + 1 }, NMAskPlayer p s return)

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

type Message = Text

instance Show Player where
    show = unpack . playerName

data GameState = GSNeedPlayers !(Vector Player)
               | GSRunning !GameData !(NextMove ())

mkYesod "App" [parseRoutes|
/set-player-name SetPlayerNameR GET POST
/ JoinGameR GET
/game/#GameName GameR GET POST
/game/#GameName/token GameTokenR GET
|]

instance Yesod App where
    defaultLayout w = do
        pc <- widgetToPageContent $ do
            w
            addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css"
            addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css"
            addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
        app <- getYesod
        giveUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        <meta name=viewport content="width=device-width, initial-scale=1.0">
                        ^{pageHead pc}
                    <body>
                        <div .container>
                            <h1>#{pageTitle pc}
                            ^{pageBody pc}
                            <footer .panel-footer>
                                You're playing #{title app} on
                                <a href=https://www.fpcomplete.com>FP Haskell Center#
                                , powered by
                                <a href=http://www.yesodweb.com>Yesod#
                                .
            |]
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

getSetPlayerNameR, postSetPlayerNameR :: Handler Html
getSetPlayerNameR = do
    ((res, form), enctype) <- runFormPost $ renderBootstrap $ areq playerNameField "Enter your codename"
        { fsAttrs =
            [ ("autofocus", "autofocus")
            , ("class", "form-control")
            ]
        } Nothing
    case res of
        FormSuccess name -> do
            setSession "player-name" name
            setMessage $ toHtml $ "Your codename is now: " ++ name
            redirectUltDest JoinGameR
        _ -> defaultLayout $ do
            setTitle "Set your name"
            [whamlet|
                <form role=form method=post enctype=#{enctype}>
                    <div .form-group>
                        ^{form}
                    <div .form-group>
                        <button .btn .btn-default>Set my name
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
            | pn `elem` v = GSNeedPlayers v
            | length v' >= pcount =
                let (gd, nm) = unGame game GameData
                                { gdPlayers = v'
                                , gdMessages = mempty
                                , gdToken = minBound
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

getGameTokenR :: GameName -> Handler Value
getGameTokenR gn = do
    moldTokenT <- lookupGetParam "token"
    moldToken <-
        case moldTokenT of
            Nothing -> notFound
            Just "waiting" -> return Nothing
            Just t ->
                case readMay $ unpack t of
                    Nothing -> notFound
                    Just i -> return $ Just i
    App {..} <- getYesod
    join $ liftIO $ atomically $ do
        gss <- readTVar games
        case lookup gn gss of
            Nothing -> return notFound
            Just GSNeedPlayers {} ->
                case moldToken of
                    Nothing -> retry
                    Just _ -> return $ return $ object ["token" .= asText "waiting"]
            Just (GSRunning gd _) ->
                case moldToken of
                    Just t | t == gdToken gd -> retry
                    _ -> return $ return $ object ["token" .= gdToken gd]

reloadWidget :: GameName -> Maybe Word32 -> Widget
reloadWidget gn token = do
    let url = (GameTokenR gn, [("token", pack $ maybe "waiting" show token)])
    toWidget
        [julius|
            $(function(){
                $.ajax({
                    dataType: "json",
                    url: "@?{url}",
                    complete: function(){ location.reload() }
                });
            });
        |]

getGameR :: GameName -> Handler Html
getGameR gn = withPlayerName $ \pn -> do
    App {..} <- getYesod
    gss <- liftIO $ readTVarIO games
    gs <- maybe (redirect JoinGameR) return $ lookup gn gss
    case gs of
        GSNeedPlayers players | pn `elem` players -> defaultLayout $ do
            setTitle "Waiting for players"
            [whamlet|
                <p>Waiting for players to join. We need #{show pcount} total, currently waiting:
                <ul>
                    $forall p <- players
                        <li>#{p}
            |]
            reloadWidget gn Nothing
        GSNeedPlayers _ -> redirect JoinGameR
        GSRunning gd nm -> defaultLayout $ do
            setTitle $ toHtml $ "Playing a game of " ++ title
            toWidget
                [julius|
                    $(function(){
                        $('html, body').animate(
                           { scrollTop: $(document).height()-$(window).height()},
                           1400
                        );
                    });
                |]
            [whamlet|
                <h2>Players: #{intercalate ", " $ map playerName $ gdPlayers gd}
                <ul>
                    $forall msg <- fromMaybe mempty $ lookup pn $ gdMessages gd
                        <li>#{msg}
            |]
            case nm of
                NMDone () -> [whamlet|<p>Game over.|]
                NMAskPlayer p msg _
                    | p == pn ->
                        [whamlet|
                            <form method=post>
                                <p>#{msg}
                                <input type=text name=input autofocus>
                                <input type=submit>
                        |]
                    | otherwise -> do
                        [whamlet|<p>Waiting for response from #{p}.|]
                        reloadWidget gn $ Just $ gdToken gd

postGameR :: GameName -> Handler ()
postGameR gn = withPlayerName $ \pn -> do
    App {..} <- getYesod
    minput <- lookupPostParam "input"
    input <- maybe (invalidArgs ["Missing input"]) return minput
    join $ liftIO $ atomically $ do
        gss <- readTVar games
        case lookup gn gss of
            Nothing -> return $ redirect $ GameR gn
            Just (GSRunning gd (NMAskPlayer p _ f)) | p == pn -> do
                let (gd', nm) = unGame (f $ unpack input) gd
                writeTVar games $ insert gn (GSRunning gd' nm) gss
                return $ redirect $ GameR gn
            Just _ -> return $ redirect $ GameR gn

playMultiplayerGame :: String
                    -> Int -- ^ player count
                    -> Game ()
                    -> IO ()
playMultiplayerGame t players g = do
    app <- App <$> newTVarIO mempty
               <*> pure (pack t)
               <*> newTVarIO mempty
               <*> pure g
               <*> pure players
    warpEnv app
