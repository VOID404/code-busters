import Control.Monad
import System.IO

data Team
    = TeamGhost
    | TeamTL
    | TeamBR

data State = Idle | Carrying

data Position = Position Float Float

data Entity = Entity
    { entityID :: Int
    , entityPos :: Position
    , entityTeam :: Team
    , entityState :: State
    , entityValue :: Int
    }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Send your busters out into the fog to trap ghosts and bring them home!

    -- the amount of busters you control
    bustersPerPlayer <- read <$> getLine :: IO Int

    -- the amount of ghosts on the map
    ghostcount <- read <$> getLine :: IO Int

    -- if this is 0, your base is on the top left of the map, if it is one, on the bottom right
    myteamid <- read <$> getLine :: IO Int

    -- game loop
    forever $ do
        input_line <- getLine
        let entitiesN = read input_line :: Int -- the number of busters and ghosts visible to you
        entities <- replicateM entitiesN $ do
            input <- map read . words <$> getLine :: IO [Int]
            let [entityID, x, y, team, state, value] = input
            let pos = Position (fromIntegral x) (fromIntegral y)
            return
                Entity
                    { entityID = entityID
                    , entityPos = pos
                    , entityTeam = case team of
                        -1 -> TeamGhost
                        0 -> TeamTL
                        1 -> TeamBR
                        _ -> error "Invalid team number"
                    , entityState = case state of
                        0 -> Idle
                        1 -> Carrying
                        _ -> error "Invalid entity state"
                    , entityValue = value
                    }

        replicateM bustersPerPlayer $ do
            -- hPutStrLn stderr "Debug messages..."

            -- MOVE x y | BUST id | RELEASE
            putStrLn ("MOVE 8000 4500")
            return ()
