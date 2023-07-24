{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad
import Data.Foldable (minimumBy)
import System.IO
import Text.Printf (printf)

data Action
    = ActionMove Position
    | ActionBust Int
    | ActionRelease
    | Message String Action

instance Show Action where
    show = \case
        ActionMove pos -> "MOVE " ++ show pos
        ActionBust i -> "BUST " ++ show i
        ActionRelease -> "RELEASE"
        Message msg act -> show act ++ " " ++ msg

data Team
    = TeamGhost
    | TeamTL
    | TeamBR
    deriving (Eq)

basePosition :: Team -> Position
basePosition = \case
    TeamGhost -> Position 8000 4500
    TeamTL -> Position 0 0
    TeamBR -> Position 16000 9000

instance Enum Team where
    toEnum = \case
        -1 -> TeamGhost
        0 -> TeamTL
        1 -> TeamBR
        _ -> error "Invalid team number"
    fromEnum = \case
        TeamGhost -> -1
        TeamTL -> 0
        TeamBR -> 1

data State
    = Idle
    | Carrying
    deriving (Eq)

data Position = Position Float Float deriving (Eq)

distance :: Position -> Position -> Float
distance (Position x1 y1) (Position x2 y2) =
    sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

instance Show Position where
    show (Position x y) = printf "%.f %.f" x y

type Buster = Entity
type Ghost = Entity
type Enemy = Entity

data Entity = Entity
    { entityID :: Int
    , entityPos :: Position
    , entityTeam :: Team
    , entityState :: State
    , entityValue :: Int
    }

parseEntity :: IO Entity
parseEntity = do
    input <- map read . words <$> getLine :: IO [Int]
    let [entityID, x, y, team, state, value] = input
    let pos = Position (fromIntegral x) (fromIntegral y)
    return
        Entity
            { entityID = entityID
            , entityPos = pos
            , entityTeam = toEnum team
            , entityState = case state of
                0 -> Idle
                1 -> Carrying
                _ -> error "Invalid entity state"
            , entityValue = value
            }

splitTeams :: Team -> [Entity] -> ([Buster], [Enemy], [Ghost])
splitTeams myTeam = rec [] [] []
  where
    rec bs es gs [] = (reverse bs, reverse es, reverse gs)
    rec bs es gs (h@Entity{entityTeam} : t)
        | entityTeam == TeamGhost = rec bs es (h : gs) t
        | entityTeam == myTeam = rec (h : bs) es gs t
        | otherwise = rec bs (h : es) gs t

data Game = Game
    { bustersPerPlayer :: Int
    , ghostCount :: Int
    , myTeam :: Team
    }

gameLogic :: Game -> [Buster] -> [Ghost] -> [Enemy] -> [Action]
gameLogic Game{myTeam} busters ghosts _ = map process busters
  where
    process b@Entity{entityState}
        | entityState == Carrying =
            if baseDist < 1500
                then ActionRelease
                else ActionMove base
        | null ghosts = ActionMove center
        | ghostDist < 1760 = ActionBust $ entityID ghost
        | otherwise = ActionMove $ entityPos ghost
      where
        ghost = closestGhost b
        ghostDist = distance (entityPos b) (entityPos ghost)
        baseDist = distance (entityPos b) base

    closestGhost :: Buster -> Ghost
    closestGhost b = minimumBy (compareDistance b) ghosts
    compareDistance :: Entity -> Entity -> Entity -> Ordering
    compareDistance Entity{entityPos = bPos} Entity{entityPos = g1Pos} Entity{entityPos = g2Pos} =
        let dist1 = distance bPos g1Pos
            dist2 = distance bPos g2Pos
         in compare dist1 dist2

    base = basePosition myTeam
    center = Position 8000 4500

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Send your busters out into the fog to trap ghosts and bring them home!

    -- the amount of busters you control
    bustersPerPlayer <- read <$> getLine :: IO Int

    -- the amount of ghosts on the map
    ghostCount <- read <$> getLine :: IO Int

    -- if this is 0, your base is on the top left of the map,
    -- if it is one, on the bottom right
    myTeam <- toEnum . read <$> getLine :: IO Team

    let game = Game{bustersPerPlayer, ghostCount, myTeam}

    -- game loop
    forever $ do
        input_line <- getLine
        -- the number of busters and ghosts visible to you
        let entitiesN = read input_line :: Int

        (busters, enemies, ghosts) <-
            splitTeams myTeam
                <$> replicateM entitiesN parseEntity

        mapM_ print $ gameLogic game busters ghosts enemies

-- hPutStrLn stderr "Debug messages..."
-- MOVE x y | BUST id | RELEASE

-- return $ ActionMove $ Position 8000 4500
