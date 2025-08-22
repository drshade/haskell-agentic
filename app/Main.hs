{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Main where

import           Control.Monad.Free (Free (..), liftF)
import           Control.Monad.RWS  (RWS, ask, get, modify, tell)
import           Data.List          (isInfixOf)
import           Data.Map           (Map)
import qualified Data.Map           as Map (lookup)

-- Define the DSL
newtype Area = Area String
newtype House = House String

data Step next
    = WantsHouse (Bool -> next)
    | GetFavouriteArea (Area -> next)
    | FindHouse Area (House -> next)
    | BuyHouse House (() -> next)
    | GetHighGrowthAreas ([Area] -> next)
    | GetProperties ([String] -> next)
    deriving (Functor)

type Script = Free Step

wantsHouse :: Script Bool
wantsHouse = liftF $ WantsHouse id

getFavouriteArea :: Script Area
getFavouriteArea = liftF $ GetFavouriteArea id

findHouse :: Area -> Script House
findHouse area = liftF $ FindHouse area id

buyHouse :: House -> Script ()
buyHouse house = liftF $ BuyHouse house id

getHighGrowthAreas :: Script [Area]
getHighGrowthAreas = liftF $ GetHighGrowthAreas id

getProperties :: Script [String]
getProperties = liftF $ GetProperties id

customer :: Script ()
customer = do
    wantsHouse' <- wantsHouse
    if wantsHouse'
        then do
            favourite_area <- getFavouriteArea
            found_house <- findHouse favourite_area
            buyHouse found_house
        else
            pure ()

contains :: [String] -> String -> Bool
contains ps prop = foldr ((||) . isInfixOf prop) False ps

investor :: Script ()
investor = do
    properties <- getProperties
    areas <- getHighGrowthAreas
    let filtered = filter (\(Area area) -> not (contains properties area)) areas
    mapM_
        (\area -> do
            found_house <- findHouse area
            buyHouse found_house
        )
        filtered

-- Toy Script RWS
type Input = Map String String
type Output = [String]
type State = [String]
type ScriptRWS = RWS Input Output State

interpret :: Script a -> ScriptRWS ()
interpret (Pure _) = pure ()
interpret (Free (WantsHouse next)) = do
    input <- ask
    if Map.lookup "wants_house" input == Just "true"
        then interpret (next True)
        else do
            tell ["missing wants_house == true"]
            pure ()
interpret (Free (GetFavouriteArea next)) = do
    input <- ask
    case Map.lookup "favourite_area" input of
        Just area -> interpret (next $ Area area)
        Nothing -> do
            tell ["missing favourite_area"]
            pure ()
interpret (Free (FindHouse (Area area) next)) = do
    input <- ask
    case Map.lookup "found_house" input of
        Just house -> interpret (next $ House house)
        Nothing -> do
            tell ["Looking for houses in " <> area]
            tell ["missing found_house"]
            tell ["option 1: 123 Boom Straat, " <> area]
            tell ["option 2: 456 Kerk Straat, " <> area]
            pure ()
interpret (Free (BuyHouse (House house) next)) = do
    modify (\s -> s <> ["Owns house: " <> house])
    interpret (next ())
interpret (Free (GetHighGrowthAreas next)) = do
    interpret (next [Area "Sandton", Area "Melrose", Area "Fourways"])
interpret (Free (GetProperties next)) = do
    properties <- get
    interpret (next properties)

main :: IO ()
main = putStrLn "Hello, Haskell!"
