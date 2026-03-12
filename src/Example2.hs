
{-# LANGUAGE TemplateHaskell #-}

module Example2 where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import           Data.Map             as Map
import           Data.Text            (Text)
import           Dhall                (FromDhall, ToDhall)
import           GHC.Generics         (Generic)

newtype Year = Year Int deriving newtype (Show, Ord, Eq, FromDhall, ToDhall)
newtype Month = Month Int deriving newtype (Show, Ord, Eq, FromDhall, ToDhall)
newtype DayOfMonth = DayOfMonth Int deriving newtype (Show, Ord, Eq, FromDhall, ToDhall)
newtype DayOfWeek = DayOfWeek Int deriving newtype (Show, Ord, Eq, FromDhall, ToDhall)
newtype HourOfDay = HourOfDay Int deriving newtype (Show, Ord, Eq, FromDhall, ToDhall)
newtype RepoUrl = RepoUrl Text deriving newtype (Show, Eq, Ord, FromDhall, ToDhall)

newtype LanguageName = LanguageName Text deriving newtype (Show, Eq, Ord, FromDhall, ToDhall)

data PeriodCommits = PeriodCommits
    { changes        :: Int
    , linesAdded     :: Int
    , linesRemoved   :: Int
    , linesModified  :: Int
    , chunksAdded    :: Int
    , chunksRemoved  :: Int
    , chunksModified :: Int
    }
    deriving (Show, Generic, FromDhall, ToDhall)

instance Semigroup PeriodCommits where
    (<>) :: PeriodCommits -> PeriodCommits -> PeriodCommits
    (<>) p1 p2 =
        PeriodCommits   (p1.changes + p2.changes)
                        (p1.linesAdded + p2.linesAdded)
                        (p1.linesRemoved + p2.linesRemoved)
                        (p1.linesModified + p2.linesModified)
                        (p1.chunksAdded + p2.chunksAdded)
                        (p1.chunksRemoved + p2.chunksRemoved)
                        (p1.chunksModified + p2.chunksModified)

data RepoCommits = RepoCommits
    { repoUrl :: Text
    , changes :: Int
    }
    deriving (Show, Generic, FromDhall, ToDhall)

data LanguageCommits = LanguageCommits
    { language :: LanguageName
    , changes  :: Int
    }
    deriving (Show, Generic, FromDhall, ToDhall)

data Analysis = Analysis
    { commitsPerPeriod    :: Map.Map (Year, Month) PeriodCommits
    , commitsPerDayOfWeek :: Map.Map (Year, DayOfWeek) PeriodCommits
    , commitsPerHourOfDay :: Map.Map (Year, HourOfDay) PeriodCommits
    -- , reposPerPeriod      :: Map.Map ((Year, Month), RepoUrl) PeriodCommits
    -- , languagesPerPeriod  :: Map.Map ((Year, Month), LanguageName) PeriodCommits
    }
    deriving (Show, Generic, FromDhall, ToDhall)

deriveJSON defaultOptions ''Year
deriveJSON defaultOptions ''Month
deriveJSON defaultOptions ''DayOfMonth
deriveJSON defaultOptions ''DayOfWeek
deriveJSON defaultOptions ''HourOfDay
deriveJSON defaultOptions ''LanguageName

deriveJSON defaultOptions ''PeriodCommits
deriveJSON defaultOptions ''RepoCommits
deriveJSON defaultOptions ''LanguageCommits
deriveJSON defaultOptions ''RepoUrl
deriveJSON defaultOptions ''Analysis

readAndParse :: IO Analysis
readAndParse = do
    contents <- BL.readFile "/Users/tom/Downloads/out.json"
    case eitherDecode contents of
        Left err  -> error $ "Failed " ++ err
        Right val -> pure val
