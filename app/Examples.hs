module Examples where


import           Data.Text
import           Dhall        (FromDhall, ToDhall)
import           GHC.Generics (Generic)

data Joke = Joke { genre :: Text, setup :: Text, punchline :: Text }
    deriving (Generic, Show, FromDhall, ToDhall)

newtype Dog = Dog { name :: String }
    deriving (Generic, Show, FromDhall, ToDhall)

data BetterJoke
    = DadJoke { setup :: Text, punchline :: Text }
    | OneLiner { line :: Text }
    | Story { paragraphs :: [Text] }
    deriving (Generic, Show, FromDhall, ToDhall)

data MaritalStatus = Unmarried | Married | Widowed
    deriving (Generic, Show, FromDhall, ToDhall)

data ApplicationForm = ApplicationForm
    { name          :: Text
    , age           :: Int
    , maritalStatus :: MaritalStatus
    }
    deriving (Generic, Show, FromDhall, ToDhall)

data Priority = Low | Medium | High | Urgent
    deriving (Generic, Show, FromDhall, ToDhall, Eq)

data TaskStatus = Todo | InProgress | Blocked | Completed
    deriving (Generic, Show, FromDhall, ToDhall, Eq)

data Task = Task
    { title       :: Text
    , description :: Maybe Text
    , priority    :: Priority
    , status      :: TaskStatus
    , dueDate     :: Maybe Text
    , tags        :: [Text]
    }
    deriving (Generic, Show, FromDhall, ToDhall)


