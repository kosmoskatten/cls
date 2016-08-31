{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Data.Aeson (FromJSON, decode)
import Data.List ((\\), nub)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative
import Network.Nats ( Nats, MsgQueue, withNats
                    , defaultSettings, subscribe, nextMsg
                    )

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map

-- | Recipe how to launch a service, and how to watch that it is
-- up and running.
data Recipe = Recipe
  { start :: ![Component]
  -- ^ The list of components needed to implement the service.
 
  , watch :: ![Service]
  -- ^ The list of services that shall have been registered as the result.
  } deriving (Generic, Show)

instance FromJSON Recipe

type Component  = Text
type Service    = Text
type ServiceSet = ([Component], [Service])
type Recipes    = Map Service Recipe

data Options = Options
  { natsUri    :: !String
  , recipePath :: !FilePath
  } deriving Show

data Self = Self
  { nats       :: !Nats
  , queue      :: !MsgQueue
  , recipes    :: !Recipes
  , serviceSet :: !ServiceSet
  }

main :: IO ()
main = do
  opts    <- getOptions
  recipes <- loadRecipes $ recipePath opts
  case recipes of
    Just rs -> withNats defaultSettings [natsUri opts] $ \n -> do
      (_, q) <- subscribe n "system.cls.apply-start" Nothing
      app $ Self { nats       = n
                 , queue      = q
                 , recipes    = rs
                 , serviceSet = newServiceSet
                 }
    Nothing -> putStrLn "Cannot read/parse the recipes."

app :: Self -> IO ()
app self = do
  msg <- nextMsg $ queue self
  print msg
  app self

loadRecipes :: FilePath -> IO (Maybe Recipes)
loadRecipes file = decode <$> LBS.readFile file

findDeltas :: ServiceSet -> ServiceSet -> ServiceSet
findDeltas (newC, newS) (oldC, oldS) = (newC \\ oldC, newS \\ oldS)

newServiceSet :: ServiceSet
newServiceSet = ([], [])

getServiceSet :: [Service] -> Recipes -> Maybe ServiceSet
getServiceSet ss rs = nubRecipes <$> getRecipes ss rs

getRecipes :: [Service] -> Recipes -> Maybe [Recipe]
getRecipes ss rs = sequence $ map (lookup' rs) ss
  where lookup' = flip Map.lookup

nubRecipes :: [Recipe] -> ServiceSet
nubRecipes rs = (nub $ concatMap start rs, nub $ concatMap watch rs)

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
  info (helper <*> optParser)
    ( fullDesc
      <> progDesc "Start the demo CLS."
      <> header "CSIM Component Launcher Service"
    )

optParser :: Parser Options
optParser =
  Options <$> option auto
    ( long "nats"
      <> short 'n'
      <> metavar "<NATS URI>"
      <> help "NATS URI for connecting to the NATS."
      <> value "nats://localhost:4222"
    )
          <*>  strOption
    ( long "recipes"
      <> short 'r'
      <> metavar "<Recipe file>"
      <> help "Recipes configuration file path."
    )
