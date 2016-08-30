{-# LANGUAGE DeriveGeneric #-}
module Main
  ( main
  ) where

import Data.Aeson (FromJSON, decode)
import Data.List ((\\), nub)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

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

main :: IO ()
main = putStrLn "Foo"

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