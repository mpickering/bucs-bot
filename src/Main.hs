{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}


module Main (main) where

import Control.Lens (to, only,(^?),ix, toListOf, Fold, filtered
                    , lengthOf, has, view, (^.), _Just)
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T (drop, concat, intercalate, uncons, break, lines, take, length)
import qualified Data.Text.IO as T (writeFile, readFile)
import Data.Text.Read
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, elements, children, contents,allNamed, allAttributed, named, content, attrs)
import Text.Taggy.DOM
import Data.Monoid
import Control.Concurrent
import Control.Monad (forM, void)
import Data.List

import Debug.Trace
import Common

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import GHC.Generics ( Generic)


databaseConfig = "host='localhost' port=5432 dbname='ultimate'"

mkConnection :: IO Connection
mkConnection = connectPostgreSQL databaseConfig


data Score = Score {
    leagueName :: Text
  , date :: Text
  , team1name :: TeamId
  , team2name :: TeamId
  , score :: (Int, Int)
   } deriving (Eq, Show)

instance ToRow Score where
  toRow (Score a b c d (e, f)) =
    [toField a, toField b, toField c, toField d, toField e, toField f]

instance FromRow Score where
  fromRow = do
    pres <- Score <$> field <*> field <*> field <*> field
    score <- (,) <$> field <*> field
    return (pres score)


-- Must only insert new values
insertIntoDB :: Connection -> [Score] -> IO ()
insertIntoDB c ss =
  () <$ executeMany c "INSERT INTO scores values (?,?,?,?,?,?)" ss

-- Get all the scores, there will never be that many so just get them all.
getDBScores :: Connection -> IO [Score]
getDBScores c = query_ c "SELECT * FROM scores"


printScore :: Score -> String
printScore (Score l d t1 t2 s1) =
  show l <> show d <> show t1 <> " " <> show s1 <> " " <> show t2

eitherToMaybe :: Either s a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

splitScore :: Text -> Maybe (Int, Int)
splitScore t =
  eitherToMaybe $
    let tscore = strip t
    in do
      (s1, r) <- decimal tscore
      (s2, "") <- decimal (T.drop 3 r) -- drop 3 for -
      return (s1, s2)

scoreRow :: Fold Element Element
scoreRow = filtered (\e -> lengthOf (allNamed (only "td")) e == 4)

scoreRowToResult :: Text -> [Node] -> Maybe Score
scoreRowToResult date row = do
  league <- row ^? ix 0 . allNamed (only "img") . attrs . ix "title"
  team1 <- row ^? ix 1 . allNamed (only "a") . attrs . ix "href" . to urlToTeamId
  score <- row ^? ix 2 . allNamed (only "span") . children . traverse . content
  team2 <- row ^? ix 3 . allNamed (only "a") . attrs . ix "href" . to urlToTeamId
  pscore <- splitScore score
  return $ Score league date team1 team2 pscore

-- We pass in a URL and pick out the team id
urlToTeamId :: Text -> TeamId
urlToTeamId = TeamId . read . take 4 . drop (T.length sampleUrl) . unpack
 where
   sampleUrl = "TeamProfile.aspx?id="


teamScores :: Response ByteString -> [Score]
  --[Text.Taggy.DOM.Element]
teamScores =
  let
    tableRows :: Response ByteString -> [Element]
    tableRows =
      toListOf
        $ responseBody . to (decodeUtf8With lenientDecode)
        . html . allAttributed getInfoTable . allNamed (only "tr")

    getDate :: Element -> Text
    getDate e = e ^. allNamed (only "em") . contents

    groupedWithDate :: [Element] -> Maybe Text -> [Element] -> [(Text, [Element])]
    groupedWithDate acc Nothing [] = []
    groupedWithDate acc (Just hdr) [] = [(hdr, reverse acc)]
    groupedWithDate acc hdr (e:es)
      -- See header row
      | has (attrs . ix "class") e
      , Just thdr <- hdr =
          (thdr, reverse acc) : groupedWithDate [] (Just (getDate e)) es
      -- Initialisation
      | has (attrs . ix "class") e
      , Nothing <- hdr , null acc =
          groupedWithDate [] (Just (getDate e)) es
      | Nothing <- hdr = trace "No header row" (groupedWithDate es hdr acc)
      | otherwise = groupedWithDate (e:acc) hdr es
    processScoreRows :: Text -> [Element] -> [Score]
    processScoreRows t es =
      toListOf (traverse . scoreRow . children . to (scoreRowToResult t) . _Just) es
  in
    concatMap (uncurry processScoreRows) . groupedWithDate [] Nothing  . tableRows






--               . scoreRow . children . to scoreRowToResult

getInfoTable :: Fold (HashMap Text Text) ()
getInfoTable = ix "class" . only "info-table"

url1 =
  "http://www.bucs.org.uk//bucscore/Results.aspx?id=4604&ResultType=league"


data League = L Text Text deriving Show

newtype TeamId = TeamId { getTeamId :: Int } deriving (Generic, Show, Eq)

instance ToField TwitterHandle where
    toField = toField . getTwitter
instance ToField TeamId where
    toField = toField . getTeamId

instance FromField TwitterHandle where
    fromField a b = TwitterHandle <$> (fromField a b)
instance FromField TeamId where
    fromField a b = TeamId <$> (fromField a b)

newtype TwitterHandle = TwitterHandle { getTwitter :: Text } deriving (Generic, Show)

data Team = Team {
    teamName :: Text --
  , teamId   :: TeamId
  , teamTwitter :: Maybe TwitterHandle
  , teamNumber :: Int --Whether 1s 2s etc
  } deriving (Generic, Show)

instance ToRow Team
instance FromRow Team

insertTeams :: Connection -> [Team] -> IO ()
insertTeams c = void . executeMany c "INSERT INTO teams VALUES (?,?,?,?)"

getTeams :: Connection -> IO [Team]
getTeams c = query_ c "SELECT * FROM teams"

initialiseTeams :: IO ()
initialiseTeams = do
  c <- mkConnection
  teams >>= insertTeams c
  close c



leagueUrls :: [League]
leagueUrls = [
   (L "Northern 1A" "5485")
 , (L "Midlands 1A" "5486")
 , (L "Western 1A" "5487")
 , (L "South Eastern 1A" "5488")
 , (L "Northern 2A" "5489")
 , (L "Northern 2B" "5490")
 , (L "Midlands 2A" "5491")
 , (L "Midlands 2B" "5492")
 , (L "Western 2A" "5493")
 , (L "Western 2B" "5494")
 , (L "South Eastern 2A" "5495")
 , (L "South Eastern 2B" "5496")
 , (L "Scottish 1A" "5634")
 ]

teams :: IO [Team]
teams = do
  ls <- T.lines <$> T.readFile "team-info.csv"
  return $ map mkTeam ls

mkTeam :: Text -> Team
mkTeam s =
  let p = T.break (==',')
      (name, s') = p s
      (twitter, s'') = p (T.drop 1 s')
      (n, s''') = p (T.drop 1 s'')
      teamid = strip (T.drop 1 s''')
      checkTwitter s@(T.uncons -> Just ('@',_)) = Just (TwitterHandle s)
      checkTwitter _ = Nothing
  in Team name (TeamId (read $ unpack teamid))  (checkTwitter twitter) (read (unpack n))


mkLeagueUrl :: Team -> String
mkLeagueUrl t =
  concat ["http://www.bucs.org.uk/bucscore/Results.aspx?id="
         , show (getTeamId (teamId t))
         , "&ResultType=team"]

getAllTeamScores :: [Team] -> IO [Score]
getAllTeamScores ts =
  concat <$> (forM ts $ \t -> do
    traceShowM ("Processing", t)
    threadDelay 1000000
    teamScores <$> get (mkLeagueUrl t))

--run = get (mkLeagueUrl (head leagueUrls))  >>= return . leagueScores (L "" "")

main :: IO ()
main = return ()

calculuateNewResults :: [Score] -> [Score] -> [Score]
calculuateNewResults oldscores newscores = newscores \\ oldscores

postResult :: Score -> IO ()
postResult = postMessage . formatScore

formatScore :: Score -> Text
formatScore = undefined

realmain = do
  forkIO $ mainLoop
  threadDelay (floor 3.6e9)

mainLoop :: IO ()
mainLoop = do
        c <- mkConnection
        ts <- getTeams c

        records <- getDBScores c
        results <- nub <$> getAllTeamScores ts
        let new_results = calculuateNewResults records results
        insertIntoDB c new_results
--        mapM_ print new_results
--        mapM_ postResult new_results
        return ()


populateDatabase :: IO ()



--- TODO
--Make a team table and FromRow, ToRow instances
--Insert all teams into table
--update scores to take teams as arguments
--update scores table for teams foreign key
--dedup score lookups as the will appear twice, once for each team.
--
