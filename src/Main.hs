{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}


module Main (main, initialiseTeams) where

import Control.Lens (to, only,(^?),ix, toListOf, filtered
                    , lengthOf, has, (^.), _Just, Traversal')
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T (drop, intercalate, uncons, break
                                , lines, length, pack, append)
import qualified Data.Text.IO as T (readFile)
import Data.Text.Read
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, children, contents,allNamed, allAttributed, content, attrs)
import Text.Taggy.DOM
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

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import qualified Data.ByteString as BS (ByteString)


databaseConfig :: BS.ByteString
databaseConfig = "host='localhost' port=5432 dbname='ultimate'"

mkConnection :: IO Connection
mkConnection = connectPostgreSQL databaseConfig


data Score = Score {
    _leagueName :: Text
  , _date :: Text
  , _team1name :: TeamId
  , _team2name :: TeamId
  , _score :: (Int, Int)
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

scoreRow :: Traversal' Element Element
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
    groupedWithDate _acc Nothing [] = []
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


getInfoTable :: Traversal' (HashMap Text Text) ()
getInfoTable = ix "class" . only "info-table"

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


teams :: IO [Team]
teams = do
  ls <- T.lines <$> T.readFile "team-info.csv"
  return $ map mkTeam ls

mkTeam :: Text -> Team
mkTeam s =
  let p = T.break (==',')
      (teamname, s') = p s
      (twitter, s'') = p (T.drop 1 s')
      (n, s''') = p (T.drop 1 s'')
      teamid = strip (T.drop 1 s''')
      checkTwitter t@(T.uncons -> Just ('@',_)) = Just (TwitterHandle t)
      checkTwitter _ = Nothing
  in Team teamname (TeamId (read $ unpack teamid))
                   (checkTwitter twitter) (read (unpack n))


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

calculuateNewResults :: [Score] -> [Score] -> [Score]
calculuateNewResults oldscores newscores = newscores \\ oldscores

postResult :: IntMap Team -> Score -> IO ()
postResult ts = traceShowM . formatScore ts

formatScore :: IntMap Team -> Score -> Text
formatScore ts (Score league _date team1 team2 (s1, s2)) =
  T.intercalate " " [ "Ultimate Result:"
                    , T.append league ":", getTwitterDisplay team1
                    , T.pack (show s1)
                    , "-"
                    , T.pack (show s2)
                    , getTwitterDisplay team2
                    , "#BUCSWednesday"]
  where
    getTwitterDisplay :: TeamId -> Text
    getTwitterDisplay tid =
      let Team{teamTwitter, teamName, teamNumber} = ts ! getTeamId tid
      in case teamTwitter of
           Nothing -> teamName
           Just twitter -> T.intercalate " " [getTwitter twitter
                                             , teamNumberToIndicator teamNumber]

    teamNumberToIndicator :: Int -> Text
    teamNumberToIndicator n =
      case n of
        1 -> "1st"
        2 -> "2nd"
        3 -> "3rd"
        4 -> "4th"
        5 -> "5th"
        _ -> "6th"


main :: IO ()
main = do
        c <- mkConnection
        ts <- getTeams c

        records <- getDBScores c
        results <- nub <$> getAllTeamScores ts
        let new_results = calculuateNewResults records results
--        insertIntoDB c new_results
        mapM_ print new_results
        let teamMap = IntMap.fromList (map (\t -> (getTeamId (teamId t), t)) ts)
        mapM_ (\s -> postResult teamMap s >> threadDelay 1000000) new_results

