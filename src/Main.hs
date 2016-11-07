{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens (to, only,(^?),ix, toListOf, Fold, filtered, lengthOf)
import Data.HashMap.Strict
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, strip)
import qualified Data.Text as T (drop)
import Data.Text.Read
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, elements, children, contents,allNamed, allAttributed, named, content)
import Text.Taggy.DOM
import Data.Monoid

import Debug.Trace

scoreRow :: Fold Element Element
scoreRow = filtered (\e -> lengthOf (allNamed (only "td")) e == 4)

data Score = Score {
  team1name :: Text
  , team2name :: Text
  , team1score :: Int
  , team2score :: Int } deriving Show

printScore :: Score -> String
printScore (Score t1 t2 s1 s2) =
  show t1 <> " " <> show s1 <> " - " <> show s2 <> " " <> show t2

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


scoreRowToResult :: [Node] -> Maybe Score
scoreRowToResult row = do
  team1 <- row ^? ix 1 . allNamed (only "a") . children . traverse . content
  score <- row ^? ix 2 . allNamed (only "span") . children . traverse . content
  team2 <- row ^? ix 3 . allNamed (only "a") . children . traverse . content
  (t1s, t2s) <- splitScore score
  return $ Score team1 team2 t1s t2s


recentPackages :: Response ByteString -> [Maybe Score]
  --[Text.Taggy.DOM.Element]
recentPackages = toListOf
               $ responseBody . to (decodeUtf8With lenientDecode)
               . html . allAttributed getInfoTable . allNamed (only "tr")
               . scoreRow . children . to scoreRowToResult

getInfoTable :: Fold (HashMap Text Text) ()
getInfoTable = ix "class" . only "info-table"

url1 =
  "http://www.bucs.org.uk//bucscore/Results.aspx?id=4604&ResultType=league"

url =
  "http://www.bucs.org.uk/bucscore/Results.aspx?id=5634&ResultType=league"

run = get url >>= return . recentPackages

main :: IO ()
main = get url >>= print <$> recentPackages
