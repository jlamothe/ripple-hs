-- Ripple Tools
-- Copyright (C) 2015 Jonathan Lamothe <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Network.Ripple ( Account (..)
                      , Balance (..)
                      , buildAccount
                      , identURI
                      , getAccount
                      , getAccountFromData
                      , balancesURI
                      , getBalances
                      , getBalancesFromData
                      ) where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Safe
import Text.JSON ( JSValue (..)
                 , JSString
                 , JSObject
                 , fromJSString
                 , fromJSObject
                 )
import qualified Text.JSON as JSON

data Account = Account { address :: String
                       , username :: Maybe String
                       , identity :: Maybe String
                       } deriving (Eq, Show)

data Balance = Balance { balanceValue :: Rational
                       , balanceCurrency :: String
                       , balanceAccount :: Maybe Account
                       } deriving (Eq, Show)

buildAccount :: String -> Account
buildAccount addr = Account addr Nothing Nothing

identURI :: String -> String
identURI ident = "https://id.ripple.com/v1/user/" ++ ident

getAccount :: String -> IO (Maybe Account)
getAccount ident =
  getFromURI (identURI ident) >>= return . getAccountFromData

getAccountFromData :: String -> Maybe Account
getAccountFromData rawJSON = case JSON.decode rawJSON of
  JSON.Ok (JSObject obj) -> case addr obj of
    Just addr' -> Just (buildAccount addr') { username = uname obj }
    _          -> Nothing
  _ -> Nothing
  where
    addr = getStringFromObj "address"
    uname = getStringFromObj "username"

balancesURI :: Account -> String
balancesURI account =
  "https://api.ripple.com/v1/accounts/" ++ address account ++ "/balances"

getBalances :: Account -> IO [Balance]
getBalances acct = getFromURI (balancesURI acct) >>= return . getBalancesFromData

getBalancesFromData :: String -> [Balance]
getBalancesFromData rawJSON = case JSON.decode rawJSON of
  JSON.Ok (JSObject obj) -> getBalancesFromArray $ getArrayFromObj "balances" obj
  _                      -> []

getBalancesFromArray :: [JSValue] -> [Balance]
getBalancesFromArray = catMaybes . map jsvalToBalance

jsvalToBalance :: JSValue -> Maybe Balance
jsvalToBalance (JSObject obj) = do
  valueString <- getStringFromObj "value" obj
  value <- stringToRational valueString
  currency <- getStringFromObj "currency" obj
  counterparty <- getStringFromObj "counterparty" obj
  if counterparty == ""
    then Just $ Balance value currency Nothing
    else Just $ Balance value currency (Just $ buildAccount counterparty)
jsvalToBalance _ = Nothing

getFromURI :: String -> IO String
getFromURI uri = simpleHTTP (getRequest uri) >>= getResponseBody

stringToRational :: String -> Maybe Rational
stringToRational str =
  (readMay str :: Maybe Double) >>= Just . toRational

getStringFromObj :: String -> JSObject JSValue -> Maybe String
getStringFromObj key obj = case getElemFromObj key obj of
  Just (JSString x) -> Just $ fromJSString x
  _                 -> Nothing

getArrayFromObj :: String -> JSObject JSValue -> [JSValue]
getArrayFromObj key obj = case getElemFromObj key obj of
    Just (JSArray xs) -> xs
    _                 -> []

getElemFromObj :: String -> JSObject JSValue -> Maybe JSValue
getElemFromObj key = Map.lookup key . Map.fromList . fromJSObject

-- jl
