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
                      , buildAccount
                      , identURI
                      , getAccount
                      , getAccountFromData
                      , balancesURI
                      , getBalances
                      , getBalancesFromData
                      ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
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

data Balance = Balance deriving (Eq, Show)

buildAccount :: String -> Account
buildAccount addr = Account addr Nothing Nothing

identURI :: String -> String
identURI ident = "https://id.ripple.com/v1/user/" ++ ident

getAccount :: String -> IO (Maybe Account)
getAccount ident =
  get (identURI ident) >>= return . getAccountFromData

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
getBalances acct = get (balancesURI acct) >>= return . getBalancesFromData

getBalancesFromData :: String -> [Balance]
getBalancesFromData _ = []

get :: String -> IO String
get uri = simpleHTTP (getRequest uri) >>= getResponseBody

getStringFromObj :: String -> JSObject JSValue -> Maybe String
getStringFromObj key obj = do
  val <- (Map.lookup key . Map.fromList . fromJSObject) obj
  case val of
    JSString x -> Just $ fromJSString x
    _          -> Nothing

-- jl
