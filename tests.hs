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

module Main (main) where

import Data.List
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (..), Test (..), runTestTT, (@=?))

import qualified Network.Ripple as Ripple

main :: IO ()
main = runTestTT tests >>= checkCounts

tests = TestList [ buildAccountTests addr
                 , identURITest
                 , getAccountFromDataTests addr uname
                 , balancesURITest
                 , getBalancesFromDataTests
                 ]
  where
    addr = "rs0m34cc0unt"
    uname = "username"

buildAccountTests :: String -> Test
buildAccountTests addr = TestLabel "buildAccount" $ TestList
  [ TestLabel ("address should be " ++ addr) $
    TestCase $ addr @=? Ripple.address account
  , TestLabel "username should be Nothing" $
    TestCase $ Nothing @=? Ripple.username account
  , TestLabel "identity should be Nothing" $
    TestCase $ Nothing @=? Ripple.identity account
  ]
  where account = Ripple.buildAccount addr

identURITest = TestLabel "identURI" $
  TestCase $ "https://id.ripple.com/v1/user/foo" @=? Ripple.identURI "foo"

getAccountFromDataTests :: String -> String -> Test
getAccountFromDataTests addr uname = TestLabel "getAccountFromData" $ TestList
  [ TestLabel "invalid response" $
    TestCase $ Nothing @=? Ripple.getAccountFromData "asdf"
  , TestLabel "unexpected JSON" $
    TestCase $ Nothing @=? Ripple.getAccountFromData "[]"
  , TestLabel "missing address in response" $
    TestCase $ Nothing @=? missingAddressResult
  , TestLabel "missing username in response" $
    accountTests addr Nothing Nothing missingUsernameResult
  , TestLabel "valid response" $
    accountTests addr (Just uname) Nothing validResult
  ]
  where
    missingAddressResult = Ripple.getAccountFromData $
      "{ \"usernsme\": \"" ++ uname ++ "\" }"
    missingUsernameResult = Ripple.getAccountFromData $
      "{ \"address\": \"" ++ addr ++ "\" }"
    validResult = Ripple.getAccountFromData $
      "{ \"username\": \"" ++ uname ++ "\", " ++
      "\"address\": \"" ++ addr ++ "\" }"

balancesURITest = TestLabel "balancesURI" $
  TestCase $ "https://api.ripple.com/v1/accounts/foo/balances" @=?
  Ripple.balancesURI (Ripple.buildAccount "foo")

getBalancesFromDataTests = TestLabel "getBalancesFromData" $ TestList
  [ TestLabel "invalid response" $
    TestCase $ [] @=? Ripple.getBalancesFromData "asdf"
  , TestLabel "unexpected JSON" $
    TestCase $ [] @=? Ripple.getBalancesFromData "[]"
  , TestLabel "no balances" $
    TestCase $ [] @=? emptyBalanceResult
  , TestLabel "with balances" $
    TestCase $ balances @=? withBalancesResult
  ]
  where
    emptyBalanceResult = Ripple.getBalancesFromData $
      balancesData []
    withBalancesResult = Ripple.getBalancesFromData $ balancesData balances
    balancesData xs = "{ \"balances\": [" ++ joinBalances xs ++ "] }"
    joinBalances xs = concat $ intersperse ", " $ map balanceText xs
    balanceText bal =
      "{ \"value\": \"" ++ (show . fromRational . Ripple.balanceValue) bal ++ "\", " ++
      "\"currency\": \"" ++ Ripple.balanceCurrency bal ++ "\", " ++
      "\"counterparty\": \"" ++ counterpartyText bal ++ "\" }"
    counterpartyText bal = case Ripple.balanceAccount bal of
      Nothing   -> ""
      Just acct -> Ripple.address acct
    balances = [ Ripple.Balance 100 "XRP" Nothing
               , Ripple.Balance 25 "CAD" $ Just $ Ripple.buildAccount "foo"
               ]

accountTests :: String -> Maybe String -> Maybe String -> Maybe Ripple.Account -> Test
accountTests addr uname ident account = TestList
  [ TestLabel ("address should be " ++ addr) $
    TestCase $ Just addr @=? fmap Ripple.address account
  , TestLabel ("username should be " ++ show uname) $
    TestCase $ Just uname @=? fmap Ripple.username account
  , TestLabel ("identity should be " ++ show ident) $
    TestCase $ Just ident @=? fmap Ripple.identity account
  ]

checkCounts :: Counts -> IO ()
checkCounts counts = if (errors counts > 0 || failures counts > 0)
  then exitFailure
  else exitSuccess

-- jl
