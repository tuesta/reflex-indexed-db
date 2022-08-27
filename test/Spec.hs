{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import qualified Data.Map               as M
import           Reflex.Dom             hiding (Delete, Insert)
import           Reflex.IDB

testIDB :: IO ()
testIDB = mainWidget $ do
  db <- indexedDB
           "db-test" 1
           [ CreateStore "objectStore-test"
           $ fmap Insert
           $ M.fromList
           $ zip (KeyString "last" : fmap KeyInt [1..7]) ['a'..'z']
           , DeleteStore mempty
           ] never
  getEv <- button "get"
  _   <- runModifyStore @_ @_ @Char db "objectStore-test" getEv (\_ -> M.singleton (KeyInt 3) Delete)
  res <- runReduceStore @_ @_ @Char db "objectStore-test" getEv (\_ -> ToMap)
  holdDyn "no added" (fmap show res) >>= display
  pure ()

main :: IO ()
main = testIDB
