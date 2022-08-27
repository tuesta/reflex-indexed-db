{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Reflex.IDB
  ( IDBRef(..), IndexedDBState(..), isClosed, IndexedDB(..)
  , ObjectStore(..)
  , ModifyStore(..)
  , ReduceStore(..)
  , Key(..)
  , CloseDB(..), IDBErr(..)
  , indexedDB
  , runModifyStore
  , runReduceStore
  ) where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe                     (fromMaybe)
import           Prelude                        hiding (lookup, (!!))

import           Control.Concurrent.STM         (atomically)
import qualified Control.Concurrent.STM.TMQueue as STM
import qualified Control.Concurrent.STM.TMVar   as STM


import           Data.Aeson                     (FromJSON (..), FromJSONKey,
                                                 Result (..), ToJSON (..),
                                                 ToJSONKey, Value (..),
                                                 fromJSON)
import           Data.Aeson.Types               (typeMismatch)
import qualified Data.Map                       as M
import           Data.Scientific
import qualified Data.Text                      as T

import           Reflex                         (current, ffor, holdDyn,
                                                 leftmost, newTriggerEvent,
                                                 performEvent, sample)
import           Reflex.Dom.Core                (Dynamic, Event, MonadSample,
                                                 MonadWidget)
import           Reflex.PerformEvent.Class      (Performable)

import qualified GHCJS.DOM                      as DOM
import qualified GHCJS.DOM.Enums                as DOM
import qualified GHCJS.DOM.EventM               as DOM
import qualified GHCJS.DOM.IDBDatabase          as IDBD
import qualified GHCJS.DOM.IDBFactory           as IDBF
import qualified GHCJS.DOM.IDBObjectStore       as IDBStore
import qualified GHCJS.DOM.IDBOpenDBRequest     as IDBOReq
import qualified GHCJS.DOM.IDBRequest           as IDBReq
import qualified GHCJS.DOM.IDBTransaction       as IDBTrans
import qualified GHCJS.DOM.Types                as DOM
import qualified GHCJS.DOM.Window               as FFIWin
import qualified JSDOM.Generated.IDBCursor      as IDBC

import qualified Data.JSString                  as JS
import qualified Language.Javascript.JSaddle    as JS

-- TODO handle errors properly

-- | Convert an applicative 'Maybe' to a panic
(!!) :: Applicative m => m (Maybe a) -> String -> m a
(!!) a e = fmap (fromMaybe (error e)) a

onBlocked :: DOM.JSM ()
onBlocked = pure ()

onError :: DOM.JSM ()
onError = pure ()

onSuccess :: (IndexedDBState -> IO ()) -> IndexedDBState -> DOM.JSM ()
onSuccess eOpenTrigger idbSt = liftIO (eOpenTrigger idbSt)

indexedDB
  :: forall t m v
   . (MonadWidget t m, ToJSON v, FromJSON v)
  => T.Text
  -> Int
  -> [ObjectStore v]
  -> Event t ()
  -> m (IndexedDB t)
indexedDB nameDB versionDB stores closeEv = do
  (eOpen   , eOpenTrigger)    <- newTriggerEvent
  (eUpgrade, eUpgradeTrigger) <- newTriggerEvent
  idbRef <- liftIO $ newIORef Nothing
  DOM.liftJSM $ do
    wind       <- DOM.currentWindow !! "Error getting current window"
    idbFact    <- FFIWin.getIndexedDB wind
    idbOpenReq <- IDBF.open idbFact nameDB (Just $ fromIntegral versionDB)

    -- IDBOpenDBRequest: blocked event
    void $ DOM.liftJSM $ DOM.on idbOpenReq IDBOReq.blocked $ DOM.liftJSM onBlocked
    -- IDBRequest: error event
    void $ DOM.liftJSM $ DOM.on idbOpenReq IDBReq.error    $ DOM.liftJSM onError
    -- IDBOpenDBRequest: upgradeneeded event
    void $ DOM.liftJSM $ DOM.on idbOpenReq IDBOReq.upgradeNeeded $ do
       idbReq :: IDBReq.IDBRequest <- DOM.target !! "Error getting idb request"
       idbAny                      <- IDBReq.getResultUnchecked idbReq
       idb                         <- DOM.unsafeCastTo IDBD.IDBDatabase idbAny
       mapM_ (runObjectStore idb) stores
       liftIO $ eUpgradeTrigger ()
    -- IDBRequest: success even
    void $ DOM.liftJSM $ DOM.on idbOpenReq IDBReq.success $ do
       tgt <- DOM.target
       let idbReq = maybe (error "Error getting idb request") id tgt :: IDBReq.IDBRequest
       idbAny <- IDBReq.getResult idbReq !! "Error getting Database"
       idb <- DOM.castTo IDBD.IDBDatabase idbAny !! "Error converting to database"
       liftIO $ writeIORef idbRef (Just idb)
       DOM.liftJSM $ onSuccess eOpenTrigger .  Open . IDBRef =<< liftIO (newIORef idb)

  closeE <- performEvent $ ffor closeEv $ \_ -> do
    idbM <- liftIO $ readIORef idbRef
    case (idbM :: Maybe DOM.IDBDatabase) of
      Just idb -> do
        _ <- IDBD.close idb
        liftIO $ modifyIORef idbRef (const Nothing)
        pure Close
      Nothing -> pure Close

  let mergedStatus = leftmost [closeE, eOpen]
      isClosedE    = fmap isClosed mergedStatus
      isOpenE      = fmap not isClosedE
  idbState <- holdDyn Close mergedStatus
  return $ IndexedDB isOpenE idbState eUpgrade

data Key = KeyInt Int | KeyString T.Text
  deriving (Eq, Ord, Show)

instance ToJSON Key where
  toJSON (KeyInt    n)   = Number (fromIntegral n)
  toJSON (KeyString str) = String str

instance FromJSON Key where
  parseJSON (Number s) = case floatingOrInteger @Double s of
    Left  _ -> fail $ show s <> ": invalid number"
    Right n -> pure (KeyInt n)
  parseJSON (String str) = pure (KeyString str)
  parseJSON v = typeMismatch "Integer | String" v

instance FromJSONKey Key where
instance ToJSONKey Key where

data IDBErr = IDBErr | DifferentType
  deriving Show

keyJSVal :: Key -> DOM.JSM DOM.JSVal
keyJSVal (KeyInt n)      = DOM.toJSVal n
keyJSVal (KeyString str) = DOM.toJSVal str

toIDBErr :: Result a -> Either IDBErr a
toIDBErr (Error   _) = Left DifferentType
toIDBErr (Success a) = Right a

toMaybe :: Result a -> Maybe a
toMaybe (Error   _) = Nothing
toMaybe (Success a) = Just a

getObjS
  :: forall m a b
   . ( DOM.MonadJSM m
     , FromJSON a
     )
  => DOM.IDBObjectStore
  -> Key
  -> (forall dom. DOM.MonadDOM dom => Either IDBErr (Maybe a) -> dom b)
  -> m b
getObjS objStore key f = do
  let k = keyJSVal key
  resultVar <- liftIO $ atomically $ STM.newEmptyTMVar
  req       <- IDBStore.get objStore k
  void $ DOM.liftJSM $ DOM.on req IDBReq.success $ do
    r  <- IDBReq.getResult req
    r' <- case r of
      Nothing -> pure (Left IDBErr)
      Just reqResult -> do
        DOM.liftJSM DOM.syncPoint
        DOM.liftJSM $ do
          value <- DOM.toJSVal reqResult
          fmap (Right Nothing `maybe` fmap Just)
            . (fmap . fmap) (toIDBErr . fromJSON)
            . DOM.fromJSVal
            $ value
    f r' >>= liftIO . atomically . STM.putTMVar resultVar
  liftIO (atomically (STM.readTMVar resultVar))

-- getAllObjS
--   :: forall m
--    . ( DOM.MonadJSM m
--      )
--   => DOM.IDBObjectStore
--   -> m (Either IDBErr (M.Map Key Value))
-- getAllObjS objStore = do
--   resultVar <- liftIO $ atomically $ STM.newEmptyTMVar
--   req       <- IDBStore.getAllRange objStore Nothing Nothing
--   void $ DOM.liftJSM $ DOM.on req IDBReq.success $ do
--     r  <- IDBReq.getResult req
--     r' <- case r of
--       Nothing        -> pure (Left IDBErr)
--       Just reqResult -> do
--         DOM.liftJSM DOM.syncPoint
--         DOM.liftJSM $ do
--           value <- DOM.toJSVal reqResult
--           fmap (Right M.empty `maybe` id)
--             . (fmap . fmap) (toIDBErr . fromJSON)
--             . DOM.fromJSVal
--             $ value
--     liftIO $ atomically $ STM.putTMVar resultVar r'
--   liftIO (atomically (STM.readTMVar resultVar))

getAllWithKeyObjS
  :: forall m a
   . ( DOM.MonadJSM m
     , FromJSON a
     )
  => DOM.IDBObjectStore
  -> m (M.Map Key a)
getAllWithKeyObjS objStore = do
  resultQueue <- liftIO STM.newTMQueueIO

  req <- IDBStore.openCursorRange objStore Nothing (Just DOM.IDBCursorDirectionNext)
  void $ DOM.liftJSM $ DOM.on req IDBReq.success $ do
    r  <- IDBReq.getResult req
    emkv <- case r of
      Nothing        -> pure $ Left ()
      Just reqResult -> do
        DOM.liftJSM DOM.syncPoint
        DOM.liftJSM $ do
          value  <- DOM.toJSVal reqResult
          obj    <- JS.makeObject value
          (cKey, cValue) <- (,) <$> JS.getProp (JS.pack "key") obj
                                <*> JS.getProp (JS.pack "value") obj
          (rKey, rValue) <- (,) <$> (fmap . fmap) fromJSON (DOM.fromJSVal cKey)
                                <*> (fmap . fmap) fromJSON (DOM.fromJSVal cValue)

          let mkv = (,) <$> (rKey >>= toMaybe) <*> (rValue >>= toMaybe)
          IDBC.continue (IDBC.IDBCursor value) (Nothing :: Maybe DOM.JSVal)
          pure $ Right mkv
    case emkv of
      Left    _ -> liftIO $ atomically $ STM.closeTMQueue resultQueue
      Right mkv -> liftIO $ atomically $ STM.writeTMQueue resultQueue mkv

  let loop m = do
        mi <- atomically $ STM.readTMQueue resultQueue
        case mi of
          Nothing  -> pure m
          Just mkv -> loop (maybe m (\(k, v) -> M.insert k v m) mkv)

  liftIO (loop mempty)

------------------------------------------------------------------------------
-- | IndexedDBState

newtype IDBRef = IDBRef {runIDBRef :: IORef IDBD.IDBDatabase}

data IndexedDBState
  = Open IDBRef
  | Close

isClosed :: IndexedDBState -> Bool
isClosed Close = True
isClosed _     = False

data IndexedDB t = IndexedDB
  { _idb_isOpen      :: Event t Bool
  , _idb_state       :: Dynamic t IndexedDBState
  , _idb_onUpgrading :: Event t ()
  }


------------------------------------------------------------------------------
-- | Object Store

data ObjectStore v where
  CreateStore :: T.Text -> M.Map Key (ModifyStore v) -> ObjectStore v
  DeleteStore :: T.Text                              -> ObjectStore v

runObjectStore
  :: ( DOM.MonadJSM m
     , ToJSON v, FromJSON v
     )
  => IDBD.IDBDatabase
  -> ObjectStore v
  -> m ()
runObjectStore idb (CreateStore name form) = do
  store <- IDBD.createObjectStore idb name (Nothing :: Maybe DOM.IDBObjectStoreParameters)
  void $ M.traverseWithKey (modifyStore store) form
  pure ()
runObjectStore idb (DeleteStore name) = IDBD.deleteObjectStore idb name

------------------------------------------------------------------------------
-- | Modify

data ModifyStore a
  = Insert a
  | Delete
  | Adjust (a -> a)
  | Update (a -> Maybe a)

-- TODO partially ignore errors
modifyStore
  :: forall m a
   . ( DOM.MonadJSM m
     , ToJSON a, FromJSON a
     )
  => DOM.IDBObjectStore
  -> Key
  -> ModifyStore a
  -> m (Either IDBErr ())
modifyStore objStore key mS = do
  let k = keyJSVal key
  case mS of
    Delete   -> do
      IDBStore.delete_ objStore k
      pure $ Right ()
    Insert a -> do
      void $ IDBStore.add objStore (toJSON a) (Just k)
      DOM.liftJSM DOM.syncPoint
      pure $ Right ()
    Adjust f -> do
      getObjS objStore key $ \case
        Left  err -> pure (Left err)
        Right ma  -> do
          maybe (pure ()) (\a -> IDBStore.put_ objStore (toJSON $ f a) (Just k)) ma
          pure $ Right ()
    Update f -> do
      getObjS objStore key $ \case
        Left  err -> pure (Left err)
        Right ma  -> do
          maybe (pure ())
                ( maybe (IDBStore.delete_ objStore k)
                        (\a -> IDBStore.put_ objStore (toJSON a) (Just k)))
                $ fmap f ma
          pure $ Right ()

------------------------------------------------------------------------------
-- | Reduce
data ReduceStore v a where
  Lookups  :: [Key] -> ReduceStore v (M.Map Key (Either IDBErr (Maybe v)))
  Lookup   ::  Key  -> ReduceStore v (Either IDBErr (Maybe v))
  ToMap    :: ReduceStore v (M.Map Key v)
  -- TODO this must be simple to implement
  -- FoldWithKey :: (Key -> v -> b -> b) -> b -> ReduceStore v b

reduceStore
  :: forall m v a
   . ( DOM.MonadJSM m
     , ToJSON v, FromJSON v
     )
  => DOM.IDBObjectStore
  -> ReduceStore v a
  -> m a
reduceStore objStore rS =
  case rS of
    Lookups ks -> do
      M.traverseWithKey
        (\k _ -> getObjS objStore k pure)
        (M.fromList $ zip ks (repeat ()))
    Lookup k  -> do
      getObjS objStore k pure
    ToMap -> getAllWithKeyObjS objStore

------------------------------------------------------------------------------
-- | Modify-Reduce

modifyWithStore
  :: ( DOM.MonadJSM m
     , ToJSON v, FromJSON v
     )
  => DOM.IDBDatabase
  -> T.Text
  -> M.Map Key (ModifyStore v)
  -> m (M.Map Key (Either IDBErr ()))
modifyWithStore idb nameStore mStore = do
  idbTrans <- IDBD.transaction idb [T.unpack nameStore] (Just DOM.IDBTransactionModeReadwrite)
  objStore <- IDBTrans.objectStore idbTrans nameStore
  M.traverseWithKey (modifyStore objStore) mStore

reduceWithStore
  :: ( DOM.MonadJSM m
     , ToJSON v, FromJSON v
     )
  => DOM.IDBDatabase
  -> T.Text
  -> ReduceStore v a
  -> m a
reduceWithStore idb nameStore rStore = do
  idbTrans <- IDBD.transaction idb [T.unpack nameStore] (Just DOM.IDBTransactionModeReadonly)
  objStore <- IDBTrans.objectStore idbTrans nameStore
  reduceStore objStore rStore

data CloseDB = CloseDB
  deriving Show

runWithStore
  :: MonadWidget t m
  => (DOM.IDBDatabase -> T.Text -> b -> Performable m o)
  -> IndexedDB t
  -> T.Text
  -> Event t a
  -> (a -> b)
  -> m (Event t (Either CloseDB o))
runWithStore run IndexedDB{_idb_state} nameStore trigger k = do
  performEvent $ ffor trigger $ \a -> do
    idbState <- sample $ current $ _idb_state
    case idbState of
      Close        -> pure $ Left CloseDB
      Open idbRef' -> do
        idb <- liftIO $ readIORef $ runIDBRef idbRef'
        fmap Right $ run idb nameStore (k a)

runModifyStore
  :: forall t m v a
   . ( MonadIO m, MonadSample t m, MonadWidget t m
     , ToJSON v, FromJSON v
     )
  => IndexedDB t
  -> T.Text
  -> Event t a
  -> (a -> M.Map Key (ModifyStore v))
  -> m (Event t (Either CloseDB (M.Map Key (Either IDBErr ()))))
runModifyStore = runWithStore modifyWithStore

runReduceStore
  :: forall t m v a b
   . ( MonadIO m, MonadSample t m, MonadWidget t m
     , ToJSON v, FromJSON v
     )
  => IndexedDB t
  -> T.Text
  -> Event t a
  -> (a -> ReduceStore v b)
  -> m (Event t (Either CloseDB b))
runReduceStore = runWithStore reduceWithStore
