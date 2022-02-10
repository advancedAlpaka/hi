{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module HW3.Base
  where
import Codec.Serialise.Class (Serialise)
import Control.Exception (throw, throwIO)
import Control.Monad.Reader (ReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map.Strict (Map)
import Data.Ratio (Rational)
import Data.Sequence (Seq, fromList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import GHC.Real ()
import HW3.Action (HIO (HIO), HiPermission (..), PermissionException (..))
import System.Directory (Permissions, createDirectoryIfMissing, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert deriving (Eq, Ord, Generic)  -- function names (e.g. div, sort, length, ...)

instance Show HiFun where
  show fun = case fun of
      HiFunDiv            -> "div"
      HiFunMul            -> "mul"
      HiFunAdd            -> "add"
      HiFunSub            -> "sub"
      HiFunNot            -> "not"
      HiFunAnd            -> "and"
      HiFunOr             -> "or"
      HiFunLessThan       -> "less-than"
      HiFunGreaterThan    -> "greater-than"
      HiFunEquals         -> "equals"
      HiFunNotLessThan    -> "not-less-than"
      HiFunNotGreaterThan -> "not-greater-than"
      HiFunNotEquals      -> "not-equals"
      HiFunIf             -> "if"
      HiFunLength         -> "length"
      HiFunToUpper        -> "to-upper"
      HiFunToLower        -> "to-lower"
      HiFunReverse        -> "reverse"
      HiFunTrim           -> "trim"
      HiFunList           -> "list"
      HiFunRange          -> "range"
      HiFunFold           -> "fold"
      HiFunPackBytes      -> "pack-bytes"
      HiFunUnpackBytes    -> "unpack-bytes"
      HiFunEncodeUtf8     -> "encode-utf8"
      HiFunDecodeUtf8     -> "decode-utf8"
      HiFunZip            -> "zip"
      HiFunUnzip          -> "unzip"
      HiFunSerialise      -> "serialise"
      HiFunDeserialise    -> "deserialise"
      HiFunRead           -> "read"
      HiFunWrite          -> "write"
      HiFunMkDir          -> "mkdir"
      HiFunChDir          -> "cd"
      HiFunParseTime      -> "parse-time"
      HiFunRand           -> "rand"
      HiFunEcho           -> "echo"
      HiFunCount          -> "count"
      HiFunKeys           -> "keys"
      HiFunValues         -> "values"
      HiFunInvert         -> "invert"

instance Serialise HiFun

data HiValue =
    HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue) deriving (Show,Ord,Eq,Generic)  -- values (numbers, booleans, strings, ...)

instance Serialise HiValue

data HiExpr =
    HiExprValue HiValue
  | HiExprRun HiExpr
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)] deriving (Show, Eq)  -- expressions (literals, function calls, ...)

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero deriving (Show,Eq)  -- evaluation errors (invalid arguments, ...)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text deriving (Ord,Eq,Generic)

instance Show HiAction where
  show act = case act of
    HiActionRead _    -> "read"
    HiActionWrite _ _ -> "write"
    HiActionMkDir _   -> "mkdir"
    HiActionChDir _   -> "cd"
    HiActionCwd       -> "cwd"
    HiActionNow       -> "now"
    HiActionRand _ _  -> "rand"
    HiActionEcho _    -> "echo"

instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

checkPerm :: HiPermission -> IO HiValue -> HIO HiValue
checkPerm perm next = HIO $ \perms -> do
  if perm `elem` perms
    then next
    else throwIO $ PermissionRequired perm
instance Monad HIO => HiMonad HIO where
  runAction act = case act of
    HiActionRead s -> checkPerm AllowRead $ do
      isFile <- doesFileExist s
      if isFile
        then do
          resBytes <- B.readFile s
          return $ case decodeUtf8' resBytes of
            Left _    -> HiValueBytes resBytes
            Right txt -> HiValueString txt
        else do
          res <- listDirectory s
          return $ HiValueList $ fromList $ Prelude.map (HiValueString . T.pack) res
    HiActionWrite s bs -> checkPerm AllowWrite $ do
      B.writeFile s bs
      return HiValueNull
    HiActionMkDir s -> checkPerm AllowWrite $ do
      createDirectoryIfMissing True s
      return HiValueNull
    HiActionChDir s -> checkPerm AllowRead $ do
      setCurrentDirectory s
      return HiValueNull
    HiActionCwd -> checkPerm AllowRead $
      HiValueString . T.pack <$> getCurrentDirectory
    HiActionNow -> checkPerm AllowTime $
      HiValueTime <$> getCurrentTime
    HiActionRand l r -> HIO \_ -> do
      HiValueNumber . toRational <$> getStdRandom (uniformR (l, r))
    HiActionEcho t -> checkPerm AllowWrite $ do
      print t
      return HiValueNull




