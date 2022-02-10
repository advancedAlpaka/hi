{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW3.Evaluator
  ( eval
  ) where
import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
                               decompressWith, defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadError (throwError), runExcept,
                             runExceptT)
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.Map as M
import Data.Ratio (denominator, numerator, (%))
import Data.Sequence (index)
import qualified Data.Sequence as S
import Data.Set (Set, fromList, singleton)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (addUTCTime, diffUTCTime)
import GHC.Base (Semigroup (stimes))
import GHC.Real (Ratio ((:%)))
import HW3.Action (HIO (runHIO))
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction),
                 HiValue (..))
import qualified Text.Printf as T
import Text.Read (readMaybe)

class AsString a where
  slice :: Int -> Int -> a -> a
  len :: a -> Int
  toHiValue :: a -> HiValue
  reverseA :: a -> a

instance AsString Text where
  slice l r = T.drop l . T.take r
  len = T.length
  toHiValue = HiValueString
  reverseA = T.reverse

instance AsString (S.Seq HiValue) where
  slice l r = S.drop l . S.take r
  len = S.length
  toHiValue = HiValueList
  reverseA = S.reverse
instance AsString C.ByteString where
  slice l r = C.drop l . C.take r
  len = C.length
  toHiValue = HiValueBytes
  reverseA = C.reverse


arityFun :: HiFun -> Set Int
arityFun fun = singleton $ case fun of
  HiFunDiv            -> 2
  HiFunMul            -> 2
  HiFunAdd            -> 2
  HiFunSub            -> 2
  HiFunAnd            -> 2
  HiFunOr             -> 2
  HiFunLessThan       -> 2
  HiFunGreaterThan    -> 2
  HiFunEquals         -> 2
  HiFunNotLessThan    -> 2
  HiFunNotGreaterThan -> 2
  HiFunNotEquals      -> 2
  HiFunIf             -> 3
  HiFunList           -> -1
  HiFunRange          -> 2
  HiFunFold           -> 2
  HiFunWrite          -> 2
  HiFunRand           -> 2
  _                   -> 1

arity :: Monad m => HiValue -> ExceptT HiError m (Set Int)
arity (HiValueFunction f) = return $ arityFun f
arity (HiValueString _)   = return $ fromList [1, 2]
arity (HiValueList _)     = return $ fromList [1, 2]
arity (HiValueBytes _)    = return $ fromList [1, 2]
arity (HiValueDict _)     = return $ fromList [1]
arity _                   = throwError HiErrorInvalidFunction

tryInteger :: Monad m2 => Rational -> ExceptT HiError m2 Int
tryInteger (num :% den) = do
    if den == 1
      then return $ fromInteger num
      else throwError HiErrorInvalidArgument


evalBooleanFun,evalAriphmeticFun,evalStringFun,evalListFun,evalBytesFun,evalActionFun,evalDictFun,evalFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalAriphmeticFun HiFunDiv [HiValueNumber _, HiValueNumber 0] = throwError HiErrorDivideByZero
evalAriphmeticFun fun [HiValueNumber n1, HiValueNumber n2] = return $ HiValueNumber $ case fun of
  HiFunDiv -> n1 / n2
  HiFunAdd -> n1 + n2
  HiFunSub -> n1 - n2
  HiFunMul -> n1 * n2
  _        -> undefined
evalAriphmeticFun fun [HiValueString s1,HiValueString s2] | fun == HiFunDiv = return $ HiValueString $ T.concat [s1,T.pack "/",s2]
                                                          | fun == HiFunAdd = return $ HiValueString $ T.concat [s1, s2]
evalAriphmeticFun HiFunAdd [HiValueList l1,HiValueList l2] = return $ HiValueList $ l1 S.>< l2
evalAriphmeticFun HiFunAdd [HiValueBytes l1,HiValueBytes l2] = return $ HiValueBytes $ C.append l1 l2

evalAriphmeticFun HiFunMul [hv, HiValueNumber (num :% den)] =
  if den /= 1 || num <= 0
    then throwError HiErrorInvalidArgument
    else return $ case hv of
                HiValueString s -> helper' s num
                HiValueList l   -> helper' l num
                HiValueBytes l  -> helper' l num
                _               -> undefined
                where
    helper' :: (AsString a, Semigroup a) => a -> Integer -> HiValue
    helper' s num = toHiValue $ stimes num s

evalAriphmeticFun HiFunAdd [HiValueTime time, HiValueNumber n] = return $ HiValueTime $ addUTCTime (fromRational n) time
evalAriphmeticFun HiFunSub [HiValueTime time1, HiValueTime time2] = return $ HiValueNumber $ toRational $ diffUTCTime time1 time2
evalAriphmeticFun _ _ = throwError HiErrorInvalidArgument


evalBooleanFun fun [arg1, arg2] | fun == HiFunLessThan = return $ HiValueBool $ arg1 < arg2
                                | fun == HiFunGreaterThan = return $ HiValueBool $ arg1 > arg2
                                | fun == HiFunEquals = return $ HiValueBool $ arg1 == arg2
                                | fun == HiFunNotLessThan = return $ HiValueBool $ arg1 >= arg2
                                | fun == HiFunNotGreaterThan = return $ HiValueBool $ arg1 <= arg2
                                | fun == HiFunNotEquals = return $ HiValueBool $ arg1 /= arg2
evalBooleanFun HiFunNot [HiValueBool cond] = return $ HiValueBool $ not cond
evalBooleanFun _ _ = throwError HiErrorInvalidArgument


evalStringFun fun [HiValueString s] | fun == HiFunToUpper = return $ HiValueString $ T.toUpper s
                                    | fun == HiFunToLower = return $ HiValueString $ T.toLower s
                                    | fun == HiFunTrim = return $ HiValueString $ T.strip s
evalStringFun HiFunReverse [hv] = case hv of
                HiValueString s -> helperRev s 
                HiValueList l   -> helperRev l 
                HiValueBytes l  -> helperRev l 
                _               -> throwError HiErrorInvalidArgument
                where
      helperRev :: (AsString a,Monad m1) => a -> ExceptT HiError m1 HiValue
      helperRev = return . toHiValue . reverseA 
evalStringFun HiFunLength [hv] = case hv of
                HiValueString s -> helperLen s 
                HiValueList l   -> helperLen l 
                HiValueBytes l  -> helperLen l 
                _               -> throwError HiErrorInvalidArgument
                where
      helperLen :: (AsString a,Monad m1) => a -> ExceptT HiError m1 HiValue
      helperLen = return . HiValueNumber . toRational . len 
evalStringFun _ _ = throwError HiErrorInvalidArgument


evalListFun HiFunList elems = return $ HiValueList $ S.fromList elems
evalListFun HiFunFold [_,HiValueList S.Empty] = return HiValueNull
evalListFun HiFunFold [hvAsFun,HiValueList (fst S.:<| lst)] = helper hvAsFun fst lst
  where
    helper :: HiMonad m1 => HiValue -> HiValue -> S.Seq HiValue -> ExceptT HiError m1 HiValue
    helper _ first S.Empty = return first
    helper hvAsFun frst (scnd S.:<| list) = do
      newFirst <- eval' $ HiExprApply (HiExprValue hvAsFun) [HiExprValue frst,HiExprValue scnd]
      helper hvAsFun newFirst list
evalListFun HiFunRange [HiValueNumber r1, HiValueNumber r2] = return $ HiValueList $ S.fromList $ map HiValueNumber  [r1..r2]
evalListFun _ _ = throwError HiErrorInvalidArgument


evalBytesFun HiFunPackBytes [HiValueList S.Empty] = return HiValueNull
evalBytesFun HiFunPackBytes [HiValueList lst] = helper C.empty lst
  where
    helper :: Monad m1 => C.ByteString -> S.Seq HiValue -> ExceptT HiError m1 HiValue
    helper left S.Empty = return $ HiValueBytes left
    helper left ((HiValueNumber num) S.:<| list) = do
      new <- tryInteger num
      helper (C.snoc left $ toEnum new) list
    helper _ _ = throwError HiErrorInvalidArgument
evalBytesFun fun [HiValueBytes arr] | fun == HiFunUnpackBytes = return $ HiValueList $ S.fromList $ map (HiValueNumber . toRational . fromEnum) (C.unpack arr)
                                    | fun == HiFunDecodeUtf8 = return $ case T.decodeUtf8' arr of
                                                                          Left _ -> HiValueNull
                                                                          Right text -> HiValueString text
                                    | fun == HiFunZip = return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict arr
                                    | fun == HiFunUnzip = return $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams $ fromStrict arr
                                    | fun == HiFunDeserialise = case deserialiseOrFail $ fromStrict arr of
                                                            Left _ -> throwError HiErrorInvalidArgument
                                                            Right hv -> return hv
evalBytesFun HiFunEncodeUtf8 [HiValueString t] = return $ HiValueBytes $ T.encodeUtf8 t
evalBytesFun HiFunSerialise [hv] = return $ HiValueBytes $ toStrict $ serialise  hv
evalBytesFun _ _ = throwError HiErrorInvalidArgument


evalActionFun fun [HiValueString t] = do
  let path = T.unpack t
  return $ HiValueAction $ if | fun == HiFunRead  -> HiActionRead path
                              | fun == HiFunMkDir -> HiActionMkDir path
                              | fun == HiFunChDir -> HiActionChDir path
                              | otherwise         -> undefined
evalActionFun HiFunWrite [HiValueString p, HiValueString text] = return $ HiValueAction $ HiActionWrite (T.unpack p) (T.encodeUtf8 text)
evalActionFun HiFunWrite [HiValueString p, HiValueBytes str] = return $ HiValueAction $ HiActionWrite (T.unpack p) str
evalActionFun _ _ = throwError HiErrorInvalidArgument


helper :: (Monad m, Ord k1) => ((M.Map k1 HiValue -> t1 -> M.Map k1 HiValue) -> M.Map k2 a -> t2 -> M.Map HiValue HiValue) -> (t1 -> k1) -> t2 -> m HiValue
helper fold cons list = return $ HiValueDict $ fold (\oldMap ch -> M.insertWith plus (cons ch) (HiValueNumber $ 1 % 1) oldMap) M.empty list

plus :: HiValue -> HiValue -> HiValue
plus (HiValueNumber (r1 :% 1)) (HiValueNumber (r2 :% 1)) = HiValueNumber $ toRational (r1 + r2)
plus _ _                                                 = undefined


evalDictFun HiFunCount [hv] = case hv of
  HiValueString txt -> helper T.foldl (HiValueString . T.singleton) txt
  HiValueList seq   -> helper foldl id seq
  HiValueBytes bs   -> helper B.foldl (HiValueNumber . toRational) bs
  _                 -> throwError HiErrorInvalidArgument
evalDictFun HiFunKeys [HiValueDict dict] = return $ HiValueList $ S.fromList $ M.keys dict
evalDictFun HiFunValues [HiValueDict dict] = return $ HiValueList $ S.fromList $ M.elems dict
evalDictFun HiFunInvert [HiValueDict dict] = return $ HiValueDict $
  M.foldlWithKey
    (\old key val ->
      M.insertWith
        (\(HiValueList seq1) (HiValueList seq2) ->
          HiValueList $ seq1 S.>< seq2)
        val (HiValueList $ S.singleton key) old)
  M.empty dict
evalDictFun _ _ = throwError HiErrorInvalidArgument



evalFun HiFunEcho = \[HiValueString t] -> return $ HiValueAction $ HiActionEcho t
evalFun HiFunRand = \[HiValueNumber r1,HiValueNumber r2] -> do
  int1 <- tryInteger r1
  int2 <- tryInteger r2
  return $ HiValueAction $ HiActionRand int1 int2
evalFun HiFunParseTime = \[HiValueString t] -> return $ maybe HiValueNull HiValueTime (readMaybe $ T.unpack t)
evalFun fun | fun `elem` [HiFunDiv, HiFunMul, HiFunAdd, HiFunSub] = evalAriphmeticFun fun
            | fun `elem` [HiFunNot, HiFunAnd,
                          HiFunOr, HiFunLessThan,
                          HiFunGreaterThan, HiFunEquals,
                          HiFunNotLessThan, HiFunNotGreaterThan,
                          HiFunNotEquals, HiFunIf] = evalBooleanFun fun
            | fun `elem` [HiFunLength, HiFunToUpper, HiFunToLower,
                          HiFunReverse, HiFunTrim] = evalStringFun fun
            | fun `elem` [HiFunList, HiFunRange, HiFunFold] = evalListFun fun
            | fun `elem` [HiFunPackBytes, HiFunUnpackBytes, HiFunEncodeUtf8, HiFunDecodeUtf8,
                          HiFunZip, HiFunUnzip, HiFunSerialise, HiFunDeserialise] = evalBytesFun fun
            | fun `elem` [HiFunRead, HiFunWrite, HiFunMkDir, HiFunChDir] = evalActionFun fun
            | fun `elem` [HiFunCount, HiFunKeys, HiFunValues, HiFunInvert] = evalDictFun fun
            | otherwise = undefined

helperSlice :: (Monad m,AsString a) => a -> HiValue -> HiValue -> ExceptT HiError m HiValue
helperSlice s arg1 arg2 = case (arg1,arg2) of
      (HiValueNumber r1,HiValueNumber r2) -> helperSlice' s r1 r2
      (HiValueNumber r, HiValueNull)      -> helperSlice' s r 0
      (HiValueNull, HiValueNumber r)      -> helperSlice' s 0 r
      (HiValueNull, HiValueNull)          -> helperSlice' s 0 0
      _                                   -> throwError HiErrorInvalidArgument
      where
        helperSlice' :: (Monad m1,AsString a1) => a1 -> Rational -> Rational -> ExceptT HiError m1 HiValue
        helperSlice' s lRat rRat = do
              left <- tryInteger lRat
              right <- tryInteger rRat
              let l = len s
              let fix1 = mod (left + l) l
              let fix2 = mod (right + l) l
              return $ toHiValue $ slice (max left 0) (min right l) s

evalString :: Monad m => Text -> [HiValue] -> ExceptT HiError m HiValue
evalString s args = case args of
  [HiValueNumber r] -> do
    i <- tryInteger r
    if i < 0 || i >= T.length s
      then return HiValueNull
      else return $ HiValueString $ T.singleton (T.index s i)
  [hv1, hv2] -> helperSlice s hv1 hv2
  _ -> throwError HiErrorInvalidArgument

evalList :: Monad m => S.Seq HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalList l args = case args of
  [HiValueNumber r] -> do
    i <- tryInteger r
    if i < 0 || i >= S.length l
      then return HiValueNull
      else return (S.index l i)
  [hv1, hv2] -> helperSlice l hv1 hv2
  _ -> throwError HiErrorInvalidArgument

evalBytes :: Monad m => C.ByteString -> [HiValue] -> ExceptT HiError m HiValue
evalBytes l args = case args of
  [HiValueNumber r] -> do
    i <- tryInteger r
    if i < 0 || i >= C.length l
      then return HiValueNull
      else return $ HiValueNumber $ toRational $ fromEnum (C.index l i)
  [hv1, hv2] -> helperSlice l hv1 hv2
  _ -> throwError HiErrorInvalidArgument

evalValue :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalValue (HiValueFunction fun) = evalFun fun
evalValue (HiValueString s)     = evalString s
evalValue (HiValueList l)       = evalList l
evalValue (HiValueBytes bytes)  = evalBytes bytes
evalValue (HiValueDict map)     = \[hv] -> return $ maybe HiValueNull id $ map M.!? hv
evalValue _                     = \_ -> return HiValueNull

eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue val) = return val
eval' (HiExprRun run) = do
  actValue <- eval' run
  case actValue of
    (HiValueAction act) -> (ExceptT . fmap Right . runAction) act -- TODO
    _                   -> throwError HiErrorInvalidArgument
eval' (HiExprApply fun args) = do
  asFun <- eval' fun
  arity <- arity asFun
  if elem (-1) arity || elem (length args) arity
    then
      case asFun of
        (HiValueFunction HiFunIf) -> do
          case args of
            [cond, a, b] -> do
              condEv <- eval' cond
              case condEv of
                HiValueBool b' -> do
                  if b'
                    then eval' a
                    else eval' b
                _ -> throwError HiErrorInvalidArgument
            _ -> undefined
        (HiValueFunction HiFunAnd) -> do
          case args of
            [cond1,cond2] -> do
              cond1' <- eval' cond1
              case cond1' of
                HiValueNull       -> return cond1'
                HiValueBool False -> return cond1'
                _                 -> eval' cond2
            _ -> undefined
        (HiValueFunction HiFunOr) -> do
          case args of
            [cond1,cond2] -> do
              cond1' <- eval' cond1
              case cond1' of
                HiValueNull       -> eval' cond2
                HiValueBool False -> eval' cond2
                _                 -> return cond1'
            _ -> undefined
        _ -> do
          args <- mapM eval' args
          evalValue asFun args
    else throwError HiErrorArityMismatch
eval' (HiExprDict dict) = do
  map <- foldM (\oldMap (newKey, newVal) -> do
    key <- eval' newKey
    val <- eval' newVal
    return $ M.insert key val oldMap
    ) M.empty dict
  return $ HiValueDict map


eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'
