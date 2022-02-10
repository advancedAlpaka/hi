module HW3.Parser
  where
import Control.Monad (void)
import Control.Monad.Combinators (many, optional)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString as B (ByteString, empty, pack)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Ratio (Rational)
import Data.Scientific (floatingOrInteger)
import Data.Text as T (intercalate, intersperse, pack)
import Data.Void (Void)
import GHC.Real ()
import HW3.Base (HiAction (HiActionCwd, HiActionNow), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, label, notFollowedBy, takeWhileP, try), ParseErrorBundle,
                        Parsec, between, choice, empty, manyTill, runParser, satisfy, sepBy, sepBy1,
                        some, (<?>), (<|>))
import Text.Megaparsec.Char (char, char', numberChar, punctuationChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pSpace :: Parser ()
pSpace =  L.space space1 Text.Megaparsec.empty
          Text.Megaparsec.empty

symbol :: String -> Parser String
symbol = L.symbol pSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme pSpace

pString :: Parser String
pString = lexeme (char '"' *> manyTill L.charLiteral (char '"')) <?> "string"

pNumber :: Parser Rational
pNumber = lexeme (do
  value <- L.signed pSpace L.scientific
  return $ toRational value) <?> "number"

pHex :: Parser Int
pHex = choice [
  0 <$ string "0",
  1 <$ string "1",
  2 <$ string "2",
  3 <$ string "3",
  4 <$ string "4",
  5 <$ string "5",
  6 <$ string "6",
  7 <$ string "7",
  8 <$ string "8",
  9 <$ string "9",
  10 <$ string "a",
  11 <$ string "b",
  12 <$ string "c",
  13 <$ string "d",
  14 <$ string "e",
  15 <$ string "f"]

pHexNum :: Parser Int
pHexNum = do
  int1 <- pHex
  int2 <- pHex
  return $ int1 * 16 + int2

pByteArray :: Parser ByteString
pByteArray = (lexeme (string "[#")) *> (do
  fstM <- optional $ pHexNum
  case fstM of
    Nothing -> return B.empty
    Just fst -> do
      tail <- many $ try ((some $ satisfy isSpace) *> pHexNum)
      many $ satisfy isSpace
      return $ B.pack $ Prelude.map toEnum (fst : tail))
  <* (lexeme $ string "#]") <?> "bytelist"

pBool :: Parser Bool
pBool = (lexeme $ False <$ string "false" <|> True <$ string "true") <?> "boolean"

funs :: [HiFun]
funs = [
    HiFunIf
  , HiFunLength
  , HiFunToUpper
  , HiFunToLower
  , HiFunTrim
  , HiFunReverse
  , HiFunList
  , HiFunRange
  , HiFunFold
  , HiFunPackBytes
  , HiFunUnpackBytes
  , HiFunEncodeUtf8
  , HiFunDecodeUtf8
  , HiFunZip
  , HiFunUnzip
  , HiFunSerialise
  , HiFunDeserialise
  , HiFunRead
  , HiFunWrite
  , HiFunMkDir
  , HiFunChDir
  , HiFunParseTime
  , HiFunRand
  , HiFunEcho
  , HiFunKeys
  , HiFunValues
  , HiFunInvert
  , HiFunCount
  , HiFunAdd
  , HiFunDiv
  , HiFunMul
  , HiFunSub
  , HiFunAnd
  , HiFunOr
  , HiFunNotLessThan
  , HiFunNotGreaterThan
  , HiFunNotEquals
  , HiFunLessThan
  , HiFunGreaterThan
  , HiFunEquals
  , HiFunNot]

pFun :: Parser HiFun
pFun = (lexeme $ choice $ Prelude.map (\fun -> fun <$ string (show fun)) funs) <?> "fun"

actions :: [HiAction]
actions = [
    HiActionCwd
  , HiActionNow]

pAction :: Parser HiAction
pAction = (lexeme $ choice $ Prelude.map (\act -> act <$ string (show act)) actions) <?> "action"

pAtom :: Parser HiValue
pAtom = choice [
    HiValueFunction <$> pFun
  , HiValueNull <$ lexeme (string "null")
  , HiValueNumber <$> pNumber
  , HiValueBool <$> pBool
  , HiValueString . T.pack <$> pString
  , HiValueBytes <$> pByteArray
  , HiValueAction <$> pAction ]

pList :: Parser [HiExpr]
pList = symbol "[" *> (do
  valMaybe <- optional pAlg
  case valMaybe of
    Nothing -> return []
    Just val -> do
      vals <- many $ symbol "," *> pAlg
      return $ val : vals
  ) <* symbol "]"

pDict :: Parser [(HiExpr,HiExpr)]
pDict = (symbol "{") *> do
  fstMaybe <- optional pPair
  case fstMaybe of
    Nothing -> return []
    Just fst -> do
      tail <- many (symbol "," *> pPair)
      return $ fst : tail
  <* (char '}') <?> "dictionary" where
    pPair :: Parser (HiExpr,HiExpr)
    pPair = (do
      key <- pAlg
      symbol ":"
      val <- pAlg
      return (key,val)) <?> "pair"

lPar, rPar :: Parser String
lPar = symbol "("
rPar = symbol ")"

pExpr :: Parser HiExpr
pExpr = do
  pSpace
  asFun <- choice [
    HiExprValue <$> pAtom,
    lPar *> pAlg <* rPar ,
    (HiExprApply (HiExprValue $ HiValueFunction HiFunList)) <$> pList,
    HiExprDict <$> pDict]
  exclMaybe <- optional $ lexeme $ many $ char '!'
  let asFunM = case exclMaybe of
        Nothing -> asFun
        Just c  -> hel' (length c) asFun
  args <- many pArgs
  let res = Prelude.foldr (\args setLeft left -> setLeft $ args left) id args asFunM
  return res
    where
      pArgs :: Parser (HiExpr -> HiExpr)
      pArgs = do
        after <- pId <|> pPars
        exclMaybe <- optional $ lexeme $ many $ char '!'
        return $ case exclMaybe of
          Nothing -> (`HiExprApply` after)
          Just c  -> helper' (length c) after
      helper' :: Int -> [HiExpr] -> HiExpr -> HiExpr
      helper' 0 = \args val -> HiExprApply val args
      helper' n = \args val -> HiExprRun $ helper' (n-1) args val
      hel' :: Int -> HiExpr -> HiExpr
      hel' 0 = id
      hel' n = HiExprRun . (hel' (n-1))
      pPars :: Parser [HiExpr]
      pPars = lPar *> (do
            opMaybe <- optional $ pAlg
            case opMaybe of
              Nothing -> return []
              Just op -> do
                ops <- many $ lexeme (char ',') *> pAlg
                return $ op : ops) <* rPar
      pId :: Parser [HiExpr]
      pId = char '.' *> do
        id <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
        return [HiExprValue $ HiValueString $ T.intercalate (T.pack "-") $ map T.pack id]

pAlg :: Parser HiExpr
pAlg = lexeme $ makeExprParser pExpr operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryL "*" HiFunMul
    , binaryL "/" HiFunDiv
    ]
  , [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub
    ]
  , [ binaryN "<" HiFunLessThan
    , binaryN ">=" HiFunNotLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "<=" HiFunNotGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals
    ]
  , [ binaryR "&&" HiFunAnd
    ]
  , [ binaryR "||" HiFunOr
    ]
  ]

op :: HiFun -> HiExpr -> HiExpr -> HiExpr
op f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

binaryN, binaryL, binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryN = binary InfixN
binaryL = binary InfixL
binaryR = binary InfixR

binary :: ((Parser (HiExpr -> HiExpr -> HiExpr)) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
binary cons name f = cons ((op f) <$ op' name)
  where
    op' n = (lexeme . try) (string n <* notFollowedBy (string "="))

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pAlg <* eof) "HiExpr"
