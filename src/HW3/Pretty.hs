{-# LANGUAGE FlexibleInstances #-}
module HW3.Pretty
  where
import Control.Monad.State (runState)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (ord)
import Data.Map.Strict (Map, toList, empty, size)
import Data.Ratio (denominator, numerator)
import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)
import Data.Sequence (Seq (Empty, (:<|)), foldlWithIndex)
import Data.Text (unpack)
import HW3.Base (HiAction (..), HiFun (..), HiValue (..))
import Numeric (showHex)
import Prettyprinter (Doc, Pretty (pretty), SimpleDocStream, comma, encloseSep, lbracket, rbracket,
                      viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)


instance Pretty a => Pretty (Map a a) where
  pretty dict = if size empty == 0 
    then encloseSep (pretty "{") (pretty " }") (pretty ",") $ map (\(k, v) -> pretty " " <> pretty k <> pretty ":" <+> pretty v) $ toList dict
    else pretty "{ }"

instance Pretty a => Pretty (Seq a) where
  pretty (fst :<| lst) = foldl (\l el -> l <> pretty "," <+> pretty el) (pretty "[" <+> pretty fst) lst <+> pretty "]"
  pretty Empty = pretty "[ ]"

instance Pretty ByteString where
  pretty str = C.foldl (\left el -> left <+> pretty (showHex' (ord el))) (pretty "[#") str <+> pretty "#]" where
    showHex' :: Int -> String
    showHex' num = if num < 16 then "0" ++ showHex num "" else showHex num ""

instance Pretty HiValue where
  pretty hv = case hv of
    (HiValueNumber n) -> pretty $ case fromRationalRepetendUnlimited n of
      (sci, Nothing) -> case floatingOrInteger sci of
        Right i -> show i
        Left d  -> show d
      _ ->  let num = numerator $ abs n in
            let denom = denominator $ abs n in
            let int = div num denom in
            let frac = num - int * denom in case (n > 0, int > 0) of
              (False, False) -> "-" ++ show frac ++ "/" ++ show denom
              (False, True)  -> "-" ++ show int ++ " - " ++ show frac ++ "/" ++ show denom
              (True, False)  -> show frac ++ "/" ++ show denom
              (True, True)   -> show int ++ " + " ++ show frac ++ "/" ++ show denom
    (HiValueFunction fun) -> pretty fun
    (HiValueBool True) -> pretty "true"
    (HiValueBool False) -> pretty "false"
    (HiValueString text) -> viaShow text
    (HiValueList list) -> pretty list
    (HiValueBytes bytes) -> pretty bytes
    HiValueNull -> pretty "null"
    (HiValueAction act) -> pretty act
    (HiValueTime time) -> pretty "parse-time(\"" <> pretty (show time) <> pretty "\")"
    (HiValueDict map) -> pretty map

instance Pretty HiAction where
  pretty act = case act of
    HiActionRead s -> pretty "read(\"" <> pretty s <> pretty "\")"
    HiActionWrite s bs -> pretty "write(\"" <> pretty s <> pretty "\", " <> pretty bs <> pretty ")"
    HiActionMkDir s -> pretty "mkdir(\"" <> pretty s <> pretty "\")"
    HiActionChDir s -> pretty "cd(\"" <> pretty s <> pretty "\")"
    HiActionCwd -> pretty "cwd"
    HiActionNow -> pretty "now"
    HiActionRand l r -> pretty "rand(" <> pretty l <> pretty "," <+> pretty r <> pretty ")"
    HiActionEcho t -> pretty "echo(" <> viaShow t <> pretty ")"

instance Pretty HiFun where
  pretty fun  = pretty $ show fun

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
