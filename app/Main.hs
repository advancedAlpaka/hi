module Main
  where
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Set
import HW3.Action (HIO (runHIO), HiPermission (AllowRead, AllowTime, AllowWrite))
import HW3.Base (HiError, HiValue)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
  where
      loop :: InputT IO ()
      loop = do
          minput <- getInputLine "hi> "
          case minput of
              Nothing -> return ()
              Just "quit" -> return ()
              Just input -> do
                  case parse input of
                    Left peb -> outputStrLn $ errorBundlePretty peb
                    Right he -> do
                        outputStrLn $ show he
                        hith <- liftIO (runHIO (eval he :: HIO (Either HiError HiValue)) perms)
                        outputStrLn $ case hith of
                            Left he  -> show he
                            Right hv -> show $ prettyValue hv
                  loop

perms :: Set HiPermission
perms = fromList [AllowRead, AllowTime, AllowWrite]
