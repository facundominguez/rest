{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.REST.SMT where

import Control.Monad.IO.Class
import Data.Hashable
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import System.Process
import Text.Parsec (endBy)
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import GHC.Generics (Generic)
import GHC.IO.Handle

type Z3Model = M.Map String String

parens :: Text.Parsec.Prim.Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p = do
  _ <- char '('
  r <- p
  _ <- char ')'
  return r

parseFunDef :: Text.Parsec.Prim.Stream s m Char => ParsecT s u m (String, String)
parseFunDef = parens $ do
  _     <- string "define-fun "
  var   <- many (noneOf " ")
  _     <- spaces
  _     <- many (noneOf " ") -- args
  _     <- spaces
  _     <- many (noneOf " ") -- type
  _     <- spaces
  value <- many (noneOf ")")
  return (var, value)

modelParser :: Text.Parsec.Prim.Stream s m Char => ParsecT s u m Z3Model
modelParser = parens $ do
  spaces
  defs <- endBy parseFunDef spaces
  return $ M.fromList defs

readModel :: Handle -> IO String
readModel handle = go "" where
  closedTerm t = length (filter (== '(') t) == length (filter (== ')') t)
  go buf = do
    line <- hGetLine handle
    let buf' = buf ++ line ++ "\n"
    if closedTerm buf'
    then return buf'
    else go buf'

parseModel :: String -> Z3Model
parseModel str = case parse modelParser "" str of
  Left err -> error (show err)
  Right t  -> t

newtype SMTVar a = SMTVar T.Text deriving (Eq, Ord)

data SMTExpr a where
    And     :: [SMTExpr Bool] -> SMTExpr Bool
    Add     :: [SMTExpr Int]  -> SMTExpr Int
    Or      :: [SMTExpr Bool] -> SMTExpr Bool
    Equal   :: [SMTExpr a]    -> SMTExpr Bool
    Greater :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    GTE     :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    Implies :: SMTExpr Bool   -> SMTExpr Bool -> SMTExpr Bool
    Var     :: SMTVar a       -> SMTExpr a
    Const   :: Int            -> SMTExpr Int


data UntypedExpr =
    UAnd [UntypedExpr]
  | UAdd [UntypedExpr]
  | UOr  [UntypedExpr]
  | UEqual  [UntypedExpr]
  | UGreater UntypedExpr UntypedExpr
  | UGTE UntypedExpr UntypedExpr
  | UImplies UntypedExpr UntypedExpr
  | UVar T.Text
  | UConst Int
  deriving (Show, Eq, Ord, Hashable, Generic)

toUntyped :: SMTExpr a -> UntypedExpr
toUntyped (And xs) = UAnd (map toUntyped xs)
toUntyped (Add xs) = UAdd (map toUntyped xs)
toUntyped (Or xs)  = UOr (map toUntyped xs)
toUntyped (Equal xs) = UEqual (map toUntyped xs)
toUntyped (Greater t u) = UGreater (toUntyped t) (toUntyped u)
toUntyped (GTE t u) = UGTE (toUntyped t) (toUntyped u)
toUntyped (Implies t u) = UImplies (toUntyped t) (toUntyped u)
toUntyped (Var (SMTVar text)) = UVar text
toUntyped (Const i) = UConst i

instance (Eq (SMTExpr a)) where
  t == u = toUntyped t == toUntyped u

instance (Ord (SMTExpr a)) where
  t <= u = toUntyped t <= toUntyped u

instance Hashable (SMTExpr a) where
  hashWithSalt salt e = hashWithSalt salt (toUntyped e)

instance Show (SMTExpr a) where
  show = T.unpack . toFormula


toFormula :: SMTExpr a -> T.Text
toFormula = go False where
  go :: Bool -> SMTExpr a -> T.Text
  go _ (And [])         = "⊤"
  go p (And ts)         = eparens p $ T.intercalate " ∧ " $ map (go (not p)) ts
  go p (Add ts)         = eparens p $ T.intercalate " + " $ map (go (not p)) ts
  go p (GTE t u)        = eparens p $ T.intercalate " ≥ " $ map (go True) $ [t, u]
  go p (Greater t u)    = eparens p $ T.intercalate " > " $ map (go True) $ [t, u]
  go _ (Var (SMTVar v)) = v
  go _ (Const c)        = T.pack (show c)
  go _ _e               = undefined

  eparens True t = T.concat ["(", t, ")"]
  eparens False t = t

vars :: SMTExpr a -> S.Set T.Text
vars (And ts)        = S.unions (map vars ts)
vars (Add ts)        = S.unions (map vars ts)
vars (Or ts)         = S.unions (map vars ts)
vars (Equal ts)      = S.unions (map vars ts)
vars (Greater t u)   = S.union (vars t) (vars u)
vars (GTE t u)       = S.union (vars t) (vars u)
vars (Var (SMTVar var)) = S.singleton var
vars (Implies e1 e2) = S.union (vars e1) (vars e2)
vars (Const _)       = S.empty

data SMTCommand = SMTAssert (SMTExpr Bool) | DeclareVar T.Text | CheckSat | Push | Pop

smtFalse :: SMTExpr Bool
smtFalse = Or []

smtTrue :: SMTExpr Bool
smtTrue  = And []

smtAdd :: [SMTExpr Int] -> SMTExpr Int
smtAdd [] = Const 0
smtAdd ts = Add ts

smtAnd :: SMTExpr Bool -> SMTExpr Bool -> SMTExpr Bool
smtAnd (And xs) (And ys) = And $ L.nub (xs ++ ys)
smtAnd (And xs) e        = And $ L.nub (xs ++ [e])
smtAnd e        (And ys) = And $ L.nub (e:ys)
smtAnd t        u        = And [t, u]

smtGTE :: SMTExpr Int -> SMTExpr Int -> SMTExpr Bool
smtGTE t u | t == u    = smtTrue
smtGTE t u | otherwise = GTE t u

app :: T.Text -> [SMTExpr a] -> T.Text
app op trms = T.concat $ ["(", op, " ", (T.intercalate " " (map exprString trms)), ")"]

exprString :: SMTExpr a -> T.Text
exprString (And [])           = "true"
exprString (Add es)           = app "+" es
exprString (Or [])            = "false"
exprString (And   es)         = app "and" es
exprString (Or    es)         = app "or" es
exprString (Equal xs) | length xs < 2 = "true"
exprString (Equal es)         = app "=" es
exprString (Greater e1 e2)    = app ">" [e1, e2]
exprString (GTE e1 e2)        = app ">=" [e1, e2]
exprString (Implies e1 e2)    = app "=>" [e1, e2]
exprString (Var (SMTVar var)) = var
exprString (Const i)          = T.pack (show i)

commandString :: SMTCommand -> T.Text
commandString (SMTAssert expr) = app "assert" [expr]
commandString (DeclareVar var) = T.concat $ ["(declare-const ", var,  " Int)"]
commandString CheckSat = "(check-sat)"
commandString Push     = "(push)"
commandString Pop      = "(pop)"

askCmds :: SMTExpr Bool -> [SMTCommand]
askCmds expr = varDecls ++ [SMTAssert expr, CheckSat] where
  varDecls = map DeclareVar $ S.toList (vars expr)

type SolverHandle = (Handle, Handle)

spawnZ3 :: IO (Handle, Handle)
spawnZ3 = do
  (Just stdIn, Just stdOut, _, _) <- createProcess (proc "z3" ["-in"]) {std_in = CreatePipe, std_out = CreatePipe}
  return (stdIn, stdOut)

killZ3 :: (Handle, b) -> IO ()
killZ3 (stdIn, _) = hClose stdIn

withZ3 :: MonadIO m => ((Handle, Handle) -> m b) -> m b
withZ3 f =
  do
    z3     <- liftIO $ spawnZ3
    result <- f z3
    liftIO $ killZ3 z3
    return result

getModel :: Handle -> IO ()
getModel stdIn = do
  hPutStr stdIn "(get-model)\n"
  hFlush stdIn

checkSat' :: (Handle,  Handle) -> SMTExpr Bool -> IO Bool
checkSat' (stdIn, stdOut) expr = do
  sendCommands $ Push:askCmds expr
  result <- hGetLine stdOut
  sat <- case result of
    "sat"   -> do
      -- getModel stdIn
      -- model <- readModel stdOut
      -- putStrLn model
      return True
    "unsat" -> return False
    other   -> error other
  sendCommands [Pop]
  return sat
  where
    sendCommands cmds = do
      hPutStr stdIn $ (T.unpack (T.intercalate "\n" (map commandString cmds))) ++ "\n"
      hFlush stdIn

checkSat :: SMTExpr Bool -> IO Bool
checkSat expr = do
  z3     <- spawnZ3
  result <- checkSat' z3 expr
  killZ3 z3
  return result

class ToSMTVar a b | a -> b where
  toSMTVar :: a -> SMTVar b

class ToSMT a b where
  toSMT :: a -> SMTExpr b

instance ToSMT Int Int where
  toSMT = Const

instance {-# OVERLAPPABLE #-} (ToSMTVar a b) => ToSMT a b where
  toSMT :: a -> SMTExpr b
  toSMT op = Var $ toSMTVar op
