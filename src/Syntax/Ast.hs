{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax.Ast
  ( Expr (..),
    Var (..),
    Fact (..),
    Plain,
    Labeled,
    labelExpr,
    runLabel,
    toFacts,
    factToText,
    lambdaFacts,
    appFacts,
    varFacts,
    PlainExpr,
    LabeledExpr,
    LabelError (..),
  )
where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text, intercalate, pack)

-----------------------------------------------
-- Trees that grow strategy

data Plain

data Labeled

-- AST Extensions
class ASTExt tag where
  type ExpAnn tag
  type VarAnn tag

-- no new information for plain ASTs
instance ASTExt Plain where
  type ExpAnn Plain = ()
  type VarAnn Plain = ()

-- annotations for labelled ASTs
instance ASTExt Labeled where
  type ExpAnn Labeled = Int
  type VarAnn Labeled = Int

data Var tag = Var (VarAnn tag) String

data Expr tag
  = Lam (ExpAnn tag) (Var tag) (Expr tag)
  | App (ExpAnn tag) (Expr tag) (Expr tag)
  | VarE (Var tag)

type PlainExpr = Expr Plain

type LabeledExpr = Expr Labeled

data Fact
  = LamF Text Text Text
  | AppF Text Text Text
  | VarF Text

-----------------------------------------------

data LabelState = LabelState
  { variableLabels :: Map String Int,
    currentLabel :: Int
  }

newtype LabelError = UnboundVariable String
  deriving (Show, Eq)

emptyState :: LabelState
emptyState = LabelState Map.empty 0

fresh :: Monad m => StateT LabelState m Int
fresh = do
  st <- get
  let lbl = currentLabel st
  put st {currentLabel = lbl + 1}
  return lbl

bindVar ::
  Monad m =>
  Var a ->
  StateT LabelState m (Var Labeled)
bindVar (Var _ name) = do
  oldMap <- gets variableLabels
  lbl <- fresh
  modify $ \st -> st {variableLabels = Map.insert name lbl oldMap}
  return (Var lbl name)

useVar ::
  Var a ->
  StateT LabelState (Either LabelError) (Var Labeled)
useVar (Var _ name) = do
  mp <- gets variableLabels
  case Map.lookup name mp of
    Just lbl -> return (Var lbl name)
    Nothing -> lift $ Left $ UnboundVariable name

labelExpr :: PlainExpr -> StateT LabelState (Either LabelError) LabeledExpr
labelExpr pe = case pe of
  Lam _ plainVar body -> do
    lamLbl' <- fresh
    oldMap <- gets variableLabels
    boundVar' <- bindVar plainVar
    body' <- labelExpr body
    modify $ \st -> st {variableLabels = oldMap}
    pure (Lam lamLbl' boundVar' body')
  App _ fun arg -> do
    appLbl <- fresh
    f' <- labelExpr fun
    x' <- labelExpr arg
    pure (App appLbl f' x')
  VarE plainVar -> do
    usedVar <- useVar plainVar
    pure (VarE usedVar)

runLabel :: PlainExpr -> Either LabelError LabeledExpr
runLabel expr = evalStateT (labelExpr expr) emptyState

varLabel :: Var Labeled -> Int
varLabel (Var label _) = label

exprLabel :: LabeledExpr -> Int
exprLabel expr = case expr of
  Lam label _ _ -> label
  App label _ _ -> label
  VarE v -> varLabel v

labelToString :: Int -> Text
labelToString = pack . show

toFacts :: LabeledExpr -> [Fact]
toFacts = go []
  where
    go acc expr' = case expr' of
      Lam label var e ->
        let bodyFacts = go acc e
            lamFact =
              LamF
                (labelToString label)
                (labelToString (varLabel var))
                (labelToString (exprLabel e))
         in lamFact : bodyFacts
      App label left right ->
        let rightFacts = go acc right
            leftFacts = go rightFacts left
            appFact =
              AppF
                (labelToString label)
                (labelToString (exprLabel left))
                (labelToString (exprLabel right))
         in appFact : leftFacts
      VarE var ->
        let varFact = VarF (labelToString (varLabel var))
         in varFact : acc

lambdaFacts :: LabeledExpr -> [Text]
lambdaFacts =
  mapMaybe
    ( \case
        lamf@(LamF {}) -> Just $ factToText lamf
        _ -> Nothing
    )
    . toFacts

appFacts :: LabeledExpr -> [Text]
appFacts =
  mapMaybe
    ( \case
        appF@(AppF {}) -> Just $ factToText appF
        _ -> Nothing
    )
    . toFacts

varFacts :: LabeledExpr -> [Text]
varFacts =
  mapMaybe
    ( \case
        varF@(VarF {}) -> Just $ factToText varF
        _ -> Nothing
    )
    . toFacts

factToText :: Fact -> Text
factToText (LamF a b c) = intercalate "," [a, b, c]
factToText (AppF a b c) = intercalate "," [a, b, c]
factToText (VarF a) = a
