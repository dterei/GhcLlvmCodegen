--------------------------------------------------------------------------------
-- Prettyprint an LLVM Module.
--

module Llvm.PpLlvm (

    ppLlvmModule,
    ppLlvmComments,
    ppLlvmComment,
    ppLlvmConstants,
    ppLlvmConstant,
    ppLlvmGlobals,
    ppLlvmGlobal,
    ppLlvmFunctionDecls,
    ppLlvmFunctionDecl,
    ppLlvmFunctions,
    ppLlvmFunction,
    ppLlvmType,
    ppLlvmTypes

    ) where

#include "HsVersions.h"

import Llvm.AbsSyn
import Llvm.Types

import Data.List ( intersperse )
import Pretty
-- import Text.PrettyPrint.HughesPJ

ppLlvmModule :: LlvmModule -> Doc
ppLlvmModule (LlvmModule comments constants globals decls funcs)
  = ppLlvmComments comments
    $+$ empty
    $+$ ppLlvmConstants constants
    $+$ ppLlvmGlobals globals
    $+$ empty
    $+$ ppLlvmFunctionDecls decls
    $+$ empty
    $+$ ppLlvmFunctions funcs


ppLlvmComments :: [LMString] -> Doc
ppLlvmComments comments = vcat $ map ppLlvmComment comments

ppLlvmComment :: LMString -> Doc
ppLlvmComment com = semi <+> (text com)


ppLlvmGlobal :: LMGlobal -> Doc
ppLlvmGlobal (var@(LMGlobalVar _ _ _), Nothing) =
  (text $ getName var) <+> (equals <+> (text $ show $ getLink var)
      <+> text "global") <+> (text $ show (pLower $ getVarType var))

ppLlvmGlobal (var@(LMGlobalVar _ _ _), (Just stat)) =
  (text $ getName var) <+> (equals <+> (text $ show $ getLink var)
      <+> text "global") <+> (text $ show stat)

ppLlvmGlobal oth = error $ "Non Global var ppr as global! " ++ show oth

ppLlvmGlobals :: [LMGlobal] -> Doc
ppLlvmGlobals ls = vcat $ map ppLlvmGlobal ls


ppLlvmConstants :: [LMConstant] -> Doc
ppLlvmConstants cons = vcat $ map ppLlvmConstant cons

ppLlvmConstant :: LMConstant -> Doc
ppLlvmConstant (dst@(LMGlobalVar _ _ link),src) =
    ppAssignment dst $ text (show link) <+> text ("constant")
        <+> text (show src)

ppLlvmConstant c = error $ "Non global var as constant! " ++ show c


ppLlvmFunctions :: LlvmFunctions -> Doc
ppLlvmFunctions funcs = vcat $ map ppLlvmFunction funcs

ppLlvmFunction :: LlvmFunction -> Doc
ppLlvmFunction (LlvmFunction dec attrs body) =
    let attrDoc = ppSpaceJoin attrs
    in (text "define") <+> (ppLlvmFuncDecSig dec)
        <+> attrDoc
        $+$ lbrace
        $+$ ppLlvmBlocks body
        $+$ rbrace


ppLlvmFunctionDecls :: LlvmFunctionDecls -> Doc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs


ppLlvmFunctionDecl :: LlvmFunctionDecl -> Doc
ppLlvmFunctionDecl dec = (text "declare") <+> ppLlvmFuncDecSig dec

ppLlvmFuncDecSig :: LlvmFunctionDecl -> Doc
ppLlvmFuncDecSig (LlvmFunctionDecl name link cc retTy argTy params)
  = let linkTxt = show link
        linkDoc   | linkTxt == "" = empty
                  | otherwise     = (text linkTxt) <> space
        ppParams = either ppCommaJoin ppCommaJoin params <>
                    (case argTy of
                        VarArgs -> (text ", ...")
                        FixedArgs -> empty)
  in linkDoc <> (text $ show cc) <+> (text $ show retTy)
      <+> atsym <> (text name) <> lparen <+> ppParams <+> rparen


ppLlvmBlocks :: LlvmBlocks -> Doc
ppLlvmBlocks blocks = vcat $ map ppLlvmBlock blocks

ppLlvmBlock :: LlvmBlock -> Doc
ppLlvmBlock (LlvmBlock blockId stmts)
  = ppLlvmStatement (MkLabel blockId)
        $+$ nest 4 (vcat $ map  ppLlvmStatement stmts)


ppLlvmTypes :: [LlvmType] -> Doc
ppLlvmTypes tys = vcat $ map ppLlvmType tys

ppLlvmType :: LlvmType -> Doc

ppLlvmType al@(LMAlias _ t)
  = (text $ show al) <+> equals <+> (text "type") <+> (text $ show t)

ppLlvmType (LMFunction t)
  = ppLlvmFunctionDecl t

ppLlvmType _ = empty


ppLlvmStatement :: LlvmStatement -> Doc
ppLlvmStatement stmt
  = case stmt of
        Assignment  dst expr      -> ppAssignment dst (ppLlvmExpression expr)
        Branch      target        -> ppBranch target
        BranchIf    cond ifT ifF  -> ppBranchIf cond ifT ifF
        Comment     comments      -> ppLlvmComments comments
        MkLabel     label         -> (text $ label) <> colon
        Store       value ptr     -> ppStore value ptr
        Switch      scrut def tgs -> ppSwitch scrut def tgs
        Return      result        -> ppReturn result
        Expr        expr          -> ppLlvmExpression expr
        Unreachable               -> text "unreachable"


ppLlvmExpression :: LlvmExpression -> Doc
ppLlvmExpression expr
  = case expr of
        Alloca     tp amount        -> ppAlloca tp amount
        LlvmOp     op left right    -> ppMachOp op left right
        Call       tp fp args attrs -> ppCall tp fp args attrs
        Cast       op from to       -> ppCast op from to
        Compare    op left right    -> ppCmpOp op left right
        GetElemPtr ptr indexes      -> ppGetElementPtr ptr indexes
        Load       ptr              -> ppLoad ptr
        Malloc     tp amount        -> ppMalloc tp amount
        Phi        tp precessors    -> ppPhi tp precessors


--------------------------------------------------------------------------------
-- Print functions
--------------------------------------------------------------------------------

-- Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: LlvmCallType -> LlvmVar -> [LlvmVar] -> [LlvmFuncAttr] -> Doc
ppCall ct fptr vals attrs = case fptr of
                           --
    -- if local var function pointer, unwrap
    LMLocalVar _ (LMPointer (LMFunction d)) -> ppCall' d

    -- should be function type otherwise
    LMGlobalVar _ (LMFunction d) _          -> ppCall' d

    -- not pointer or function, so error
    _other -> error $ "ppCall called with non LMFunction type!\nMust be "
                ++ " called with either global var of function type or "
                ++ "local var of pointer function type."

    where
        ppCall' (LlvmFunctionDecl _ _ cc ret argTy params) =
            let tc = if ct == TailCall then text "tail " else empty
                ppValues = ppCommaJoin vals
                ppArgTy = either ppCommaJoin (\x -> ppCommaJoin $ map getVarType x) params <>
                           (case argTy of
                               VarArgs -> (text ", ...")
                               FixedArgs -> empty)
                fnty = space <> lparen <> ppArgTy <> rparen <> (text "*")
                attrDoc = ppSpaceJoin attrs
            in  tc <> (text "call") <+> (text $ show cc) <+> (text $ show ret)
                    <> fnty <+> (text $ getName fptr) <> lparen <+> ppValues
                    <+> rparen <+> attrDoc


ppMachOp :: LlvmMachOp -> LlvmVar -> LlvmVar -> Doc
ppMachOp op left right =
  (text $ show op) <+> (text $ show (getVarType left)) <+> (text $ getName left)
        <> comma <+> (text $ getName right)


ppCmpOp :: LlvmCmpOp -> LlvmVar -> LlvmVar -> Doc
ppCmpOp op left right =
  let cmpOp
        | isInt (getVarType left) && isInt (getVarType right) = text "icmp"
        | isFloat (getVarType left) && isFloat (getVarType right) = text "fcmp"
        | otherwise = error ("can't compare different types, left = "
                ++ (show $ getVarType left) ++ ", right = "
                ++ (show $ getVarType right))
  in cmpOp <+> (text $ show op) <+> (text $ show (getVarType left))
        <+> (text $ getName left) <> comma <+> (text $ getName right)


ppAssignment :: LlvmVar -> Doc -> Doc
ppAssignment var expr = (text $ getName var) <+> equals <+> expr


ppLoad :: LlvmVar -> Doc
ppLoad var = (text "load") <+> (text $ show var)


ppStore :: LlvmVar -> LlvmVar -> Doc
ppStore val dst =
  (text "store") <+> (text $ show val) <> comma <+> (text $ show dst)


ppCast :: LlvmCastOp -> LlvmVar -> LlvmType -> Doc
ppCast op from to =
  let castOp = text $ show op
  in castOp <+> (text $ show from) <+> (text "to") <+> (text $ show to)


ppMalloc :: LlvmType -> Int -> Doc
ppMalloc tp amount =
  (text "malloc") <+> (text $ show tp) <> comma <+> (text $ show llvmWord)
      <+> (text $ show amount)


ppAlloca :: LlvmType -> Int -> Doc
ppAlloca tp amount =
  (text "alloca") <+> (text $ show tp) <> comma <+> (text $ show llvmWord)
      <+> (text $ show amount)


ppGetElementPtr :: LlvmVar -> [Int] -> Doc
ppGetElementPtr ptr idx =
  let indexes = hcat $ map ((comma <+> (text $ show llvmWord) <+>) . text . show) idx
  in (text "getelementptr") <+> (text $ show ptr) <> indexes


ppReturn :: LlvmVar -> Doc
ppReturn var
  | getVarType var == LMVoid  = (text "ret") <+> (text $ show (getVarType var))
  | otherwise                 = (text "ret") <+> (text $ show var)


ppBranch :: LlvmVar -> Doc
ppBranch var = (text "br") <+> (text $ show var)


ppBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> Doc
ppBranchIf cond trueT falseT
  = (text "br") <+> (text $ show cond) <> comma <+> (text $ show trueT) <> comma
        <+> (text $ show falseT)


ppPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> Doc
ppPhi tp preds =
  let ppPreds (val, label) = brackets $ (text $ getName val) <> comma
        <+> (text $ getName label)
  in (text "phi") <+> (text $ show tp)
        <+> (hcat $ intersperse comma (map ppPreds preds))


ppSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> Doc
ppSwitch scrut dflt targets =
  let ppTarget  (val, lab) = (text $ show val) <> comma <+> (text $ show lab)
      ppTargets  xs        = brackets $ vcat (map ppTarget xs)
  in (text "switch") <+> (text $ show scrut) <> comma <+> (text $ show dflt)
        <+> (ppTargets targets)


--------------------------------------------------------------------------------
-- Misc functions
--------------------------------------------------------------------------------
atsym :: Doc
atsym = text "@"

ppCommaJoin :: (Show a) => [a] -> Doc
ppCommaJoin strs = hcat $ intersperse comma (map (text . show) strs)

ppSpaceJoin :: (Show a) => [a] -> Doc
ppSpaceJoin strs = hcat $ intersperse space (map (text . show) strs)

