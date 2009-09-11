--------------------------------------------------------------------------------
-- Prettyprint a Llvm Module
--------------------------------------------------------------------------------

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
    ppLlvmTypeAlias 

    ) where

import Llvm.AbsSyn
import Llvm.Types

import Pretty
import Data.List ( intersperse )

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
              
ppLlvmGlobal oth = error ("Non Global var ppr as global!" ++ show oth)

ppLlvmGlobals :: [LMGlobal] -> Doc
ppLlvmGlobals ls = vcat $ map ppLlvmGlobal ls

ppLlvmConstants :: [LMConstant] -> Doc
ppLlvmConstants cons = vcat $ map ppLlvmConstant cons

ppLlvmConstant :: LMConstant -> Doc
ppLlvmConstant (dst@(LMGlobalVar _ _ link),src) = 
        ppAssignment dst (text ("internal constant") <+> text (show src))

ppLlvmFunctions :: LlvmFunctions -> Doc
ppLlvmFunctions funcs = vcat $ map ppLlvmFunction funcs

ppLlvmFunction :: LlvmFunction -> Doc
ppLlvmFunction (LlvmFunction dec link body) =
  let linkTxt = show link
      linkDoc   | linkTxt == "" = empty
                | otherwise     = space <> (text linkTxt)
  in (text "define") <> linkDoc <+> (ppLlvmFuncDecSig dec)
        <+> (text "nounwind")
        $+$ lbrace
        $+$ ppLlvmBasicBlocks body
        $+$ rbrace


ppLlvmFunctionDecls :: LlvmFunctionDecls -> Doc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs


ppLlvmFunctionDecl :: LlvmFunctionDecl -> Doc
ppLlvmFunctionDecl dec = (text "declare") <+> ppLlvmFuncDecSig dec

ppLlvmFuncDecSig :: LlvmFunctionDecl -> Doc
ppLlvmFuncDecSig (LlvmFunctionDecl name retType argtype params)
  = case argtype of
        VarArgs   -> ppVarargsFunctionSignature name retType params
        FixedArgs -> ppFunctionSignature name retType params


ppLlvmFuncDecCall :: LlvmFunctionDecl -> [LlvmVar] -> Doc
ppLlvmFuncDecCall (LlvmFunctionDecl name retType argtype params) values
  = case argtype of
        VarArgs   -> ppVarargsFunctionCall name retType params values
        FixedArgs -> ppFunctionSignature name retType values


ppLlvmBasicBlocks :: LlvmBasicBlocks -> Doc
ppLlvmBasicBlocks blocks = vcat $ map ppLlvmBasicBlock blocks

ppLlvmBasicBlock :: LlvmBasicBlock -> Doc
ppLlvmBasicBlock (LlvmBasicBlock blockId stmts)
  = ppLlvmStatement (MkLabel blockId)
        $+$ nest 4 (vcat $ map  ppLlvmStatement stmts)


ppLlvmTypeAlias :: LlvmType -> Doc
ppLlvmTypeAlias al@(LMAlias _ t)
  = (text $ show al) <+> equals <+> (text "type") <+> (text $ show t)
        

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
        Alloca      tp amount     -> ppAlloca tp amount
        MachOp      op left right -> ppMachOp op left right
        Call        dec tp args   -> ppCall tp (ppLlvmFuncDecCall dec args)
        Cast        from to       -> ppCast from to
        Compare     op left right -> ppCmpOp op left right
        GetElemPtr  ptr indexes   -> ppGetElementPtr ptr indexes
        Load        ptr           -> ppLoad ptr
        Malloc      tp amount     -> ppMalloc tp amount
        Phi         tp precessors -> ppPhi tp precessors


--------------------------------------------------------------------------------
-- Print functions
--------------------------------------------------------------------------------

ppFunctionSignature :: String -> LlvmType -> [LlvmVar] -> Doc
ppFunctionSignature fnName returnType params =
  let ppParams = ppCommaJoin params
   in (text (show returnType)) <+> atsym <> (text fnName)
        <> lparen <+> ppParams <+> rparen


ppVarargsFunctionSignature :: String -> LlvmType -> [LlvmVar] -> Doc
ppVarargsFunctionSignature fnName returnType params =
  let ppParams = ppCommaJoin params <> (text ", ...")
   in (text $ show returnType) <+> atsym <> (text fnName) <> lparen
        <+> ppParams <+> rparen


ppVarargsFunctionCall :: String -> LlvmType -> [LlvmVar] -> [LlvmVar] -> Doc
ppVarargsFunctionCall fnName returnType varArgParams params =
  let ppParams = ppCommaJoin params
      ppTpList = (ppCommaJoin $ map getVarType varArgParams) <+> (text ", ...")
   in (text $ show returnType) <+> lparen <+> ppTpList <+> rparen <> (text "*")
        <+> atsym <> (text fnName) <> lparen <+> ppParams <+> rparen


ppCall :: LlvmCallType -> Doc -> Doc
ppCall tailCall funcSig =
  let tailAnot (StdCall)  = empty
      tailAnot (TailCall) = text "tail "
   in tailAnot tailCall <> (text "call") <+> funcSig


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
ppStore rhs lhs =
  (text "store") <+> (text $ show rhs) <> comma <+> (text $ show lhs)


ppCast :: LlvmVar -> LlvmType -> Doc
ppCast from to =
  let castOp
        | isInt to && (isPointer $ getVarType from)   = text "ptrtoint"
        | (isPointer to) && (isInt $ getVarType from) = text "inttoptr"
        | otherwise                                = text "bitcast"
  in castOp <+> (text $ show from) <+> (text "to") <+> (text $ show to)


ppMalloc :: LlvmType -> Int -> Doc
ppMalloc tp amount =
  (text "malloc") <+> (text $ show tp) <> (text ", i32") <+> (text $ show amount)


ppAlloca :: LlvmType -> Int -> Doc
ppAlloca tp amount =
  (text "alloca") <+> (text $ show tp) <> (text ", i32") <+> (text $ show amount)


ppGetElementPtr :: LlvmVar -> [Int] -> Doc
ppGetElementPtr ptr idx =
  let indexes = hcat $ map (((text ", i32") <+>) . text . show) idx
  in (text "getelementptr") <+> (text $ show ptr) <> indexes


ppReturn :: LlvmVar -> Doc
ppReturn var
  | getVarType var == LMVoid  = (text "ret") <+> (text $ show (getVarType var))
  | otherwise              = (text "ret") <+> (text $ show var)


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
        <+> (hcat $ intersperse (text ",") (map ppPreds preds))


ppSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> Doc
ppSwitch scrut dflt targets =
  let ppTarget  (val, lab) = (text $ show val) <> comma <+> (text $ show lab)
      ppTargets  xs        = brackets $ vcat (map ppTarget xs)
  in (text "switch") <+> (text $ show scrut) <> comma <+> (text $ show dflt)
        <+> (ppTargets targets)


--------------------------------------------------------------------------------
-- Misc functions
--------------------------------------------------------------------------------
ppPrependList :: String -> [Doc] -> Doc
ppPrependList str docs = vcat (map ((text str) <+>) docs)

atsym :: Doc
atsym = text "@"

ppCommaJoin :: (Show a) => [a] -> Doc
ppCommaJoin strs = hcat $ intersperse (comma <> space)  $ map (text . show) strs
