module Llvm.AbsSyn where

import Llvm.Types

type BlockId = String

data LlvmBasicBlock = LlvmBasicBlock BlockId [LlvmStatement]

type LlvmBasicBlocks = [LlvmBasicBlock]

-- | Modules consist of 
--    * comments:  Just plain comments added to the Llvm IR.
--    * constants: The first element of the tuple is the declaration of the 
--                 constant while the second element is the value of the
--                 constant.
--    * globals:   Global modifiable variables.
--    * fwdDecls:  Functions used in this module, defined in other modules.
--    * funcs:     Functions defined in this module. 
data LlvmModule = LlvmModule  {
        modComments  :: [LMString],
        modConstants :: [LMConstant],
        modGlobals   :: [LMGlobal],
        modFwdDecls  :: LlvmFunctionDecls,
        modFuncs     :: LlvmFunctions
  }

-- | Functions have
--    * funcDecl: The signature of this declared function.
--    * internal: LinkageType of the function.
--    * body:     The body of the functions.
data LlvmFunction = LlvmFunction {
        funcDecl    :: LlvmFunctionDecl,
        funcLinkage :: LlvmLinkageType,
        funcAttrs   :: [LlvmFuncAttr],
        funcBody    :: LlvmBasicBlocks
  }

type LlvmFunctions  = [LlvmFunction]


-- | A function declaration has the following elements
--    * name:       Unique identifier for the function.
--    * returnType: Type of the returned value
--    * varargs:    ParameterListType indicating if this function uses varargs
--    * params:     Signature of the parameters 
data LlvmFunctionDecl = LlvmFunctionDecl {
        decName       :: String,
        decReturnType :: LlvmType,
        decVarargs    :: LlvmParameterListType,
        decParams     :: [LlvmVar]
  }

instance Show LlvmFunctionDecl where
  show (LlvmFunctionDecl n r VarArgs p)
        = (show r) ++ " @" ++ n ++ "(" ++ (show p) ++ ", ...)"
  show (LlvmFunctionDecl n r FixedArgs p)
        = (show r) ++ " @" ++ n ++ "(" ++ (show p) ++ ")"

instance Eq LlvmFunctionDecl where
  (LlvmFunctionDecl n1 r1 v1 p1) == (LlvmFunctionDecl n2 r2 v2 p2)
        = (n1 == n2) && (r1 == r2) && (v1 == v2) && (p1 == p2)

type LlvmFunctionDecls = [LlvmFunctionDecl]

-- | Llvm Statements
data LlvmStatement
  {-
    Assignment
    Assign an expression to an variable
      * dest:   Variable to assign to
      * source: Source expression
  -}
  = Assignment LlvmVar LlvmExpression
                 
  {-
    Branch
    Always branch to the target label
  -}               
  | Branch LlvmVar 
  
  {-
    BranchIf
    Branch to label targetTrue if cond is true otherwise to label targetFalse
      * cond:        condition that will be tested, must be of type i1
      * targetTrue:  label to branch to if cond is true
      * targetFalse: label to branch to if cond is false
  -}
  | BranchIf LlvmVar LlvmVar LlvmVar
                 
  {-
    Comment
    Plain comment.
  -}               
  | Comment [String]
  
  {-
    Label
    Set a label on this position.
      * name: Identifier of this label, unique for this module
  -}     
  | MkLabel String
  
  {- 
    Store
    Store variable value in pointer ptr. If value is of type t then ptr must
    be of type t*.
      * value: Variable/Constant to store.
      * ptr:   Location to store the value in
  -}         
  | Store LlvmVar LlvmVar
  {-
    Switch
      * scrutinee: Variable or constant which must be of integer type that is
                   determines which arm is chosen.
      * def:       The default label if there is no match in target.
      * target:    A list of (value,label) where the value is an integer 
                   constant and label the corresponding label to jump to if the 
                   scrutinee matches the value.
  -}
  | Switch LlvmVar LlvmVar [(LlvmVar, LlvmVar)] 
  
  {-
    Return
      * result: The variable or constant to return
  -}                  
  | Return LlvmVar

  {-
    Unreachable
      An instruction for the optimizer that the code following is not reachable
  -}
  | Unreachable

  {-
    Expr
      Raise an expression to a statement (if don't want result or want to use
      Llvm unamed values.
  -}
  | Expr LlvmExpression

  deriving (Show, Eq)

type LlvmStatements = [LlvmStatement]

-- | Llvm Expressions
data LlvmExpression
  {- 
    Alloca
    Allocate amount * sizeof(tp) bytes on the stack
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  -} 
  = Alloca LlvmType Int
                 
  {-
    MachOp
    Perform the machine operator op on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | MachOp LlvmMachOp LlvmVar LlvmVar
  
  {-
    Compare
    Perform a compare operation on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | Compare LlvmCmpOp LlvmVar LlvmVar

  {- 
    Malloc
    Allocate amount * sizeof(tp) bytes on the heap
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  -} 
  | Malloc LlvmType Int
                 
  {-
    Load
    Load the value at location ptr
  -}               
  | Load LlvmVar
  
  {-
    GetElemPtr
    Navigate in an structure, selecting elements
      * ptr:     Location of the structure
      * indexes: A list of indexes to select the correct value. For example
                 the first element of the third element of the structure ptr
                 is selected with [3,1] (zero indexed)
  -}
  | GetElemPtr LlvmVar [Int]
                 
  {- Cast
     Cast the variable from to the to type. This is an abstraction of three
     cast operators in Llvm, inttoptr, prttoint and bitcast.
       * from: Variable to cast
       * to:   type to cast to
  -}                
  | Cast LlvmVar LlvmType
                 
  {-
    Call
    Call a function. The result is the value of the expression.
      * func:      Signature of the function to call
      * tailJumps: CallType to signal if the function should be tail called
      * args:      Concrete arguments for the parameters 
  -}                   
  | Call LlvmFunctionDecl LlvmCallType [LlvmVar]
                 
  {- 
    Phi
    Merge variables from different basic blocks which are predecessors of this
    basic block in a new variable of type tp.
      * tp:         type of the merged variable, must match the types of the
                    precessors variables.
      * precessors: A list of variables and the basic block that they originate
                    from.
  -}               
  | Phi LlvmType [(LlvmVar,LlvmVar)]
                 
  deriving (Show, Eq)

