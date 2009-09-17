module Llvm.AbsSyn where

import Llvm.Types

-- | Block labels
type LlvmBlockId = String

-- | Blocks consist of
--     * label: The code label for this block
--     * stmts: A list of LlvmStatement's representing the code for this block.
--              This list must end with a control flow. A return, tail call or
--              branch to another LlvmBlock within the current function scope.
data LlvmBlock = LlvmBlock {
        blockLabel :: LlvmBlockId,
        blockStmts :: [LlvmStatement]
  }

type LlvmBlocks = [LlvmBlock]

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
--    * funcDecl:  The signature of this declared function.
--    * funcAttrs: The function attributes.
--    * body:     The body of the functions.
data LlvmFunction = LlvmFunction {
        funcDecl    :: LlvmFunctionDecl,
        funcAttrs   :: [LlvmFuncAttr],
        funcBody    :: LlvmBlocks
  }

type LlvmFunctions  = [LlvmFunction]


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
    LlvmOp
    Perform the machine operator op on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | LlvmOp LlvmMachOp LlvmVar LlvmVar
  
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
       * cast: Cast type
       * from: Variable to cast
       * to:   type to cast to
  -}                
  | Cast LlvmCastOp LlvmVar LlvmType
                 
  {-
    Call
    Call a function. The result is the value of the expression.
      * tailJumps: CallType to signal if the function should be tail called
      * fnptrval:  An LLVM value containing a pointer to a function to be
                   invoked. Can be indirect. Should be LMFunction type.
      * args:      Concrete arguments for the parameters 
  -}                   
  | Call LlvmCallType LlvmVar [LlvmVar]
                 
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

