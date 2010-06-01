--------------------------------------------------------------------------------
-- | The LLVM abstract syntax.
--

module Llvm.AbsSyn where

import Llvm.Types

-- | Block labels
type LlvmBlockId = LMString

-- | A block of LLVM code.
data LlvmBlock = LlvmBlock {
    -- | The code label for this block
    blockLabel :: LlvmBlockId,

    -- | A list of LlvmStatement's representing the code for this block.
    -- This list must end with a control flow statement.
    blockStmts :: [LlvmStatement]
  }

type LlvmBlocks = [LlvmBlock]

-- | An LLVM Module. This is a top level contianer in LLVM.
data LlvmModule = LlvmModule  {
    -- | Comments to include at the start of the module.
    modComments  :: [LMString],

    -- | Constants to include in the module.
    modConstants :: [LMConstant],

    -- | Global variables to include in the module.
    modGlobals   :: [LMGlobal],

    -- | LLVM Functions used in this module but defined in other modules.
    modFwdDecls  :: LlvmFunctionDecls,

    -- | LLVM Functions defined in this module.
    modFuncs     :: LlvmFunctions
  }

-- | An LLVM Function
data LlvmFunction = LlvmFunction {
    -- | The signature of this declared function.
    funcDecl    :: LlvmFunctionDecl,

    -- | The function attributes.
    funcAttrs   :: [LlvmFuncAttr],

    -- | The body of the functions.
    funcBody    :: LlvmBlocks
  }

type LlvmFunctions  = [LlvmFunction]


-- | Llvm Statements
data LlvmStatement
  {- |
    Assign an expression to an variable:
      * dest:   Variable to assign to
      * source: Source expression
  -}
  = Assignment LlvmVar LlvmExpression

  {- |
    Always branch to the target label
  -}
  | Branch LlvmVar

  {- |
    Branch to label targetTrue if cond is true otherwise to label targetFalse
      * cond:        condition that will be tested, must be of type i1
      * targetTrue:  label to branch to if cond is true
      * targetFalse: label to branch to if cond is false
  -}
  | BranchIf LlvmVar LlvmVar LlvmVar

  {- |
    Comment
    Plain comment.
  -}
  | Comment [LMString]

  {- |
    Set a label on this position.
      * name: Identifier of this label, unique for this module
  -}
  | MkLabel LMString

  {- |
    Store variable value in pointer ptr. If value is of type t then ptr must
    be of type t*.
      * value: Variable/Constant to store.
      * ptr:   Location to store the value in
  -}
  | Store LlvmVar LlvmVar

  {- |
    Mutliway branch
      * scrutinee: Variable or constant which must be of integer type that is
                   determines which arm is chosen.
      * def:       The default label if there is no match in target.
      * target:    A list of (value,label) where the value is an integer
                   constant and label the corresponding label to jump to if the
                   scrutinee matches the value.
  -}
  | Switch LlvmVar LlvmVar [(LlvmVar, LlvmVar)]

  {- |
    Return a result.
      * result: The variable or constant to return
  -}
  | Return LlvmVar

  {- |
    An instruction for the optimizer that the code following is not reachable
  -}
  | Unreachable

  {- |
    Raise an expression to a statement (if don't want result or want to use
    Llvm unamed values.
  -}
  | Expr LlvmExpression

  deriving (Show, Eq)


-- | Llvm Expressions
data LlvmExpression
  {- |
    Allocate amount * sizeof(tp) bytes on the stack
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  -}
  = Alloca LlvmType Int

  {- |
    Perform the machine operator op on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | LlvmOp LlvmMachOp LlvmVar LlvmVar

  {- |
    Perform a compare operation on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | Compare LlvmCmpOp LlvmVar LlvmVar

  {- |
    Allocate amount * sizeof(tp) bytes on the heap
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  -}
  | Malloc LlvmType Int

  {- |
    Load the value at location ptr
  -}
  | Load LlvmVar

  {- |
    Navigate in an structure, selecting elements
      * ptr:     Location of the structure
      * indexes: A list of indexes to select the correct value. For example
                 the first element of the third element of the structure ptr
                 is selected with [3,1] (zero indexed)
  -}
  | GetElemPtr LlvmVar [Int]

  {- |
     Cast the variable from to the to type. This is an abstraction of three
     cast operators in Llvm, inttoptr, prttoint and bitcast.
       * cast: Cast type
       * from: Variable to cast
       * to:   type to cast to
  -}
  | Cast LlvmCastOp LlvmVar LlvmType

  {- |
    Call a function. The result is the value of the expression.
      * tailJumps: CallType to signal if the function should be tail called
      * fnptrval:  An LLVM value containing a pointer to a function to be
                   invoked. Can be indirect. Should be LMFunction type.
      * args:      Concrete arguments for the parameters
      * attrs:     A list of function attributes for the call. Only NoReturn,
                   NoUnwind, ReadOnly and ReadNone are valid here.
  -}
  | Call LlvmCallType LlvmVar [LlvmVar] [LlvmFuncAttr]

  {- |
    Merge variables from different basic blocks which are predecessors of this
    basic block in a new variable of type tp.
      * tp:         type of the merged variable, must match the types of the
                    precessors variables.
      * precessors: A list of variables and the basic block that they originate
                    from.
  -}
  | Phi LlvmType [(LlvmVar,LlvmVar)]

  deriving (Show, Eq)

