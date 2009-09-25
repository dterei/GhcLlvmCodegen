--------------------------------------------------------------------------------
-- The LLVM Type System.
--
module Llvm.Types where

#include "HsVersions.h"

-- | Llvm compare functions, parameter of the 'Expression_Compare' constructor 
--   of type 'Expression'
data LlvmCmpOp
  = LM_CMP_Eq  -- Equality
  | LM_CMP_Ne  -- Non equality
  | LM_CMP_Ugt -- Unsigned greater then
  | LM_CMP_Uge -- Unsigned greater then or equal
  | LM_CMP_Ult -- Unsigned less then
  | LM_CMP_Ule -- Unsigned less then or equal
  | LM_CMP_Sgt -- Signed greater then
  | LM_CMP_Sge -- Signed greater then or equal
  | LM_CMP_Slt -- Signed less then
  | LM_CMP_Sle -- Signed less then or equal
  | LM_CMP_Feq -- Float equality
  | LM_CMP_Fne -- Float non equality
  | LM_CMP_Fgt -- Float greater then
  | LM_CMP_Fge -- Float greater then or equal
  | LM_CMP_Flt -- Float less then
  | LM_CMP_Fle -- Float less then or equal
  deriving (Eq)

instance Show LlvmCmpOp where
  show LM_CMP_Eq  = "eq"
  show LM_CMP_Ne  = "ne"
  show LM_CMP_Ugt = "ugt"
  show LM_CMP_Uge = "uge"
  show LM_CMP_Ult = "ult"
  show LM_CMP_Ule = "ule"
  show LM_CMP_Sgt = "sgt"
  show LM_CMP_Sge = "sge"
  show LM_CMP_Slt = "slt"
  show LM_CMP_Sle = "sle"
  show LM_CMP_Feq = "oeq"
  show LM_CMP_Fne = "one"
  show LM_CMP_Fgt = "ogt"
  show LM_CMP_Fge = "oge"
  show LM_CMP_Flt = "olt"
  show LM_CMP_Fle = "ole"


-- | Llvm binary operators, parameter of the 'Expression_MachOp' constructor of type
--   'Expression'
data LlvmMachOp
  -- Following take two integer, floating point or vector values.
  = LM_MO_Add
  | LM_MO_FAdd
  | LM_MO_Sub
  | LM_MO_FSub
  | LM_MO_Mul
  | LM_MO_UDiv -- unsigned integer or vector division.
  | LM_MO_SDiv -- signed integer ..
  | LM_MO_FDiv -- floating point ..
  | LM_MO_URem -- unsigned integer or vector remainder (mod)
  | LM_MO_SRem -- signed ...
  | LM_MO_FRem -- floating point ...
  -- Left shift
  | LM_MO_Shl
  -- Logical shift right
  -- Shift right, filling with zero
  | LM_MO_LShr
  -- Arithmetic shift right
  -- The most significant bits of the result will be equal to the sign bit of
  -- the left operand.
  | LM_MO_AShr
  -- Bitwise logical operations.
  | LM_MO_And
  | LM_MO_Or
  | LM_MO_Xor
  deriving (Eq)

instance Show LlvmMachOp where
  show LM_MO_Add  = "add"
  show LM_MO_FAdd = "fadd"
  show LM_MO_Sub  = "sub"
  show LM_MO_FSub = "fsub"
  show LM_MO_Mul  = "mul"
  show LM_MO_UDiv = "udiv"
  show LM_MO_SDiv = "sdiv"
  show LM_MO_FDiv = "fdiv"
  show LM_MO_URem = "urem"
  show LM_MO_SRem = "srem"
  show LM_MO_FRem = "frem"
  show LM_MO_Shl  = "shl"
  show LM_MO_LShr = "lshr"
  show LM_MO_AShr = "ashr"
  show LM_MO_And  = "and"
  show LM_MO_Or   = "or"
  show LM_MO_Xor  = "xor"


-- | The various LLVM cast operations.
--   Converts between the various LLVM types.
data LlvmCastOp
  = LM_Trunc
  | LM_Zext
  | LM_Sext
  | LM_Fptrunc
  | LM_Fpext
  | LM_Fptoui
  | LM_Fptosi
  | LM_Uitofp
  | LM_Sitofp
  | LM_Ptrtoint
  | LM_Inttoptr
  | LM_Bitcast
  deriving (Eq)

instance Show LlvmCastOp where
  show LM_Trunc    = "trunc"
  show LM_Zext     = "zext"
  show LM_Sext     = "sext"
  show LM_Fptrunc  = "fptrunc"
  show LM_Fpext    = "fpext"
  show LM_Fptoui   = "fptoui"
  show LM_Fptosi   = "fptosi"
  show LM_Uitofp   = "uitofp"
  show LM_Sitofp   = "sitofp"
  show LM_Ptrtoint = "ptrtoint"
  show LM_Inttoptr = "inttoptr"
  show LM_Bitcast  = "bitcast"


-- | Some nice types
type LMGlobal   = (LlvmVar, Maybe LlvmStatic)
type LMConstant = (LlvmVar, LlvmStatic)
type LMString   = String


-- | Llvm variables
data LlvmVar
  -- references to variables with a global scope.
  = LMGlobalVar LMString LlvmType LlvmLinkageType
  -- references to variables local for a function or parameters.
  | LMLocalVar  LMString LlvmType
  -- a constant variable
  | LMLitVar  LlvmLit
  deriving (Eq)

instance Show LlvmVar where
  show (LMLitVar x) = show x
  show (x         ) = show (getVarType x) ++ " " ++ getName x


-- | Llvm literal data
data LlvmLit
  -- refers to an integer constant as (i64 42).
  = LMIntLit Integer LlvmType
  -- floating point literal
  | LMFloatLit Rational LlvmType
  deriving (Eq)

instance Show LlvmLit where
  show l = show (getLitType l) ++ " " ++ getLit l


-- | Llvm Static data
--   This can be declared in constants
data LlvmStatic
  -- | A comment in a static section
  = LMComment LMString
  -- | A static variant of a literal value
  | LMStaticLit LlvmLit
  -- | For uninitialised data
  | LMUninitType LlvmType
  -- defines a static LMString
  | LMStatStr LMString LlvmType
  -- structure type
  | LMStaticStruc [LlvmStatic] LlvmType
  -- pointer to other data
  | LMStaticPointer LlvmVar
  
  -- static expressions, could split out but leave
  -- for moment for ease of use. Not many of them.
  
  -- pointer to int
  | LMPtoI LlvmStatic LlvmType
  -- constant add
  | LMAdd LlvmStatic LlvmStatic
  -- constant sub
  | LMSub LlvmStatic LlvmStatic
  deriving (Eq)

instance Show LlvmStatic where
  show (LMComment       s) = "; " ++ s
  show (LMStaticLit   l  ) = show l
  show (LMUninitType    t) = show t ++ " undef"
  show (LMStatStr     s t) = show t ++ " c\"" ++ s ++ "\\00\""

  show (LMStaticStruc d t)
      = let struc = case d of
              [] -> "{}"
              ts -> "{" ++
                      (show (head ts) ++ concat (map (\x -> "," ++ show x)
                          (tail ts)))
                      ++ "}"
        in show t ++ " " ++ struc

  show (LMStaticPointer v) = show v

  show (LMPtoI v t)
      = show t ++ " ptrtoint (" ++ show v ++ " to " ++ show t ++ ")"

  show (LMAdd s1 s2)
      = let ty1 = getStatType s1
        in if ty1 == getStatType s2
                then show ty1 ++ " add (" ++ show s1 ++ "," ++ show s2 ++ ")"
                else error $ "LMAdd with different types! s1: "
                        ++ show s1 ++ ", s2: " ++ show s2
  show (LMSub s1 s2)
      = let ty1 = getStatType s1
        in if ty1 == getStatType s2
                then show ty1 ++ " sub (" ++ show s1 ++ "," ++ show s2 ++ ")"
                else error $ "LMSub with different types! s1: "
                        ++ show s1 ++ ", s2: " ++ show s2


-- | Llvm types.
data LlvmType
  -- An integer with a given width in bits.
  = LMInt Int
  -- floating point types
  | LMFloat -- 32 bit
  | LMDouble -- 64 bit
  | LMFloat80 -- 80 bit (x86 only)
  | LMFloat128 -- 128 bit
  -- A pointer to a 'LlvmType'
  | LMPointer LlvmType
  -- An array of 'LlvmType'
  | LMArray Int LlvmType
  -- A 'LlvmVar' can represent a label (address) 
  | LMLabel
  -- Void
  | LMVoid
  -- Struct
  | LMStruct [LlvmType]
  -- function type, used to create pointers to functions
  | LMFunction LlvmFunctionDecl
  -- a type alias
  | LMAlias LMString LlvmType
  deriving (Eq)

instance Show LlvmType where
  show (LMInt size    ) = "i" ++ show size
  show (LMFloat       ) = "float"
  show (LMDouble      ) = "double"
  show (LMFloat80     ) = "x86_fp80"
  show (LMFloat128    ) = "fp128"
  show (LMPointer x   ) = show x ++ "*"
  show (LMArray nr tp ) = "[" ++ show nr ++ " x " ++ show tp ++ "]"       
  show (LMLabel       ) = "label"
  show (LMVoid        ) = "void"
  show (LMStruct tys  ) = "{" ++ (commaCat tys) ++ "}"

  show (LMFunction (LlvmFunctionDecl _ _ _ r VarArgs p))
        = (show r) ++ " (" ++ (commaCat p) ++ ", ...)"
  show (LMFunction (LlvmFunctionDecl _ _ _ r FixedArgs p))
        = (show r) ++ " (" ++ (commaCat p) ++ ")"

  show (LMAlias s _   ) = "%" ++ s

commaCat :: Show a => [a] -> String
commaCat [] = ""
commaCat x  = show (head x) ++ (concat $ map (\y -> "," ++ show y) (tail x))

-- | Test if a 'LlvmVar' is global.
isGlobal :: LlvmVar -> Bool
isGlobal (LMGlobalVar _ _ _) = True
isGlobal _                   = False  
  
-- | Return the variable name or value of the 'LlvmVar'
--   in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
getName :: LlvmVar -> String
getName (LMGlobalVar x _ _ ) = "@" ++ x
getName (LMLocalVar  x _)    = "%" ++ x
getName (LMLitVar x)         = getLit x

-- | Print a literal value. No type.
getLit :: LlvmLit -> String
getLit (LMIntLit i _)   = show i
getLit (LMFloatLit r _) =
    let d = fromRational r :: Double
        -- BUG: INFIN.. aren't valid LLVM values, will cause compile error
        str | isInfinite d && d < 0 = "-INFINITY"
            | isInfinite d          = "INFINITY"
            | isNaN d               = "NAN"
            | otherwise             = show d
    in str

-- | Return the variable name or value of the 'LlvmVar'
--   in a plain textual representation (e.g. @x@, @y@ or @42@).
getPlainName :: LlvmVar -> String
getPlainName (LMGlobalVar x _ _) = x
getPlainName (LMLocalVar  x _)   = x
getPlainName (LMLitVar x)        = getLit x

-- | Return the 'LlvmType' of the 'LlvmVar'
getVarType :: LlvmVar -> LlvmType
getVarType (LMGlobalVar _ y _) = y
getVarType (LMLocalVar _  y  ) = y
getVarType (LMLitVar      l  ) = getLitType l

-- | Return the 'LlvmLinkageType' for a 'LlvmVar'
getLink :: LlvmVar -> LlvmLinkageType
getLink (LMGlobalVar _ _ l) = l
getLink _                   = ExternallyVisible
 
-- | Return the 'LlvmType' of a 'LlvmLit'
getLitType :: LlvmLit -> LlvmType
getLitType (LMIntLit   _ t) = t
getLitType (LMFloatLit _ t) = t

-- | Return the 'LlvmType' of the 'LlvmStatic'
getStatType :: LlvmStatic -> LlvmType
getStatType (LMStaticLit   l  ) = getLitType l
getStatType (LMUninitType    t) = t
getStatType (LMStatStr     _ t) = t
getStatType (LMStaticStruc _ t) = t
getStatType (LMStaticPointer v) = getVarType v
getStatType (LMPtoI        _ t) = t
getStatType (LMAdd         t _) = getStatType t
getStatType (LMSub         t _) = getStatType t
getStatType (LMComment       _) = error "Can't call getStatType on LMComment!"

-- | Return the 'LlvmType' of the 'LMGlobal'
getGlobalType :: LMGlobal -> LlvmType
getGlobalType (v, _) = getVarType v

-- | Return the 'LlvmVar' part of a 'LMGlobal'
getGlobalVar :: LMGlobal -> LlvmVar
getGlobalVar (v, _) = v

-- | Shortcut for 64 bit integer 
i64 :: LlvmType
i64 = LMInt 64

-- | Shortcut for 32 bit integer
i32 :: LlvmType
i32 = LMInt 32

-- | Shortcut for 16 bit integer
i16 :: LlvmType
i16 = LMInt 16

-- | Shortcut for 8 bit integer (byte)
i8 :: LlvmType
i8  = LMInt 8

-- | Shortcut for 1 bit integer (boolean)
i1 :: LlvmType
i1  = LMInt 1
   
-- | Add a pointer indirection to the supplied type. 'Label' and 'Void'
--  cannot be lifted.
pLift :: LlvmType -> LlvmType
pLift (LMLabel) = error "Labels are unliftable"
pLift (LMVoid)  = error "Voids are unliftable"
pLift x         = LMPointer x

-- | Remove the pointer indirection of the supplied type. Only 'Pointer' 
--  constructors can be lowered.
pLower :: LlvmType -> LlvmType
pLower (LMPointer x) = x
pLower x             = error $ show x ++ " is a unlowerable type, need a pointer"

pVarLower :: LlvmVar -> LlvmVar
pVarLower (LMGlobalVar s t l) = LMGlobalVar s (pLower t) l
pVarLower (LMLocalVar s t   ) = LMLocalVar s (pLower t)
pVarLower (LMLitVar _)        = error $ "Can't lower a literal type!"

-- | Test if the given 'LlvmType' is an integer
isInt :: LlvmType -> Bool
isInt (LMInt _) = True
isInt _         = False

-- | Test if the given LlvmType is a floating point type
isFloat :: LlvmType -> Bool
isFloat LMFloat    = True
isFloat LMDouble   = True
isFloat LMFloat80  = True
isFloat LMFloat128 = True
isFloat _          = False

-- | Test if the given 'LlvmType' is a 'Pointer'
isPointer :: LlvmType -> Bool
isPointer (LMPointer _) = True
isPointer _             = False

-- | Width in bits of an LlvmType, returns 0 if not applicable
llvmWidthInBits :: LlvmType -> Int
llvmWidthInBits (LMInt n)        = n
llvmWidthInBits (LMFloat)        = 32
llvmWidthInBits (LMDouble)       = 64
llvmWidthInBits (LMFloat80)      = 80
llvmWidthInBits (LMFloat128)     = 128
-- Really should return the pointer width, but can't do that without support
-- FIX: return pointer width
llvmWidthInBits (LMPointer t)    = llvmWidthInBits t
llvmWidthInBits (LMArray _ t)    = llvmWidthInBits t
llvmWidthInBits LMLabel          = 0
llvmWidthInBits LMVoid           = 0
llvmWidthInBits (LMStruct tys)   = sum $ map llvmWidthInBits tys
llvmWidthInBits (LMFunction  _)  = 0
llvmWidthInBits (LMAlias _ t)    = llvmWidthInBits t


-- -----------------------------------------------------------------------------
-- Functions
--

-- | A function declaration has the following elements
--    * name:       Unique identifier of the function.
--    * internal:   LinkageType of the function.
--    * funCc:      The calling convention of the function.
--    * returnType: Type of the returned value
--    * varargs:    ParameterListType indicating if this function uses varargs
--    * params:     Signature of the parameters 
data LlvmFunctionDecl = LlvmFunctionDecl {
        decName       :: LMString,
        funcLinkage   :: LlvmLinkageType,
        funcCc        :: LlvmCallConvention,
        decReturnType :: LlvmType,
        decVarargs    :: LlvmParameterListType,
        decParams     :: [LlvmType]
  }

instance Show LlvmFunctionDecl where
  show (LlvmFunctionDecl n l c r VarArgs p)
        = (show l) ++ " " ++  (show c) ++ " " ++ (show r)
            ++ " @" ++ n ++ "(" ++ (commaCat p) ++ ", ...)"
  show (LlvmFunctionDecl n l c r FixedArgs p)
        = (show l) ++ " " ++  (show c) ++ " " ++ (show r)
            ++ " @" ++ n ++ "(" ++ (commaCat p) ++ ")"

instance Eq LlvmFunctionDecl where
  (LlvmFunctionDecl n1 l1 c1 r1 v1 p1) == (LlvmFunctionDecl n2 l2 c2 r2 v2 p2)
        = (n1 == n2) && (l1 == l2) && (c1 == c2) && (r1 == r2)
            && (v1 == v2) && (p1 == p2)

type LlvmFunctionDecls = [LlvmFunctionDecl]


-- | Different types to call a function.
data LlvmCallType
  -- Normal call, allocate a new stack frame.
  = StdCall
  -- Tail call, perform the call in the current stack frame.
  | TailCall
  deriving (Eq,Show)

-- | Different calling conventions a function can use.
data LlvmCallConvention
  -- | The C calling convention
  --   This calling convention (the default if no other calling convention is
  --   specified) matches the target C calling conventions. This calling
  --   convention supports varargs function calls and tolerates some mismatch in
  --   the declared prototype and implemented declaration of the function (as
  --   does normal C).
  = CC_Ccc
  -- | This calling convention attempts to make calls as fast as possible
  --   (e.g. by passing things in registers). This calling convention allows
  --   the target to use whatever tricks it wants to produce fast code for the
  --   target, without having to conform to an externally specified ABI
  --   (Application Binary Interface). Implementations of this convention should
  --   allow arbitrary tail call optimization to be supported. This calling
  --   convention does not support varargs and requires the prototype of al
  --   callees to exactly match the prototype of the function definition.
  | CC_Fastcc
  -- | This calling convention attempts to make code in the caller as efficient
  --   as possible under the assumption that the call is not commonly executed.
  --   As such, these calls often preserve all registers so that the call does
  --   not break any live ranges in the caller side. This calling convention
  --   does not support varargs and requires the prototype of all callees to
  --   exactly match the prototype of the function definition.
  | CC_Coldcc
  -- | Any calling convention may be specified by number, allowing
  --   target-specific calling conventions to be used. Target specific calling
  --   conventions start at 64.
  | CC_Ncc Int  
  -- | X86 Specific 'StdCall' convention. LLVM includes a specific alias for it
  -- rather than just using CC_Ncc.
  | CC_X86_Stdcc 
  deriving (Eq)

instance Show LlvmCallConvention where
  show CC_Ccc       = "ccc"
  show CC_Fastcc    = "fastcc"
  show CC_Coldcc    = "coldcc"
  show (CC_Ncc i)   = "cc " ++ (show i)
  show CC_X86_Stdcc = "x86_stdcallcc"


-- | Functions can have a fixed amount of parameters, or a variable amount.
data LlvmParameterListType
  -- Fixed amount of arguments.
  = FixedArgs
  -- Variable amount of arguments.
  | VarArgs
  deriving (Eq,Show)


-- | Linkage type of a symbol. The description of the constructors is copied from
--  the Llvm Assembly Language Reference Manual 
--  <http://www.llvm.org/docs/LangRef.html#linkage>, because they correspond to 
--  the Llvm linkage types.
data LlvmLinkageType
  -- | Global values with internal linkage are only directly accessible by 
  --   objects in the current module. In particular, linking code into a module
  --   with an internal global value may cause the internal to be renamed as 
  --   necessary to avoid collisions. Because the symbol is internal to the 
  --   module, all references can be updated. This corresponds to the notion 
  --   of the @static@ keyword in C.
  = Internal
  -- | Globals with @linkonce@ linkage are merged with other globals of the 
  --   same name when linkage occurs. This is typically used to implement 
  --   inline functions, templates, or other code which must be generated 
  --   in each translation unit that uses it. Unreferenced linkonce globals are
  --   allowed to be discarded.
  | LinkOnce
  -- | @weak@ linkage is exactly the same as linkonce linkage, except that 
  --   unreferenced weak globals may not be discarded. This is used for globals
  --   that may be emitted in multiple translation units, but that are not 
  --   guaranteed to be emitted into every translation unit that uses them. One
  --   example of this are common globals in C, such as @int X;@ at global 
  --   scope.
  | Weak
  -- | @appending@ linkage may only be applied to global variables of pointer 
  --   to array type. When two global variables with appending linkage are 
  --   linked together, the two global arrays are appended together. This is 
  --   the Llvm, typesafe, equivalent of having the system linker append 
  --   together @sections@ with identical names when .o files are linked.
  | Appending
  -- | The semantics of this linkage follow the ELF model: the symbol is weak 
  --   until linked, if not linked, the symbol becomes null instead of being an
  --   undefined reference.
  | ExternWeak
  -- | The symbol participates in linkage and can be used to resolve external 
  --   symbol references.
  | ExternallyVisible
  -- | Alias for 'ExternallyVisible' but with explicit textual form in LLVM
  --   assembly.
  | External
  deriving (Eq)
  
instance Show LlvmLinkageType where
  show Internal          = "internal"
  show LinkOnce          = "linkonce"
  show Weak              = "weak"       
  show Appending         = "appending"
  show ExternWeak        = "extern_weak"
  -- ExternallyVisible does not have a textual representation, it is 
  -- the linkage type a function resolves to if no other is specified
  -- in Llvm.  
  show ExternallyVisible = ""
  show External          = "external"

-- | Llvm Function Attributes
--
--   Function attributes are set to communicate additional information about a
--   function. Function attributes are considered to be part of the function,
--   not of the function type, so functions with different parameter attributes
--   can have the same function type.
--
--   Functions can have multiple attributes.
--
--   Descriptions taken from <http://llvm.org/docs/LangRef.html#fnattrs>
data LlvmFuncAttr
  -- | This attribute indicates that the inliner should attempt to inline this
  --   function into callers whenever possible, ignoring any active inlining
  --   size threshold for this caller.
  = AlwaysInline
  -- | This attribute indicates that the source code contained a hint that
  --   inlining this function is desirable (such as the "inline" keyword in
  --   C/C++). It is just a hint; it imposes no requirements on the inliner.
  | InlineHint
  -- | This attribute indicates that the inliner should never inline this
  --   function in any situation. This attribute may not be used together
  --   with the alwaysinline attribute.
  | NoInline
  -- | This attribute suggests that optimization passes and code generator
  --   passes make choices that keep the code size of this function low, and
  --   otherwise do optimizations specifically to reduce code size.
  | OptSize
  -- | This function attribute indicates that the function never returns
  --   normally. This produces undefined behavior at runtime if the function
  --   ever does dynamically return.
  | NoReturn
  -- | This function attribute indicates that the function never returns with
  --   an unwind or exceptional control flow. If the function does unwind, its
  --   runtime behavior is undefined.
  | NoUnwind
  -- | This attribute indicates that the function computes its result (or
  --   decides to unwind an exception) based strictly on its arguments, without
  --   dereferencing any pointer arguments or otherwise accessing any mutable
  --   state (e.g. memory, control registers, etc) visible to caller functions.
  --   It does not write through any pointer arguments (including byval
  --   arguments) and never changes any state visible to callers. This means
  --   that it cannot unwind exceptions by calling the C++ exception throwing
  --   methods, but could use the unwind instruction.
  | ReadNone
  -- | This attribute indicates that the function does not write through any
  --   pointer arguments (including byval arguments) or otherwise modify any
  --   state (e.g. memory, control registers, etc) visible to caller functions.
  --   It may dereference pointer arguments and read state that may be set in
  --   the caller. A readonly function always returns the same value (or unwinds
  --   an exception identically) when called with the same set of arguments and
  --   global state. It cannot unwind an exception by calling the C++ exception
  --   throwing methods, but may use the unwind instruction.
  | ReadOnly
  -- | This attribute indicates that the function should emit a stack smashing
  --   protector. It is in the form of a "canary"—a random value placed on the
  --   stack before the local variables that's checked upon return from the
  --   function to see if it has been overwritten. A heuristic is used to
  --   determine if a function needs stack protectors or not.
  --
  --   If a function that has an ssp attribute is inlined into a function that
  --   doesn't have an ssp attribute, then the resulting function will have an
  --   ssp attribute.
  | Ssp
  -- | This attribute indicates that the function should always emit a stack
  --   smashing protector. This overrides the ssp function attribute.
  --
  --   If a function that has an sspreq attribute is inlined into a function
  --   that doesn't have an sspreq attribute or which has an ssp attribute,
  --   then the resulting function will have an sspreq attribute.
  | SspReq
  -- | This attribute indicates that the code generator should not use a red
  --   zone, even if the target-specific ABI normally permits it.
  | NoRedZone
  -- | This attributes disables implicit floating point instructions.
  | NoImplicitFloat
  -- | This attribute disables prologue / epilogue emission for the function.
  --   This can have very system-specific consequences.
  | Naked
  deriving (Eq)

instance Show LlvmFuncAttr where
  show AlwaysInline    = "alwaysinline"
  show InlineHint      = "inlinehint"
  show NoInline        = "noinline"
  show OptSize         = "optsize"
  show NoReturn        = "noreturn"
  show NoUnwind        = "nounwind"
  show ReadNone        = "readnon"
  show ReadOnly        = "readonly"
  show Ssp             = "ssp"
  show SspReq          = "ssqreq"
  show NoRedZone       = "noredzone"
  show NoImplicitFloat = "noimplicitfloat"
  show Naked           = "naked"

