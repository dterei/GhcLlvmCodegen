module Llvm.Types where

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

-- | Llvm binary operators, parameter of the 'Expression_MachOp' constructor of type
--   'Expression'
data LlvmMachOp
  -- Following take two integer, floating point or vector values.
  = LM_MO_Add
  | LM_MO_Sub
  | LM_MO_Mul
  | LM_MO_U_Div -- unsigned integer or vector division.
  | LM_MO_S_Div -- signed integer ..
  | LM_MO_F_Div -- floating point ..
  | LM_MO_U_Rem -- unsigned integer or vector remainder (mod)
  | LM_MO_S_Rem -- signed ...
  | LM_MO_F_Rem -- floating point ...
  -- Left shift
  | LM_MO_Shl
  -- Logical shift right
  -- Shift right, filling with zero
  | LM_MO_L_Shr
  -- Arithmetic shift right
  -- The most significant bits of the result will be equal to the sign bit of
  -- the left operand.
  | LM_MO_A_Shr
  -- Bitwise logical operations.
  | LM_MO_And
  | LM_MO_Or
  | LM_MO_Xor
  deriving (Eq)

instance Show LlvmMachOp where
  show LM_MO_Add  = "add"
  show LM_MO_Sub  = "sub"
  show LM_MO_Mul  = "mul"
  show LM_MO_U_Div = "udiv"
  show LM_MO_S_Div = "sdiv"
  show LM_MO_F_Div = "fdiv"
  show LM_MO_U_Rem = "urem"
  show LM_MO_S_Rem = "srem"
  show LM_MO_F_Rem = "frem"
  show LM_MO_Shl  = "shl"
  show LM_MO_L_Shr = "lshr"
  show LM_MO_A_Shr = "ashr"
  show LM_MO_And  = "and"
  show LM_MO_Or   = "or"
  show LM_MO_Xor  = "xor"


-- | Some nice types
type LMGlobal   = (LlvmVar, Maybe LlvmStatic)
type LMConstant = (LlvmVar, LlvmStatic)
type LMString   = String


-- | Llvm variables
data LlvmVar
  -- references to variables with a global scope.
  = LMGlobalVar String LlvmType LlvmLinkageType
  -- references to variables local for a function or parameters.
  | LMLocalVar  String LlvmType
  -- a constant variable
  | LMLitVar  LlvmLit
  deriving (Eq)

instance Show LlvmVar where
  show x = show (getVarType x) ++ " " ++ getName x


-- | Llvm literal data
data LlvmLit
  -- refers to an integer constant as (i64 42).
  = LMIntLit Integer LlvmType
  -- floating point literal
  | LMFloatLit Rational LlvmType
  deriving (Eq)

instance Show LlvmLit where
  show (LMIntLit i t) = show t ++ " " ++ show i
  show (LMFloatLit r t)
      = show t ++ " " ++ str
        where d = fromRational r :: Double
              str | isInfinite d && d < 0 = "-INFINITY"
                  | isInfinite d          = "INFINITY"
                  | isNaN d               = "NAN"
                  | otherwise             = show d


-- | Llvm Static data
--   This can be decalred in constants
data LlvmStatic
  -- | A static variant of a literal value
  = LMStaticLit LlvmLit
  -- | For uninitialised data
  | LMUninitType LlvmType
  -- defines a static string
  | LMString String LlvmType
  -- structure type
  | LMStaticStruc [LlvmStatic] LlvmType
  -- pointer to other data
  | LMStaticPointer LlvmVar
  
  -- static expressions
  
  -- pointer to int
  | LMPtoI LlvmStatic LlvmType
  -- add
  | LMAdd LlvmStatic LlvmStatic
  deriving (Eq)

instance Show LlvmStatic where
  show (LMStaticLit   l  ) = show l
  show (LMUninitType    t) = show t ++ " undef"
  show (LMString      s t) = show t ++ " c\"" ++ s ++ "\\00\""

  show (LMStaticStruc d t)
      = let struc = case d of
              [] -> "{}"
              t  -> "{" ++
                      (show (head t) ++ concat (map (\x -> "," ++ show x)
                          (tail t)))
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
  -- a type alias
  | LMAlias String LlvmType
  deriving (Eq)

instance Show LlvmType where
  show (LMInt size   ) = "i" ++ show size
  show (LMFloat      ) = "float"
  show (LMDouble     ) = "double"
  show (LMFloat80    ) = "x86_fp80"
  show (LMFloat128   ) = "fp128"
  show (LMPointer x  ) = show x ++ "*"
  show (LMArray nr tp) = "[" ++ show nr ++ " x " ++ show tp ++ "]"       
  show (LMLabel      ) = "label"
  show (LMVoid       ) = "void"
  show (LMAlias s t  ) = "%" ++ s

  show (LMStruct tys )
      = case tys of
          [] -> "{}"
          t  -> "{" ++
                  (show (head t) ++ concat (map (\x -> "," ++ show x) (tail t)))
                  ++ "}"


-- | Test if a 'LlvmVar' is global.
isGlobal :: LlvmVar -> Bool
isGlobal (LMGlobalVar _ _ _) = True
isGlobal _                   = False  
  
-- | Return the variable name or value of the 'LlvmVar'
--   in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
getName :: LlvmVar -> String
getName (LMGlobalVar x _ _ ) = "@" ++ x
getName (LMLocalVar  x _)    = "%" ++ x
getName (LMLitVar x)         = show x

-- | Return the variable name or value of the 'LlvmVar'
--   in a plain textual representation (e.g. @x@, @y@ or @42@).
getPlainName :: LlvmVar -> String
getPlainName (LMGlobalVar x _ _) = x
getPlainName (LMLocalVar  x _)   = x
getPlainName (LMLitVar x)        = show x

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

-- | Return the 'LlvmType' of the 'LlvmVar'
getStatType :: LlvmStatic -> LlvmType
getStatType (LMStaticLit   l  ) = getLitType l
getStatType (LMUninitType    t) = t
getStatType (LMString      _ t) = t
getStatType (LMStaticStruc _ t) = t
getStatType (LMStaticPointer v) = getVarType v
getStatType (LMPtoI        _ t) = t

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

-- | Different types to call a function.
data LlvmCallType
  -- Normal call, allocate a new stack frame.
  = StdCall
  -- Tail call, perform the call in the current stack frame.
  | TailCall
  deriving (Eq,Show)

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
