{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- -----------------------------------------------------------------------------
-- | GHC LLVM Mangler
--
-- This script processes the assembly produced by LLVM, rearranging the code
-- so that an info table appears before its corresponding function. We also
-- use it to fix up the stack alignment, which needs to be 16 byte aligned
-- but always ends up off by 4 bytes because GHC sets it to the wrong starting
-- value in the RTS.
--
-- We only need this for Mac OS X, other targets don't use it.
--

module LlvmMangler ( llvmFixupAsm ) where

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import LlvmCodeGen.Ppr ( infoSection, iTableSuf )

import Util

{- Configuration. -}
newSection, oldSection, functionSuf, tableSuf, funDivider, eol :: ByteString
newSection  = BS.pack "\n.text\n"
oldSection  = BS.pack infoSection
functionSuf = BS.pack $ if ghciTablesNextToCode then "_info:" else "\n_"
tableSuf    = BS.pack $ "_info" ++ iTableSuf ++ ":"
funDivider  = BS.pack "\n\n"
eol         = BS.pack "\n"


eolPred, dollarPred, commaPred :: Char -> Bool
eolPred = ((==) '\n')
dollarPred = ((==) '$')
commaPred = ((==) ',')

-- | Read in assembly file and process
llvmFixupAsm :: FilePath -> FilePath -> IO ()
llvmFixupAsm f1 f2 = do
    asm <- BS.readFile f1
    BS.writeFile f2 BS.empty
    allTables f2 asm
    return ()

-- | Run over whole assembly file
allTables :: FilePath -> ByteString -> IO ()
allTables f str = do
    rem <- oneTable f str
    if BS.null rem
       then return ()
       else allTables f rem

{- |
  Look for the next function that needs to have its info table
  arranged to be before it and process it. This will print out
  any code before this function, then the info table, then the
  function. It will return the remainder of the assembly code
  to process.

  We rely here on the fact that LLVM prints all global variables
  at the end of the file, so an info table will always appear
  after its function.

  To try to help explain the string searches, here is some
  assembly code that would be processed by this program, with
  split markers placed in it like so, <split marker>:

    [ ...asm code... ]
    jmp *%eax
    <before|fheader>
    .def Main_main_info
    .section TEXT
    .globl _Main_main_info
    _Main_main<bl|al>_info:
        sub $12, %esp
        [ ...asm code... ]
        jmp *%eax
    <fun|after>
    .def .....

    [ ...asm code... ]

        .long 231231
    <bit'|itable_h>
    .section TEXT
    .global _Main_main_entry
    .align 4
    <bit|itable>_Main_main_entry:
        .long 0
        [ ...asm code... ]
    <itable'|ait>
    .section TEXT
-}
oneTable :: FilePath -> ByteString -> IO ByteString
oneTable f str =
    let last' xs = if (null xs) then 0 else last xs

        -- get the function
        (bl, al) = BS.breakSubstring functionSuf str
        start = last' $ BS.findSubstrings funDivider bl
        (before, fheader) = BS.splitAt start bl
        (fun, after) = BS.breakSubstring funDivider al
        label = snd $ BS.breakEnd eolPred bl

        -- get the info table
        ilabel = label `BS.append` tableSuf
        (bit, itable) = BS.breakSubstring ilabel after
        (itable', ait) = BS.breakSubstring funDivider itable
        istart = last' $ BS.findSubstrings funDivider bit
        (bit', iheader) = BS.splitAt istart bit

        -- fixup stack alignment
        fun' = fixupStack fun BS.empty

        -- fix up sections
        fheader' = replaceSection fheader
        iheader' = replaceSection iheader

        function = [before, eol, iheader', itable', eol, fheader', fun', eol]
        remainder = bit' `BS.append` ait
    in if BS.null al
          then do
              BS.appendFile f bl
              return BS.empty

          else if ghciTablesNextToCode
                  then if BS.null itable
                          then error $ "Function without matching info table! ("
                                      ++ (BS.unpack label) ++ ")"
                          else do
                              mapM_ (BS.appendFile f) function
                              return remainder

                  else do
                      -- TNTC not turned on so just fix up stack
                      mapM_ (BS.appendFile f) [before, fheader, fun']
                      return after

-- | Replace the current section in a function or table header with the
-- text section specifier.
replaceSection :: ByteString -> ByteString
replaceSection sec =
    let (s1, s2) = BS.breakSubstring oldSection sec
        s1' = fst $ BS.breakEnd eolPred s1
        s2' = snd $ BS.break eolPred s2
    in s1' `BS.append` newSection `BS.append` s2'


-- | Mac OS X requires that the stack be 16 byte aligned when making a function
-- call (only really required though when making a call that will pass through
-- the dynamic linker). During code generation we marked any points where we
-- make a call that requires this alignment. The alignment isn't correctly
-- generated by LLVM as LLVM rightly assumes that the stack wil be aligned to
-- 16n + 12 on entry (since the function call was 16 byte aligned and the return
-- address should have been pushed, so sub 4). GHC though since it always uses
-- jumps keeps the stack 16 byte aligned on both function calls and function
-- entry. We correct LLVM's alignment then by putting inline assembly in that
-- subtracts and adds 4 to the sp as required.
fixupStack :: ByteString -> ByteString -> ByteString
fixupStack fun nfun | BS.null nfun =
    let -- fixup sub op
        (a, b) = BS.breakSubstring (BS.pack ", %esp\n") fun
        (a', num) = BS.breakEnd dollarPred a
        num' = BS.pack $ show (read (BS.unpack num) + 4::Int)
        fix = a' `BS.append` num'
    in if BS.null b
          then nfun `BS.append` a
          else fixupStack b (nfun `BS.append` fix)

fixupStack fun nfun =
    let -- fixup add ops
        (a, b) = BS.breakSubstring (BS.pack "jmp") fun
        -- We need to avoid processing jumps to labels, they are of the form:
        -- jmp\tL..., jmp\t_f..., jmpl\t_f..., jmpl\t*%eax...
        labelJump = BS.index b 4 == 'L'
        (jmp, b') = BS.break eolPred b
        (a', numx) = BS.breakEnd dollarPred a
        (num, x) = BS.break commaPred numx
        num' = BS.pack $ show (read (BS.unpack num) + 4::Int)
        fix = a' `BS.append` num' `BS.append` x `BS.append` jmp
    in if BS.null b
          then nfun `BS.append` a
          else if labelJump
                then fixupStack b' (nfun `BS.append` a `BS.append` jmp)
                else fixupStack b' (nfun `BS.append` fix)

