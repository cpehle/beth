{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Assembler where

import Control.Monad.Trans.State
import Control.Lens
import Data.Int
import Data.Text
import Data.Word
import Data.Vector as V
import Data.Vector.Unboxed as U
import Data.Bits

import Register


data Inst =
    Add
  | And
  | B
  | Cmp
  | Div
  | Ext
  | Load
  | Or
  | Sl
  | Sr
  | Store
  | Sub
  | Xor


data Scalar =
    U8
  | U16
  | U32
    deriving (Show)

data Val =
    I Word32
  | R Reg
  | VR VReg
  | CRF Word32
  | A Word32
    deriving (Show)



-- |  Power PC opcodes have several different forms, the following are convenience functions for constructing opcodes of the different forms. The implementation is specific to the 32bit Instruction set.

dform :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
dform opcode ra rb d = (d `shift` 16) .|. (ra `shift` 11) .|. (rb `shift` 6) .|. opcode

xform :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
xform opcode rc ra rb xo flag = (flag `shift` 31) .|. (xo `shift` 21) .|. (rb `shift` 16) .|. (ra `shift` 11) .|. (rc `shift` 6) .|. opcode

xoform :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
xoform opcode rt ra rb oe xo rc = (rc `shift` 31) .|. (xo `shift` 22) .|. (oe `shift` 21) .|. (rb `shift` 16) .|. (ra `shift` 11) .|. (rt `shift` 6) .|. opcode


-- | The overall state of the Assembler
data AsmState = AsmState {
  _next :: Int,
  _nextBlock :: Int,
  _currentBlock :: Int,
  _registers :: RegisterState Reg,
  _vectorRegisters :: RegisterState VReg,
  _instructions :: [Word32]
} deriving (Show)

-- | State of the Register allocator
data RegisterState a = RegisterState {
  _usedRegisters :: [a],
  _availableRegisters :: [a],
  _savedRegisters :: [a]
} deriving (Show)


initialAsmState :: AsmState
initialAsmState = AsmState {
  _next = 0,
  _nextBlock = 0,
  _currentBlock = 0,
  _instructions = [],
  _registers = RegisterState {
      _usedRegisters = [],
      _availableRegisters = [R0 .. R31],
      _savedRegisters = []
      },
  _vectorRegisters = RegisterState {
      _usedRegisters = [],
      _availableRegisters = [VR0 .. VR31],
      _savedRegisters = []
  }
}

makeLenses ''AsmState
makeLenses ''RegisterState

type Asm a = StateT AsmState IO a

emit :: Word32 -> Asm ()
emit i = instructions <>= [i]


crf :: Asm Val
crf = return $ CRF 0

add :: Val -> Val -> Val -> Asm () -- | see p. 68 of PowerISA Version 2.07 B
add (R d) (R s) (R s') = do
  emit (xoform 31 (regIndex d) (regIndex s) (regIndex s') 0 266 0)
add (R d) (R s) (I s') = do
  emit (dform 14 (regIndex d) (regIndex s) s')
add _ _ _ = error "Illegal arguments to add."

mul :: Val -> Val -> Val -> Asm () -- | see p. XX of PowerISA Version 2.07 B
mul (R d) (R s) (R s') = do
  emit (xoform 31 (regIndex d) (regIndex s) (regIndex s') 0 235 0)
mul (R d) (R s) (I s') = do
  emit (dform 7 (regIndex d) (regIndex s) s')
mul _ _ _ = error "Illegal arguments to mul."

cmp :: Val -> Val -> Val -> Asm () -- | see p. XX of PowerISA Version 2.07 B
cmp (CRF d) (R s) (R s') =
  emit (xform 31 d (regIndex s) (regIndex s') 0 0)
cmp _ _ _ = error "Illegal arguments to cmp."

div :: Val -> Val -> Val -> Asm () -- | see p. XX of PowerISA Version 2.07 B
div (R d) (R s) (R s') = do
  emit (xoform 31 (regIndex d) (regIndex s) (regIndex s') 0 491 0)
div _ _ _ = error "Illegal arguments to div."

load :: Val -> Val -> Val -> Asm ()
load (R t) (R a) (R b) = do
  emit (xform 31 (regIndex t) (regIndex a) (regIndex b) 87 0)
load _ _ _ = error "Illegal arguments to load."

store :: Val -> Val -> Val -> Asm ()
store = undefined

mov :: Val -> Val -> Asm ()
mov (R d) (R s) = undefined


receive = undefined
label = undefined
jump = undefined

constant c = return $ I c
inc = undefined
set = undefined


for :: Val -> Val -> (Val -> Asm ()) -> Asm ()
for low high action = do
  i <- reg
  loop <- label
  action i
  gt <- crf
  Assembler.cmp gt i high
  jump gt loop

block :: [Val] -> Asm () -> Asm ()
block regs action = do
  registers . savedRegisters <>= (Prelude.foldl (\acc r -> case r of
                                          R r' -> r' : acc
                                          _ -> acc
                                          ) [] regs)
  vectorRegisters . savedRegisters <>= (Prelude.foldl (\ acc r -> case r of
                                           VR r' -> r' : acc
                                           _ -> acc
                                           ) [] regs)
  action
  used <- use $ registers . usedRegisters
  saved <- use $ registers . savedRegisters
  registers . availableRegisters <>= used
  registers . usedRegisters <>= saved

register :: Asm Val -- | Allocate register
register = do
  available <- use $ registers  . availableRegisters
  used <- use $ registers . usedRegisters
  case available of
    [] -> error "Not enough registers available."
    r:rs -> do registers . availableRegisters .= rs
               registers . usedRegisters .= r : used
               return (R r)

withRegister :: (Val -> Asm ()) -> Asm ()
withRegister f = do
  r <- register
  f r
  release r

release :: Val -> Asm () -- | Release register
release (R r) = do
  available <- use $ registers  . availableRegisters
  used <- use $ registers . usedRegisters
  saved <- use $ registers . savedRegisters
  registers . usedRegisters .= Prelude.filter (/= r) used
  registers . savedRegisters .= Prelude.filter (/= r) saved -- | TODO: Should we warn the user if he releases a saved register?
  registers . availableRegisters .= r : available
release (VR r) = do
  available <- use $ vectorRegisters . availableRegisters
  used <- use $ vectorRegisters . usedRegisters
  saved <- use $ vectorRegisters . savedRegisters
  vectorRegisters . usedRegisters .= Prelude.filter (/= r) used
  vectorRegisters . savedRegisters .= Prelude.filter (/= r) saved
  vectorRegisters . availableRegisters .= r : available
release (I _) = error "Cannot release a constant value"
release (A _) = error "Cannot release an address"
release (CRF _) = error "Cannot release a condition field register"



dup :: Val -> Asm (Val, Val)
dup a = do
  r <- register
  r' <- register
  mov r a
  mov r' a
  release a
  return (r,r')


test :: Asm ()
test = do
  a :: Val <- receive
  b :: Val <- receive
  c <- constant 0x3fff -- alternatively let c = constant 0x3ffff
  r <- register
  add r a b





  release r
  loop <- label

  jump loop
  return ()
