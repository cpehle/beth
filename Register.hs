module Register where
import Data.Word

data Reg =
    R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | R16
  | R17
  | R18
  | R19
  | R20
  | R21
  | R22
  | R23
  | R24
  | R25
  | R26
  | R27
  | R28
  | R29
  | R30
  | R31
  deriving (Eq, Enum, Show)

data VReg =
    VR0
  | VR1
  | VR2
  | VR3
  | VR4
  | VR5
  | VR6
  | VR7
  | VR8
  | VR9
  | VR10
  | VR11
  | VR12
  | VR13
  | VR14
  | VR15
  | VR16
  | VR17
  | VR18
  | VR19
  | VR20
  | VR21
  | VR22
  | VR23
  | VR24
  | VR25
  | VR26
  | VR27
  | VR28
  | VR29
  | VR30
  | VR31
  deriving (Eq, Enum, Show)


regIndex :: Reg -> Word32
regIndex x = case x of
  R0 -> 0
  R1 -> 1
  R2 -> 2
  R3 -> 3
  R4 -> 4
  R5 -> 5
  R6 -> 6
  R7 -> 7
  R8 -> 8
  R9 -> 9
  R10 -> 10
  R11 -> 11
  R12 -> 12
  R13 -> 13
  R14 -> 14
  R15 -> 15
  R16 -> 16
  R17 -> 17
  R18 -> 18
  R19 -> 19
  R20 -> 20
  R21 -> 21
  R22 -> 22
  R23 -> 23
  R24 -> 24
  R25 -> 25
  R26 -> 26
  R27 -> 27
  R28 -> 28
  R29 -> 29
  R30 -> 30
  R31 -> 31

vregIndex :: VReg -> Word32
vregIndex x = case x of
  VR0 -> 0
  VR1 -> 1
  VR2 -> 2
  VR3 -> 3
  VR4 -> 4
  VR5 -> 5
  VR6 -> 6
  VR7 -> 7
  VR8 -> 8
  VR9 -> 9
  VR10 -> 10
  VR11 -> 11
  VR12 -> 12
  VR13 -> 13
  VR14 -> 14
  VR15 -> 15
  VR16 -> 16
  VR17 -> 17
  VR18 -> 18
  VR19 -> 19
  VR20 -> 20
  VR21 -> 21
  VR22 -> 22
  VR23 -> 23
  VR24 -> 24
  VR25 -> 25
  VR26 -> 26
  VR27 -> 27
  VR28 -> 28
  VR29 -> 29
  VR30 -> 30
  VR31 -> 31
