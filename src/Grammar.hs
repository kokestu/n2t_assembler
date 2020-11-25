module Grammar where

type Program = [Line]

data Line = LIn LInstruction
          | AIn AInstruction 
          | CIn CInstruction
          deriving (Show)

data VirtualRegister = R Int deriving (Show)

data PredefinedPointer = SP
                        | LCL
                        | ARG
                        | THIS
                        | THAT
                        deriving (Show)

data IOPointer = SCREEN
               | KBD
               deriving (Show)

data Symbol = VR VirtualRegister
            | PP PredefinedPointer
            | IP IOPointer
            | UDefSymbol String
            deriving (Show)

data LInstruction = Label String deriving (Show)

data AInstruction = AtInt Int
                  | AtSymbol Symbol
                  deriving (Show)

data Reg = D | A | M deriving (Show)

data DestReg = Single Reg
             | Double Reg Reg
             | Triple Reg Reg Reg
             | RNull
             deriving (Show)

data ConstExpr = Zero
               | One
               | Register Reg
               deriving (Show)
 
data Expr = C ConstExpr
          | Add ConstExpr ConstExpr
          | Minus ConstExpr ConstExpr
          | And ConstExpr ConstExpr
          | Or ConstExpr ConstExpr
          | Not ConstExpr
          deriving (Show)


data Assignment = Ass DestReg Expr deriving (Show)

data Jump = JMP
          | JEQ
          | JNE
          | JLT
          | JLE
          | JGT
          | JGE
          | JNull
          deriving (Show)

data CInstruction = CAss Assignment
                  | JAss Assignment Jump
                  | JExpr Expr Jump
                  deriving (Show)

