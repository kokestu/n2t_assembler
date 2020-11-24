module Grammar where

type Program = [Line]

data Line = LIn LInstruction
          | AIn AInstruction 
          | CIn CInstruction

data VirtualRegister = R Int

data PredefinedPointer = SP
                        | LCL
                        | ARG
                        | THIS
                        | THAT

data IOPointer = Screen
               | Kbd

data Symbol = VR VirtualRegister
            | PP PredefinedPointer
            | IP IOPointer
            | UDefSymbol String

data LInstruction = Label String

data AInstruction = AtNum Int
                  | AtSymbol Symbol

data Reg = D | A | M

data DestReg = Single Reg
             | Double Reg Reg
             | Triple Reg Reg Reg
             | RNull

data ConstExpr = Zero
               | One
               | Register Reg
 
data Expr = C ConstExpr
          | Add ConstExpr ConstExpr
          | Minus ConstExpr ConstExpr
          | And ConstExpr ConstExpr
          | Or ConstExpr ConstExpr
          | Not ConstExpr


data Assignment = Ass DestReg Expr

data Jump = JMP
          | JEQ
          | JNE
          | JLT
          | JLE
          | JGT
          | JGE
          | JNull

data CInstruction = CAss Assignment
                  | JAss Assignment Jump
                  | JExpr Expr Jump

