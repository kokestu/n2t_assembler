module Grammar where

type Program = [Line]

type Line = Either AInstruction CInstruction

data AInstruction = Todo
data CInstruction = AlsoTodo
