module Structure where
import Tokens
-- just some types for working in the interpreter
data Lexic= Move Int|Change Int|Print|Input |Loop [Lexic]|EndLoop deriving (Eq, Show)

-- some instruction that are not that complicated to parse

normalLexic=[(PrintCurrentCell,Print),    (InputCurrentCell,Input),    (EndLoopToken,EndLoop),(MoveLeft,Move (-1)),    (MoveRight,Move 1),    (Subtract,Change (-1)),    (Add,Change 1)]


parse::[Tokens]-> [Lexic]
parse tokens=
    structurize tokens 0 []
structurize::[Tokens]->Int->[Lexic]-> [Lexic]
structurize tokens pos lexicStruct=do
    if pos==(length tokens) then   lexicStruct
    else do 
        -- so , i know that im going to get something        
        let t=tokens!!pos
        case lookup t normalLexic of
            Just instruction ->
                structurize tokens (pos+1) (lexicStruct++[instruction])
            Nothing -> do
                -- if is a loop i do this
                -- because i want to have a structure
                -- in my loop
                let i=structureLoop tokens (pos+1) 1 
                let loop=Loop $parse $take (i-(pos+1))$drop (pos+1) $tokens 

                structurize tokens (i) (lexicStruct++[loop])

structureLoop::[Tokens]->Int->Int->Int
--if i dont need to loop anymore i return the position
structureLoop tokens i 0=i
-- if i already know where is the close loop ,I just return i
structureLoop tokens i needed=
    if i==(length tokens)  then i 
    else case tokens!!i of
        -- if i find a new loop inside a loop i do this
        LoopToken ->structureLoop tokens (i+1) (needed+1)
        -- if i find the end of the loop i only subtract the needed
        EndLoopToken->structureLoop tokens (i+1) (needed-1)
        _->structureLoop tokens (i+1) needed