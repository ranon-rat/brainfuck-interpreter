module Tokens where
data Tokens=MoveLeft | MoveRight | Add | Subtract | LoopToken |EndLoopToken|PrintCurrentCell|InputCurrentCell deriving (Eq, Show)
tokens=[    ('<',MoveLeft),    ('>',MoveRight),('+',Add),('-',Subtract),('[',LoopToken ),(']',EndLoopToken ),('.',PrintCurrentCell ),(',',InputCurrentCell )]

isJustToken :: Maybe Tokens -> Bool
isJustToken (Just _) = True
isJustToken _ = False
tokenize::String->[Tokens]
tokenize a= map (\(Just i)->i) $ filter isJustToken (map (\x->lookup x tokens) a)


