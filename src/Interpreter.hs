module Interpreter where
import Structure
import Control.Concurrent

interpreter::[Lexic]->IO()
interpreter lexics =do
    interpreterWithMoreStuff lexics 0 [0,0,0,0,0]  0
    return ()
thisShitToIO::[Int]->Int ->IO([Int],Int)
thisShitToIO x y =do
    return (x,y)
interpreterWithMoreStuff::[Lexic]->Int->[Int]->Int->IO([Int],Int)
interpreterWithMoreStuff instructions pos cells currentCell =do 
    if pos>=length instructions then return (cells,currentCell)
    else do
       
      
        case  instructions!!pos of
            Move a-> 
                
                interpreterWithMoreStuff instructions (pos+1)  (if (currentCell+a)<length cells then  cells else  cells++[0]) (abs (currentCell+a))
            Change a->do 
                let (x,_:ys) =splitAt currentCell cells
                interpreterWithMoreStuff instructions (pos+1)  (x++(cells!!currentCell+a):ys) currentCell
            Print->do 
                putChar (toEnum (cells!!currentCell) :: Char )
                interpreterWithMoreStuff instructions (pos+1)  cells currentCell
            Input->do 
                indu<-getChar 
                let (x,_:ys) =splitAt currentCell cells
                interpreterWithMoreStuff instructions (pos+1)  (x++(fromEnum indu):ys) currentCell
          
            EndLoop->do
                (interpreterWithMoreStuff instructions (if cells!!currentCell/=0 then 0 else (length instructions))  cells currentCell   )             
            Loop a->do
                if cells!!currentCell==0 then do
                    interpreterWithMoreStuff instructions (pos+1)  (cells) currentCell
                else do
                    (cells,currentCell)<-interpreterWithMoreStuff a 0 (cells) currentCell
                    interpreterWithMoreStuff instructions (pos+1)  (cells) currentCell