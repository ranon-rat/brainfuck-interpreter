
module Repl (repl) where 
import Interpreter
import Structure
import Tokens

repl=do
    a<-readFile "print.txt"
    putStrLn a

    putStrLn "\n\tA project made with love"

    putStrLn "\tBy ranon-rat and bruh boys\n"

    putStrLn "\t put in the console :help for knowing the commands "    
    
    evaluate [0,0,0,0,0,0,0] 0
clear = putStr "\ESC[2J"-- this clear the screen terminal
helpString::String
helpString = "The only commands that you can use when you arent using the repl are:\n\n\thelp :obviously this is going to show you this\n\trepl : you can use it for interact with the interpreter\n\trun   run <program>  : with this you can run the program  \n There is only two commands in the REPL\n\t:q     \t\tto quit \n\t:help  \t\tto list the coomands\n\t:clear \t\tclear the screan \n\t:reset \t\treset the cells and the current cell\n\t:where-im \tit show you where you are\n\t:cells \t\tit shows you the cells\n\n\t\n The operators of brainfuck are:\n\n\t+ \tto add in the current cell\n\t- \tto subtract in the current cell\n\t> \tto move to the next cell\n\t< \tto move to the previous cell\n\t. \tto print the current cell\n\t, \tto print a new line\n\n The operators for the loop\n\n\t[\tto open a loop(if the value of the current cell if  is 0 \n\t\t \tit will go to the next cell after \"]\") \n\n\t] \tthis close the loop( if the current value of the cell\n\t \t\twhen we evaluate this is 0 , the loop ends\n\t \t\tand if it doesnt the loop iterate again)\n"

evaluate::[Int]->Int->IO()
evaluate cells currentCell = do
    putStrLn "Enter your code: "
    input <- getLine
    case input of -- i see what kind of stufff he is doing
        ":q"->putStrLn "bye bye"-- if is a command like this im going to quit
        ":help"->do 
            putStrLn$ helpString
            evaluate cells currentCell
        ":clear"->clear
        ":reset"->do
            putStrLn "Reseting the cells and the current cell"
            evaluate [0,0,0,0,0,0,0] 0
        ":file"->do-- open a file
            putStrLn "Enter the name of the file: "
            input <- getLine
            
            a<-readFile input
            -- so , with this i receive as an input the cells and the current cell and that its important
            (cells,currentCell)<-interpreterWithMoreStuff (parse $tokenize a) 0 cells currentCell
            putStrLn ""
            evaluate cells currentCell
        
        ":where-im"->do 
             putStrLn $"you are in the cell "++show (currentCell+1)
             evaluate cells currentCell

        ":cells"->do 
            putStrLn $"the cells are: "++show cells
            evaluate cells currentCell
        _->do
            -- just in case that its not a command 
            (cells,currentCell)<-interpreterWithMoreStuff (parse $tokenize input) 0 cells currentCell
            putStrLn ""
            evaluate cells currentCell