module Commands where
import System.Environment
import Repl
import Tokens
import Structure
import Interpreter

helpString::String
helpString = "The only commands that you can use when you arent using the repl are:\n\n\thelp :obviously this is going to show you this\n\trepl : you can use it for interact with the interpreter\n\trun   run <program>  : with this you can run the program  \n There is only two commands in the REPL\n\t:q     \t\tto quit \n\t:help  \t\tto list the coomands\n\t:clear \t\tclear the screan \n\t:reset \t\treset the cells and the current cell\n  :where-im show you where you are\n\t:cells\n\t\n The operators of brainfuck are:\n\n\t+ \tto add in the current cell\n\t- \tto subtract in the current cell\n\t> \tto move to the next cell\n\t< \tto move to the previous cell\n\t. \tto print the current cell\n\t, \tto print a new line\n\n The operators for the loop\n\n\t[   to open a loop(if the value of the current cell if  is 0 \n\t\t\tit will go to the next cell after \"]\") \n\n\t] \tthis close the loop( if the current value of the cell\n\t\t \twhen we evaluate this is 0 , the loop ends\n\t\t \tand if it doesnt the loop iterate again)\n"

noArgumentGiven::String
noArgumentGiven = "\tNo command or enough arguments given\n"++"\tUse \"help\" to see the commands list \n"

executeCommand::IO()
executeCommand  =do
    maybeArgs <- getArgs
    case maybeArgs of
        ["help"] -> putStrLn$ helpString
        ["repl"] -> repl
        ["run",program] -> do
            putStrLn$" Running the program..."
            progr<-readFile program
            interpreter $parse $tokenize progr
        _->error noArgumentGiven
    return ()


