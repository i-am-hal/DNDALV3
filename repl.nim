#[
This is the repl for the DNDAL3 language.
Author: ALastar Slater
Date: 8/11/2019 
]#
import interpreter, random, strutils, strformat

#This is the repl for the language
proc repl =
    randomize()
    echo "D&DAL - Dungeons and Dragons Automation Language"
    echo "Enter Ctrl+Z or 'quit' to quit."
    stdout.write "\n" #Add new line buffer between this and prompt

    var done = false #If we are done yet
    #Keep going while not told to stop
    while not done:
        try:
            stdout.write "D&DAL> " #The prompt

            let
                input = stdin.readLine() #Save input
                #Get all of the words
                words = input.strip().toUpperAscii().split()
            
            #If the user gave no input, try again
            if input.strip() == "":
                continue
            
            #Exit the program
            elif len(words) > 0 and words[0] == "QUIT":
                break
            
            #Will check the types of values and the operations being used
            var typeChecker = newTypeChecker(input)
            #Check the types and operations
            typeChecker.check()
            
            #Raises the error to the user and continues
            if typeChecker.error == true:
                echo fmt"{typeChecker.errorStr}"
                stdout.write "\n"
                continue
            
            #Evaluates the input from the user
            let result = eval(typeChecker.ast)

            #Skip a few lines
            if result.evalType == ENil:
                stdout.write "\n\n"
                continue
            
            echo result #Otherwise, print result
            
        
        #Exit out of the loop
        except EOFError:
            done = true

    #Say goodbye to the user
    echo "Goodbye."

when isMainModule:
    repl()