Compiler and interpreter for simple straight line programs.

> module Comp303a3 where
  
> import Control.Monad
> import Data.Either

> import Store

A straight line program is just a list of assignment statements, where an
expression can contain variables, integer constants and arithmetic operators.

> data Prog = Prog [Stmt]
>             deriving (Show)

> data Stmt = Asgn Type Var Exp
>           | If Exp [Stmt] [Stmt]
>           | While Exp [Stmt]
>           | ProcDef String [(Var,Type)] Type [Stmt]
>           | Return Exp
>             deriving (Show, Eq)

> data Exp = Const Val
>          | Var Var
>      	   | Bin Op Exp Exp
>          | ProcCall [Exp] String
>            deriving (Show, Eq)

> data Op = Plus | Minus | Times | Div | And | Or | Lt | Gt | Eq
>           deriving (Show, Eq)

A procedure is recursive so it can have a return type. This means that procedures that return
procedures are possible although these are not that useful at this stage.

> data Type = Bool | Int | Proc [(Var,Type)] Type deriving(Show,Eq)

The store is a list of variable names and their values. 


> type Var = String

Values can be of a multitude of types such as the Int, Bool, Void or a function.
By storing functions as values we may piggy back some of the variable lookup code.

> data Val = I Int | B Bool deriving (Show, Eq)

Straight line programs are translated into code for a simple stack-oriented
virtual machine.

A VM code program is a list of commands, where a command is either a load
immdiate (which loads a constant onto the stack), load (which loads the value
of a variable onto the stack), sstore (which saves the value at the top of the
stack into memory, and deletes it), or an arithmetic operation (which applies
an operations to the two values at the top of the stack and replaces them by
the result). Procedures are enclosed in a FuncBegin and FuncEnd statement which
determines when the stack frames are added and removed and signify where to jump
to when returning from a procedure.

> type Code = [Command]

> data Command = LoadI Val | Load Var | Store Var | BinOp Op | Ret
>              | CndJump Int | Jmp Int | Label Var | JmpLabel String
>                deriving (Show, Eq)

> type Stack = [Val]

Run a program, by compiling and then executing the resulting code
The program is run with an initially empty store and empty stack, and the
output is just the resulting store.  This could be modified to provide an
initial store.

> run :: Prog -> Store Var Val Command
> run prog =
>   let typecheck = typeCheck prog
>      	code = translate prog
>   in case typecheck of
>      Left msg -> error $ "Typechecker: " ++ msg
>      Right _  -> snd (exec code ([], emptyStore 0 code))

  
Translate straight line program into stack machine code

> translate :: Prog -> Code

> translate (Prog stmts) = trans stmts

> trans :: [Stmt] -> Code

> trans [] = []

> trans (stmt : stmts) = (trans' stmt) ++ (trans stmts)

> trans' :: Stmt -> Code

> trans' (Asgn _ var exp) = transexp exp ++ [Store var]

If statements just eval the expression on the stack then decide which path to put on the code
stream.

> trans' (If exp t e  )   = transexp exp ++ [CndJump $ length el+2] ++ el ++
>                            [Jmp $ length th + 1] ++ th
>   where th = trans t 
>         el = trans e

This is essentially the same as an If statments except we have to jump backwards to do the
comparison

> trans' (While exp t ) = texp ++ [CndJump 2] ++ [Jmp $ length th+1] ++ th
>   where th = trans t ++ [Jmp (-(length th + length texp + 1))]
>         texp = transexp exp

> trans' (Return exp) = transexp exp ++ [Ret]

Here we must label the procedure, bind the arguments in reverse (as it loaded from a stack) and
then put the translated procedure body. Additionally we prepend a jump so we dont run the procedure
when defining it in memory.

> trans' (ProcDef label args _ stmts) = (Jmp $ length proced + 1) : proced
>   where proced =  [Label label] ++ bindArgs ++ trans stmts
>         bindArgs = map Store $ reverse $ map fst args

Here we load the args to the stack then jump to the procedure which will bind them to a name.

> transexp :: Exp -> Code
> transexp (ProcCall args c) = loadArgs ++ [JmpLabel c]
>   where loadArgs = concatMap transexp args

> transexp (Const n) = [LoadI n]

> transexp (Var v) = [Load v]

> transexp (Bin op e1 e2) = transexp e1 ++ transexp e2 ++ [BinOp op]

Execute a stack code program

> exec :: Code -> (Stack, Store Var Val Command) -> (Stack, Store Var Val Command)

> exec [] ss = ss

The jump commands find the new code to run based of there corresponding jump commands implemented
in store. In the case of CndJump if the test is false we just jump by 1 simulating normal progress.

> exec (CndJump amt: cmds) (B test:stack, store) = exec cmds' (stack, store')
>       where (store', cmds') = if test then jump amt store else jump 1 store

> exec (Jmp amt: cmds) (stack, store) = exec cmds' (stack, store')
>       where (store', cmds') = jump amt store 

> exec (JmpLabel label: _)  (stack, store) = exec cmds' (stack, store')
>       where (store', cmds') = jumpLabel (Label label) store

> exec (Ret : _)  (stack, store) = exec cmds' (stack, store')
>       where (store', cmds') = jumpReturn store


If we are executing a non jump command we must increment the program counter by 1

> exec (cmd : cmds) ss = exec cmds $ incPc <$> exec' cmd ss

> exec' :: Command -> (Stack, Store Var Val Command) -> (Stack, Store Var Val Command)

> exec' (LoadI n) (stack, store) = (n:stack, store) 

> exec' (Load v) (stack, store) = (x:stack, store)
> 	where x = either error id (get v store)

> exec' (Store v) (x:stack, store) = (stack, store')
> 	where store' = set v x store

> exec' (BinOp op)  (x:y:stack, store) = (z:stack, store)
> 	where z = apply op x y

> exec' _ state = state
  

Apply an arithmetic operator

> apply :: Op -> Val -> Val -> Val
> apply Plus (I x) (I y)  = I $ x+y 
> apply Minus (I x) (I y) = I $ y-x
> apply Times (I x) (I y) = I $ x*y
> apply Div (I x) (I y)   = I $ y `div` x
> apply Lt (I x) (I y)    = B $ x > y
> apply Gt (I x) (I y)    = B $ x < y
> apply Eq (I x) (I y)    = B $ x == y
> apply Eq (B x) (B y)    = B $ x == y
> apply And (B x) (B y)   = B $ x && y
> apply Or (B x) (B y)    = B $ x || y
> apply op a b = error $ "Runtime type mismatch at " ++ show op ++ " operator " ++ show a ++ show b
  
------------------------------------------------------------------------------------------
Typechecking

> typeCheck (Prog stmts) = checkStmts [] stmts 
  
We make a context to lookup the types of variables and also the check if they have been assigned
before use.

> type Context = [(Var, Type)]
  
LookupType searches through previously defined variables and returns their types, or throws an
error if the value has not yet been defined.

> lookupType :: Var -> Context -> Either String Type
> lookupType v = foldr (\(u,x) r -> if u==v then Right x else r) 
>                                  (Left $ "Variable " ++ show v ++ " not yet defined")

Sets the type of a value, throwing an error if that variable has already been defined as a different
type.

> setType :: Var -> Type -> Context -> Either String Context
> setType v t [] = Right [(v,t)]
> setType v t (c@(u,t'):s)
>   | v == u && t /= t' =
>     Left $ "Assigning " ++ show t ++ " to " ++ show v ++ " should be " ++ show t'
>   | v == u = Right $ (v,t):s 
>   | otherwise = (c :) <$> setType v t s

  
> getTypeVal :: Val -> Either String Type
> getTypeVal (I _) = Right Int
> getTypeVal (B _) = Right Bool
  
 
Statements dont have a type so we just check that any sub expressions have the right types
and gather the types of any variables that have been assigned in the store.

> getTypeStmt :: Context -> Stmt -> Either String Context
> getTypeStmt cntxt stmt = case stmt of
  
>     Asgn t v exp -> do
>       cntxt' <- setType v t cntxt
>       typ    <- getType cntxt' exp
>       when (t /= typ)
>         (Left $ show v ++ " should be assigned " ++
>           show t ++ " is: " ++ show typ
>         )
>       return cntxt'


When Defining a procedure we must first set its type in the store so that any recursive calls
will succeed and clear any previously defined variables from the scope, we then check that all sub
stmts are well typed and that the procedure returns values of the correct type. We must also check
that a return is reachable (i.e. not only in one path in a If statement or a while loop that may
never occur)
  
>     ProcDef name args ret stmts -> do
>       cntxt'    <- setType name (Proc args ret) cntxt
>       funcCntxt <- foldM (\c (n,t) -> setType n t c) (clearScope cntxt') args -- puts the arg types in scope
>       checkStmts funcCntxt stmts
>       types <- checkReturns funcCntxt stmts
>       unless (any isReturn stmts) (Left "No guranteed returns in procedure")
>       -- test all types are the same
>       when (any (/= ret) types) (Left "not all returns are the same type")
>       return cntxt'
  
>     _            -> return cntxt

Small helper to check return types. We consider an if statment where both branches return to be
suitable.

> isReturn (Return _) = True
> isReturn (If _ th el) = any isReturn th && any isReturn el
> isReturn _          = False

Here we collect the types of all return statements in a procedure so we can check that they are
appropriate.

> checkReturns :: Context -> [Stmt] -> Either String [Type]
> checkReturns c stmts = case stmts of
>   [] -> return []
>   (Return exp :ss)  -> do
>     typ <- getType c exp
>     (typ:) <$> checkReturns c ss
  
>   (If _ th el : ss) -> do
>     a <- checkReturns c th
>     b <- checkReturns c el
>     c <- checkReturns c ss
>     return (a ++ b ++ c)
  
>   (While _ th : ss) -> do
>     a <- checkReturns c th
>     b <- checkReturns c ss
>     return (a ++ b)

>   (Asgn t v exp : ss) -> do
>     cntxt' <- setType v t c
>     checkReturns cntxt' ss


>   (_ : ss) -> checkReturns c ss

Checks all the statements in a program. The first param is the return type for this
block so return statements can be checked. This is determined by the type of the procedure
and is set to int for the "main" section.

> checkStmts :: Context -> [Stmt] -> Either String Context
> checkStmts cntxt [] = Right cntxt


While and If statements are more complex to type check as they may introduce variables not
found in another branch or may introduce a type conflict if taken. So we must type check
the rest of the program as if both cases had occurred.
  
> checkStmts cntxt (If exp th el:ss) = do
>   typ <- getType cntxt exp
>   when (Bool /= typ) (Left "Condition for If is not boolean")
>   cntxt1 <- checkStmts cntxt th
>   cntxt2 <- checkStmts cntxt el
>   checkStmts cntxt1 ss
>   checkStmts cntxt2 ss

> checkStmts cntxt (While exp th :ss) = do
>   typ <- getType cntxt exp
>   when (Bool /= typ) (Left "Condition for If is not boolean")
>   cntxt2 <- checkStmts cntxt th
>   checkStmts cntxt  ss
>   checkStmts cntxt2 ss

> checkStmts cntxt (s:ss) = do
>   cntxt' <- getTypeStmt cntxt s
>   checkStmts cntxt' ss


> getType :: Context -> Exp -> Either String Type
> getType c (Const v)    = getTypeVal v
> getType c (Bin op a b) = getTypeOp c op a b
> getType c (Var v)      = lookupType v c 
> getType c (ProcCall args v) = do
>   a <- lookupType v c
>   case a of
>     (Proc args' ret) -> do
>       when (length args' /= length args) (Left "args are different length")
>       let argTypes = map snd args'
>       expTypes <- mapM (getType c) args
>       let typesMatch = zipWith (==) argTypes expTypes
>       unless (all (==True) typesMatch) (Left "type mismatch in arguments")
>       return $ ret
>     _            -> Left "trying to call a value as procedure"

We ensure that a operator is being passed it's appropriate types and return the resulting type
of applying the operators or an error message.

> getTypeOp cntxt op a b = do
>   typeA <- getType cntxt a
>   typeB <- getType cntxt b
>   case (op, typeA, typeB) of
>     (Plus,Int,Int)  -> Right Int 
>     (Minus,Int,Int) -> Right Int
>     (Times,Int,Int) -> Right Int
>     (Div,Int,Int)   -> Right Int
>     (And,Bool,Bool) -> Right Bool 
>     (Or,Bool,Bool)  -> Right Bool
>     (Lt,Int,Int)    -> Right Bool
>     (Gt,Int,Int)    -> Right Bool
>     (Eq,Int,Int)    -> Right Bool
>     _               -> Left $ "Bad operator types: " ++ show (op,typeA,typeB)
>        

Helper that clears all non functions from the context, helpful when checking functions with
different scope

> clearScope = filter isFunction
>   where isFunction (_, Int) = False
>         isFunction (_, Bool) = False
>         isFunction _         = True

--------------------------------------------------------------------------------
Some examples for testing

> e1 = Const $ I 0

> e2 = Var "a"

> e3 = Bin Plus e1 e1

> e4 = Bin Minus (Var "a") (Const $ I 1)
> e5 = Bin Plus (Var "b") (Const $ I 2)
> e6 = Const $ B True

> test = Bin Gt (Var "a") (Const $ I 0)

> s1 = Asgn Int "a" e3
> s1' = Asgn Int "a" (Const $ B True)

> s2 = Asgn Int  "a" e2

> s3 = If test [Asgn Int "q" e1] [Asgn Int "t" e1]

> s4 = Asgn Int "a" (Const $ I 8)
> s5 = Asgn Int "b" (Const $ I 0)
> s6 = While test [ Asgn Int "b" e5, Asgn Int "a" e4]
> f1 = ProcDef "f" [] Int [Asgn Int "a" (Const $ I (202020)), Return (Const $ I 2000)]
> p1 = Prog [s1, s2]
> p2 = Prog [s1, s3]

Here we assign "fac" to be the recursive definition of the factorial of the variable 'a' (for a = 6
should be 720)

> fac =  ProcDef "fac" [("a",Int)] Int [
>                          If (Bin Eq (Var "a") (Const $ I 0))
>                             [Return $ Const $ I 1]
>                             [ Asgn Int "b" (Var "a")
>                             , Asgn Int "a" (Bin Minus (Var "a") (Const $ I 1))
>                             , Return $ Bin Times (Var "b") (ProcCall  [(Var "a")] "fac")]
>                          ]

Version of factorial that trys to return a bool and int
 
> fac' =  ProcDef "fac" [] Int [Asgn Int "a" (Bin Minus (Const $ I 1) (Var "a"))
>                           ,If test
>                                   [Return (Bin Times (Var "a") (ProcCall [] "fac"))]
>                                   [Return $ Const $ B False]
>                           ]

A version of factorial using while loops. Uses the variable "a" as an argument

> facIter = ProcDef "fac" [("a",Int)] Int [ Asgn Int "count" (Var "a")
>                             , Asgn Int "result" (Const $ I 1)
>                             , While (Bin Gt (Var "count") (Const $ I 0))
>                                      [ Asgn Int "result" (Bin Times (Var "result") (Var "count"))
>                                      , Asgn Int "count" (Bin Minus (Var "count") (Const $ I 1))
>                                      ]
>                             , Return (Var "result")
>                             ]
>                             

A max function that returns the maximum of "a" and "b"

> maxi = ProcDef "max" [("a",Int), ("b",Int)] Int [ If (Bin Gt (Var "a") (Var "b"))
>                              [Return (Var "a")]
>                              [Return (Var "b")]
>                         ]
  
A maximum that works for three inputs requires several nested calls to max (requires max to be
defined beforehand

> max3 = ProcDef "max3" [("x",Int), ("y", Int), ("z",Int)] Int
>           [ Return (ProcCall [Var "x", ProcCall [Var "y", Var "z"] "max"] "max")
>           ]

A divide tests the order in which args a put on the stack

> divide = ProcDef "divide" [("a", Int), ("b",Int)] Int [ Return (Bin Div (Var "a") (Var "b"))]

> whileTest = Prog [s4,s5,s6] -- should return a = 0 b = 16 (decrements a and adds 2 to b until a=0)
> p3' = Prog [s1',s5,s6]
  
Runs various factorials on the input 6 should return 720 except for bad which shouldn't typecheck,
you can also try passing in different args to the ProgCall to test their result (except for
negatives which arent checked for).

> factorial = Prog [fac, Asgn Int "b" (ProcCall [Const $ I 6] "fac")]
> factorialBad = Prog [fac', Asgn Int "b" (ProcCall [] "fac")]
> factorialIter =   Prog [Asgn Int "c" (Const $ I 6), facIter, Asgn Int "b" (ProcCall [Var "c"] "fac")]
  

Finds the maximum of several values.

> maximum      = Prog [Asgn Int "a" (Const $ I 8), Asgn Int "b" (Const $ I 4), maxi
>                     , Asgn Int "c" (ProcCall [Var "a", Var "b"] "max")]
> maximumBad   = Prog [maxi, Asgn Bool "c" (ProcCall [Const $ I 1, Const $ I 10] "max")]
> maximumBad2  = Prog [maxi, Asgn Int "c" (ProcCall [Const $ B True, Const $ I 10] "max")]
> maximum3 = Prog [maxi,max3,Asgn Int "b" (ProcCall [Const $ I 1, Const $ I 30, Const $ I 3] "max3")]


> division = Prog [divide, Asgn Int "b" (ProcCall [Const $ I 10, Const $ I 2] "divide")]
> 
> noGuranteedReturns = Prog [ ProcDef "test" [] Int [If (Const $ B True) [] [Return $ Const $ I 1]]]
> guranteedReturns = Prog [ ProcDef "test" [] Int [If (Const $ B True) [] [Return $ Const $ I 1], Return $ Const $ I 1]]
  
> badProcCall  = Prog [Asgn Bool "a" (Const $ B True), Asgn Bool "b" (ProcCall [] "a")]
  
> ifTest  = Prog [ If (Const $ B True ) [Asgn Int "a" (Const $ I 1)] [Asgn Int "a" (Const $ I 2)]]
> p7 = Prog [s4,If test [Asgn Int "q" (Const $ I 1)] []]
  
  
A dumb program that is stupid (procedures are not values) but still typechecks due to non
termination.

> stupidButTypeChecks = Prog [ProcDef "dumb" [] Int [Return (ProcCall [] "dumb")],
>                             Asgn Int "a" (ProcCall [] "dumb")]

Some type checking tests isLeft means the program is poorly typed, is right means it is well typed

> t1 = isLeft $ typeCheck $ Prog [ Asgn Int "c" (Const $ I 1), Asgn Bool "c" (Const $ B True)]
> t2 = isLeft $ typeCheck $ Prog [ Asgn Int "c" (Const $ B True)]
> t3 = isRight $ typeCheck whileTest
> t4 = isLeft $ typeCheck p3'
> t5 = isRight $ typeCheck factorial
> t6 = isLeft $ typeCheck factorialBad
> t7 = isLeft $ typeCheck maximumBad
> t8 = isLeft $ typeCheck maximumBad2
> t9 = isLeft $ typeCheck noGuranteedReturns
> t10 = isRight $ typeCheck guranteedReturns
> t11 = isLeft $ typeCheck badProcCall
> t12 = isRight $ typeCheck maximum3

