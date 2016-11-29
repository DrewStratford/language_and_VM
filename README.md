What I have implemented:

Full procedures with arguments, recursion and type checking, while loops, if statements and boolean
and integer types.

If and while loops work as expected.

Functions are defined using the ProcDef statement which takes a list of the arguments and their
types, the return type of the procedure and the list of statements that the procedure is comprised
of. The typechecker checks that arguments are of the right type and also that we return a valid type.
It also ensures that a return statement must be reached for every control path that the procedure
could take, ensuring that the procedure returns a value (excluding non termination). This is
done by checking to see whether there is a return statement in the top level of the procedures
or there is an if statement where both branches gurantee that a return is reached.

Functions can be called using the ProcCall expression by giving the list of argument expressions
and the name of the function being called.

Functions have their own scope and cannot reach the main scope or the scope of other functions.

Function Caveats

Since functions are defined as statements in the program their definition must occur before they are
called. Unfortunately this makes mutually recursive functions impossible.

For type checking we introduce the Proc [argTypes] RetType type meaning we can write:
    Asgn "functionValue?" (Proc [] Int) ...

which gives the impression that functions are values which is not the case. There is no meaningful
way to assign a value of a Proc type that will typecheck although we can do some silly things with
non termination (see the stupidButTypeChecks program). Since the result of this is just a standard
non terminating function it is not too bad an issue, just slightly confusing.


TypeChecking
------------

Typechecking is done using the Either monad, this was chosen because we need to propogate
any errors throughout the process but some checking errors may not return meaningful results apart
from the errors. The monad ensures that the errors are propogated regardless of whether or not the
result is needed which would not be the case under normal lazy evaluation. Also things such as
when, unless and do notation are convenient.
