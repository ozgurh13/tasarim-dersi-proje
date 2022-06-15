
# tasarım dersi proje

a dynamically typed and interpreted programming language  
example code can be found [here](/examples)



* datatypes

the language comes with a few builtin types and does not support user defined ones  

| name     | description                         |
| ---      | ---                                 |
| int      | unbounded whole number              |
| double   | double precision floating number    |
| bool     | boolean values *true* and *false*   |
| string   | sequence of characters              |
| list     | an ordered heterogeneous collection | 
| function | a subroutine                        |

*note: lists are very primitive*  
*they can be indexed (val = list[index]) but do not support indexed assignment (list[index] = val)*  
*they also don't support nested indexing (list[index1][index2]), nor do they support direct indexing ([item1, item2, ...][index])*



* variable declaration / assignment

variables can be declared using the `var` or `const` keywords
```
var a = 10;
const e = 2.71828;
```
the difference is variables declared with `var` can be reassigned while the ones declared with `const` cannot and are subject to propagation  
furthermore, once a variable is declared with `const`, it can't be redeclared as `var` or as a function in the same scope

only variables declared with `var` can be reassigned


* conditionals (if-else / ternary / switch / match)

`if/else` is similar to the C language with the form
```
if condition1
    statement1
else if condition2
    statement2
else
    statement3
```
single statements in the body don't require braces and `else if` is not a single construct  
the difference is that the condition does not have to be surrounded by parenthesis


the ternary operator is defined as follows
```
? condition : true_branch : false_branch
```
here the `?` comes first, followed by the condition, true branch and false branch


`switch` is syntactic sugar for multiple if-else blocks
```
switch
  | condition1
  : statement1

  | condition2
  : statement2
  
  | condition3
  : statement3
  
  |: statement4
```
will desugar into
```
if condition1
    statement1
else if condition2
    statement2
else if condition3
    statement3
else
    statement4
```
if the condition is omitted, it is treated like an else and any conditions after it will be optimized away


`match` is syntactic sugar for multiple equality checks against a single variable
```
match variable
  | var1
  : statement1
  
  | var2
  : statement2
  
  | var3
  : statement3
  
  |: statement4  
```
will desugar into
```
if variable == var1
    statement1
else if variable == var2
    statement2
else if variable == var3
    statement3
else
    statement4
```
similar to switch, an empty case will be treated as the else case and anything following it will get optimized away



* loops (for-do / while / do-while / until / do-until / loop)

loops support `break` but do not support `continue`  

`for-do` loops are of the form
```
for initialize : condition : step
 do statement
```
*initialize* will run once before the loop, *condition* is checked at the end of each iteration, *step* is executed after each iteration  
after *step* the `do` keyword is required


`while` loops are of the form
```
while condition
    statement
```
the condition does not need to be surrounded by parenthesis


`do-while` loops are of the form
```
do 
    statement
while condition
```
the condition does not need to be surrounded by parenthesis neither does it need a semicolon at the end


`until` and `do-until` loops are exactly the same as their while counterparts except they run while the condition is false

`loop` is syntactic sugar for `while true`



* functions / lambdas

functions are first class citizens, they can be passed to and be returned from other functions  
functions are by default curried, so applying any number of arguments less than the number of parameters will return a new function  

they are defined like
```
def name param1 param2 ...
{
    statment
}
```
definition starts with the `def` keyword followed by the name of the function  
then follow the names of the parameters separated by spaces  
finally the body of the function surrounded by braces

builtin functions are
| name   | description       | return name(param_type, ...)          |
| ---    | ---               | ---                                   |
| print  | print to stdout   | null print(any, ...)                  |
| input  | read from stdin   | string read() / string read(string)   |
| length | length of item    | int length(list) / int length(string) |
| cons   | cons an element   | list cons(any, list)                  |
| uncons | uncons an element | list uncons(list)                     |
| assert | assert true       | null assert(bool, ...)                |
| abort  | abort program     | null abort()                          |
| read   | read a file       | string read(string)                   |
| time   | current cpu time  | int time()                            |


lambda functions are defined the way they are in Haskell, using the `\ ->` syntax
```
\param1 param2 ... -> expr
```
lambdas are not statements so they either need to be assigned to a name or passed to a function  
the body of the lambda must be an expression and cannot be a statement



* try-except

user defined exceptions are not supported, so this will only catch internal exceptions (like DivisionByZero)  
the key difference from many popular languages is `return` will not be treated like an exception and the function will return  
(many languages choose to execute the finally block when returning from try)



* comments

comments are C-style comments (`//` and `/* */`) and cannot nest within each other
