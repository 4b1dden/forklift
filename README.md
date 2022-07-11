## forklift

my language. not sure what this will yet be, but I always wanted to build a dummy lang from scratch. so far i have no dependencies. 

things to fix {bad}: 
- [] singular fn calls cause problems to the parser (wish rust had TCO) 
- [] empty strings (" ") break the parser? lol

todo:
- [x] lexer (ended up not using this)
- [in progress] parser
    - [x] print stmt
    - [x] scoped blocks
    - [x] if statements
    - [x] else statements
    - [x] add Strings
    - [] logical operators
    - [x] add while loop
    - [] rethink how i handle expr / declaration / reassignment
    - [x] add for loop 
        - [] make fields in for loop optional
    - [] add floats
    - [] add functions
- [] interpreter
    - [x] print stmt
    - [x] scoped blocks
    - [x] if statements
    - [x] else statements
    - [x] strings
    - [] logical operators
    - [x] while loop
    - [x] for loop 
    - [] break statement in for loop
    - [] floats
    - [] functions
- [] better testing
    - [] simple programs e2e test stdout 
    - [] add out stream type for interpreter 
- [] types? 
- [] ... ?

