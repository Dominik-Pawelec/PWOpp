(*import lexer*)

type statement =
    | ExpressionStatement of expression
    | BlockStatement of block
and binop =
    | Add
    | Sub
    | Mult
    | Div
    | Pow
    | EqualLikes (*and all the others not remembered  now*)
and expression =
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Identifier of identifier
    | Binop of {
        left : expression; 
        operator : binop;
        right : expression}
    | FunctionCall of {
        name : identifier; 
        arguments: expression list} 
    | Expression of expression
    | DefFun of {
        name: identifier;
        parameters: identifier list;
        body: block}
    | DefVar of {
        name: identifier;
        body: expression}
    | For of {
        name: identifier;
        start: int;
        endt: int;
        body: block}
    | If of {
        condition: expression;
        bodyTrue: block;
        bodyFalse: block}
    | While of {
        condition: expression;
        body: block}
    | Return of expression
and identifier = {identifier: string}
and block = {block : statement list}
and program = {statements: statement list}
;;




