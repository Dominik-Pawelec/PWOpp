(*import lexer*)



type primitives =
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Identifier of string
;;

type binop =
    | Add
    | Sub
    | Mult
    | Div
    | Pow
    | EqualLikes (*and all the others not remembered  now*)

type expression =
    | Primitive of primitives
    | Binop of binop * expression * expression
    | FunctionCall of Identifier  * list of expression
;;

type block =
    | Expression of expression
    | DefFun of Identifier * list of Identifier * blocks
    | DefVar of Identifier * expression
    | For of Identifier * Int * Int * blocks
    | If of expression * blocks * blocks
    | While of expression * blocks
    | Return of expression

and blocks = list of block
;;

