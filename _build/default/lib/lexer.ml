
module LexerMap = Map.Make(String)
;;

type operator =
    | PLUS            | MINUS     | MULT        | L_SLASH          | PERCENT           | AND       | POW
    | VERT_LINE       | DOUBLE_DOT| EQUAL       | LESS             | MORE              | LESS_EQUAL
    | MORE_EQUAL      | NOT_EQUAL | ALMOST_EQUAL| LESS_ALMOST_EQUAL| MORE_ALMOST_EQUAL
    | NOT_ALMOST_EQUAL| VERY_EQUAL| ASSIGN      | DOTDOTDOT        | FLOOR_LINE
;;


type delimiters = 
    | L_PAREN | R_PAREN | L_BRACKET | R_BRACKET
;;
type keyword = 
    | FOR | TO | RETURN  | IF | THEN | ELSE | WHILE
;;

type token =
    | Keyword of keyword
    | Operator of operator
    | Int of int
    | Float of float
    | String of string
    | Name of string
;;
type stateMachine =
    | Empty    | Number_Int    | Number_Float
    | String   | Literal       | Operator     | Bracket
;;


let getNext input =
    (String.get input 0, String.sub input 1 ((String.length input)-1))
;;
let tokenizer input = 
    let rec tokenizer_rec input curr_analysed state output =
        if input = "" then output (*(toToken curr_analysed state)::output*)
        else match state with
        | Empty -> output
        | _ -> output
    in tokenizer_rec input "" Empty []
;;











