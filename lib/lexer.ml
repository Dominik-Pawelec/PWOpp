exception Lexer_Error of string
;;

let (^^) str chr = str^(String.make 1 chr)  
;;


module LexerMap = Map.Make(String)
;;

type operator =
    | PLUS            | MINUS     | MULT        | L_SLASH          | PERCENT           | AND       | POW
    | VERT_LINE       | DOUBLE_DOT| EQUAL       | LESS             | MORE              | LESS_EQUAL
    | MORE_EQUAL      | NOT_EQUAL | ALMOST_EQUAL| LESS_ALMOST_EQUAL| MORE_ALMOST_EQUAL
    | NOT_ALMOST_EQUAL| VERY_EQUAL| ASSIGN      | DOTDOTDOT        | FLOOR_LINE
    | L_PAREN         | R_PAREN   | L_BRACKET   | R_BRACKET        | L_BRACE |R_BRACE
    | INVALID_OPERATOR
;;
let operator_map = LexerMap.of_list [
    ("+", PLUS); ("-", MINUS); ("*", MULT); ("/", L_SLASH); ("%", PERCENT); ("&", AND); ("^", POW);
    ("|", VERT_LINE); (":", DOUBLE_DOT); ("=", EQUAL); ("<", LESS); (">", MORE); ("<=", LESS_EQUAL);
    (">=", MORE_EQUAL); ("!=", NOT_EQUAL); ("=~", ALMOST_EQUAL); ("<=~", LESS_ALMOST_EQUAL); (">=~", MORE_ALMOST_EQUAL);
    ("!=~", NOT_ALMOST_EQUAL); ("==", VERY_EQUAL); (":=", ASSIGN); ("...", DOTDOTDOT); ("_", FLOOR_LINE);
    ("(", L_PAREN); (")", R_PAREN); ("[", L_BRACKET); ("]", R_BRACKET); ("{",L_BRACE); ("}",R_BRACE)
]
;;
type keyword = 
    | FOR | TO | RETURN  | IF | THEN | ELSE | WHILE
;;
let keyword_map = LexerMap.of_list [
    ("FOR", FOR); ("TO", TO); ("RETURN", RETURN); ("IF", IF); ("THEN", THEN); ("ELSE", ELSE); ("WHILE", WHILE)
]
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
    | String   | Literal       | Operator
;;


let getNext input =
    (String.get input 0, String.sub input 1 ((String.length input)-1))
;;
let operatorDivisor text =
    let toTokenOperator text : token =
        match LexerMap.find_opt text operator_map with
            | Some(x) -> Operator(x)
            | None -> Operator(INVALID_OPERATOR)
        in
    let rec scanner_rec text curr_analysed output is_repeating =
        if text = "" then (toTokenOperator curr_analysed)::output
        else let next_char = fst (getNext text) and text' = snd (getNext text) in 
        let new_analysed = curr_analysed ^^ next_char in print_endline new_analysed ;
        let f_temp x =  LexerMap.is_empty (LexerMap.filter 
            (fun s _ -> if String.length s < String.length x then false
            else String.sub s 0 (String.length x) = x)
            operator_map)
        in match f_temp new_analysed with
        | false  ->  scanner_rec text' new_analysed output false
        | true ->if is_repeating then scanner_rec text' "" output false
                else scanner_rec text "" ((toTokenOperator curr_analysed)::output) true
    in scanner_rec text "" [] false
;;
let toToken text (state : stateMachine) =
    match state with
    | Number_Int -> [Int(int_of_string text)]
    | Number_Float -> [Float(float_of_string text)]
    | String -> [String(text)]
    | Literal ->(match LexerMap.find_opt text keyword_map with
                | Some(x) -> [Keyword(x)]
                | None -> [Name(text)]
                )
    | Operator -> operatorDivisor text

    | _ -> [] 
;;
let tokenizer input = 
    let rec tokenizer_rec input curr_analysed state output =
        if input = "" && curr_analysed != "" && curr_analysed != " " 
        && curr_analysed != "\n" && curr_analysed != "\t" 
            then (toToken curr_analysed state)@output
        else if input = ""  then output (*TODO: Correct sanitisation of empty text*) 
        else match state with
        | Empty -> let next_char = fst (getNext input) and input' = snd (getNext input) in
                ( match next_char with
                | 'a'..'z'|'A'..'Z' -> tokenizer_rec input' (curr_analysed ^^ next_char) Literal output
                | '"' -> tokenizer_rec input' curr_analysed String output
                | '0'..'9' -> tokenizer_rec input' (curr_analysed ^^ next_char) Number_Int output
                | ' '|'\n'|'\t' -> tokenizer_rec input' "" Empty output
                | _ -> tokenizer_rec input' (curr_analysed ^^ next_char) Operator output  
                )
        | Literal -> let next_char = fst (getNext input) and input' = snd (getNext input) in
                ( match next_char with
                | 'a'..'z'|'A'..'Z' -> tokenizer_rec input' (curr_analysed ^^ next_char) state output
                | _ -> tokenizer_rec input "" Empty ((toToken curr_analysed state)@output)
                )
        | String -> let next_char = fst (getNext input) and input' = snd (getNext input) in
                ( match next_char with
                | '"' -> tokenizer_rec input' "" Empty ((toToken curr_analysed state)@output)
                | _ -> tokenizer_rec input' (curr_analysed ^^ next_char) state output
                )
        | Operator -> let next_char = fst (getNext input) and input' = snd (getNext input) in
                ( match next_char with 
                | 'a'..'z'|'A'..'Z'|'"'|'0'..'9'|' '|'\n'|'\t' -> 
                        tokenizer_rec input "" Empty ((toToken curr_analysed state)@output)
                | _ -> tokenizer_rec input' (curr_analysed ^^ next_char) state output
                )
        | Number_Int -> let next_char = fst (getNext input) and input' = snd (getNext input) in
                ( match next_char with
                | '0'..'9' -> tokenizer_rec input' (curr_analysed ^^ next_char) state output
                | '.' -> (match fst (getNext input') with(*make more secure, currently 1. makes very bad things*)
                        | '0'..'9' -> tokenizer_rec input' (curr_analysed ^^ next_char) Number_Float output
                        | _ -> tokenizer_rec input "" Empty ((toToken curr_analysed Number_Int)@output)
                        )
                | _ -> tokenizer_rec input "" Empty ((toToken curr_analysed Number_Int)@output)
                )
        | Number_Float -> let next_char = fst (getNext input) and input' = snd (getNext input) in
            (match next_char with
            | '0'..'9' -> tokenizer_rec input' (curr_analysed ^^ next_char) state output
            | _ -> tokenizer_rec input "" Empty ((toToken curr_analysed Number_Float)@output)
            )
    in List.rev (tokenizer_rec input "" Empty [])
;;
