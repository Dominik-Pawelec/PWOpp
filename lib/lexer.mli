type operator =
  | PLUS
  | MINUS
  | MULT
  | L_SLASH
  | PERCENT
  | AND
  | POW
  | VERT_LINE
  | DOUBLE_DOT
  | EQUAL
  | LESS
  | MORE
  | LESS_EQUAL
  | MORE_EQUAL
  | NOT_EQUAL
  | ALMOST_EQUAL
  | LESS_ALMOST_EQUAL
  | MORE_ALMOST_EQUAL
  | NOT_ALMOST_EQUAL
  | VERY_EQUAL
  | ASSIGN
  | DOTDOTDOT
  | FLOOR_LINE
  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_BRACE
  | R_BRACE
  | INVALID_OPERATOR
type keyword = FOR | TO | RETURN | IF | THEN | ELSE | WHILE | TRUE | FALSE
type token =
    Keyword of keyword
  | Operator of operator
  | Int of int
  | Float of float
  | String of string
  | Name of string
val tokenizer : string -> token list
