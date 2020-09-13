
(* lexer para Arith 2.0 *)

{
    open Lexing
    open Parser

    exception Lexing_error of char

    let kwd_tbl = ["set", SET; "print", PRINT; "if", IF; "then", THEN; "else", ELSE; "while", WHILE; "do", DO; "true", TRUE; "false", FALSE; "pass", PASS]
    let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

    let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{ pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

(* tokens compostos *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let equal = ['=']
let big = ['>']
let small = ['<']
let bigequal = big equal
let smallequal = small equal
let comparison = equal equal
let ident = letter (letter | digit)*
let integer = ['0'-'9']+
let space = [' ' '\t']
let others = ['(' ')' '-' '!' ';' '"' '@' '$' '%' '&' '/' '=' '?' '*' '+' '|' '_' '.' ':' ',' '<' '>']
let comment = ['#'] (letter | digit | space | ident | comparison | integer | others | ['#'])* (['\n'] | eof)

rule token = parse
	| comment           { newline lexbuf; token lexbuf }
	| space+            { token lexbuf }
	| ident as id       { id_or_kwd id }
	| '\n'              { token lexbuf }
	| '+'               { PLUS }
	| '-'               { MINUS }
	| '*'               { TIMES }
	| '/'               { DIV }
	| '='               { EQ }
	| comparison        { COMP }
	| big               { BIG }
	| small             { SMALL }
	| bigequal          { BIGEQUAL }
	| smallequal        { SMALLEQUAL }
	| '!'               { NEG }
	| '('               { LP }
	| ')'               { RP }
	| ';'               { SEQ }
	| integer as s      { INT (int_of_string s) }
	| eof               { EOF }
	| _ as c            { raise (Lexing_error c) }