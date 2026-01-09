/*
this file is part of datalog. See README for the license
*/

%{
%}

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token IF
%token NOT
%token LEQ
%token GEQ
%token NEQ
%token LT
%token GT
%token EQ
%token COMMA
%token EOI
%token <string> SINGLE_QUOTED
%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INT

%start parse_literal
%type <AST.literal> parse_literal

%start parse_literals
%type <AST.literal list> parse_literals

%start parse_clause
%type <AST.clause> parse_clause

%start parse_file
%type <AST.file> parse_file

%start parse_query
%type <AST.query> parse_query

%%

parse_file:
  | clauses EOI { $1 }

parse_literal:
  | literal EOI { $1 }

parse_literals:
  | literals EOI { $1 }

parse_clause:
  | clause EOI { $1 }

parse_query:
  | query EOI { $1 }

clauses:
  | clause { [$1] }
  | clause clauses { $1 :: $2 }

clause:
  | literal DOT { AST.Clause ($1, []) }
  | IF signed_literals DOT
     { 
        let pos_literals, neg_literals = $2 in
        let neg_literals = List.map AST.neg_hack neg_literals in
        AST.Clause (AST.Atom ("FAIL", []), pos_literals @ neg_literals) 
    }
  | literal IF signed_literals DOT 
    { 
        let pos_literals, neg_literals = $3 in
        let neg_literals = List.map AST.neg_hack neg_literals in
        AST.Clause ($1, pos_literals @ neg_literals) 
    }

literals:
  | literal { [$1] }
  | literal COMMA literals { $1 :: $3 }

comp:
    | LEQ { AST.Leq }
    | GEQ { AST.Geq }
    | NEQ { AST.Neq }
    | EQ { AST.Eq }
    | LT { AST.Lt }
    | GT { AST.Gt }

literal:
  | term comp term { AST.Comp ($1, $2, $3) }
  | LOWER_WORD { AST.Atom ($1, []) }
  | LOWER_WORD LEFT_PARENTHESIS args RIGHT_PARENTHESIS
    { AST.Atom ($1, $3) }

query:
  | LEFT_PARENTHESIS args RIGHT_PARENTHESIS IF signed_literals
    {
      let pos_literals, neg_literal = $5 in
      AST.Query ($2, pos_literals, neg_literal)
    }

signed_literals:
  | literal COMMA signed_literals
    { let pos, neg = $3 in $1 :: pos, neg }
  | NOT literal COMMA signed_literals
    { let pos, neg = $4 in pos, $2 :: neg }
  | literal { [$1], [] }
  | NOT literal { [], [$2] }

args:
  | term { [$1] }
  | term COMMA args  { $1 :: $3 }

term:
  | INT { AST.Const $1 }
  | LOWER_WORD { AST.Const $1 }
  | UPPER_WORD { AST.Var $1 }
  | SINGLE_QUOTED { AST.Quoted $1 }
