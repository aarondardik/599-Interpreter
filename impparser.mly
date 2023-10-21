%token RPAREN
%token LPAREN
%token PLUS
%token MINUS

%token <string> VAR
%token <bool> BOOL

%token LT 
%token LEQ
%token EQ
%token AND
%token OR
%token NOT
%token OUTPUT
%token SKIP
%token SEQ
%token IF 
%token THEN
%token ELSE
%token WHILE 
%token DO
%token DONE


%token <int> INT
%token EOF

%start <Imp.cmd> prog

%%

prog:
  | e = cmd; EOF { e }
  ;


cmd:
  | OUTPUT; e = expr1 { Imp.Output(e) }
  | SKIP; {Imp.Skip}
  | e1 = cmd; SEQ; e2 = cmd { Imp.Seq(e1, e2) } 
  | e1 = VAR; EQ; e2 = expr1 { Imp.Asgn(e1, e2) }
  | IF; e1 = expr2; THEN; e2 = cmd; ELSE; e3 = cmd {Imp.IfElse(e1, e2, e3)}
  | WHILE; e1 = expr2; DO; e2 = cmd; DONE { Imp.While(e1, e2) }
  | LPAREN; e = cmd; RPAREN; { e }
  ;

expr1:
  | e = expr01 { e } 
  | el = expr01; PLUS; er = expr01 { Imp.Plus(el, er) }
  | el = expr01; MINUS; er = expr01 { Imp.Minus(el, er) }
  ;

expr2:
  | e = expr02 {e}
  | e1 = expr01; LT; er = expr01 {Imp.Lt (e1, er)}
  | e1 = expr01; LEQ; er = expr01 {Imp.Leq (e1, er)}
  | e1 = expr01; EQ; er = expr01 {Imp.Eq (e1, er)}  
  | e1 = expr02; AND; er = expr02 {Imp.And (e1, er)}
  | e1 = expr02; OR; er = expr02 {Imp.Or (e1, er)}
  | e = expr02; NOT; {Imp.Not (e)}


expr01:
  | s = INT { Imp.Int s }
  | s = VAR { Imp.Var s}
  | LPAREN; e = expr1; RPAREN { e }
  ;


expr02:
  | s = BOOL { Imp.Bool s}
  | LPAREN; e = expr2; RPAREN { e }
  ;



