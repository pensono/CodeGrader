grammar RacketMeta;

file: expression* EOF;

expression
    : LEFT_PAREN subexpr+=expression* RIGHT_PAREN # SExpression // subexpr used because children is already taken by antlr
    | LEFT_BRACKET subexpr+=expression* RIGHT_BRACKET # SExpression
    | value=(NUMBER | STRING | BOOLEAN) # Literal
    | id=IDENTIFIER # Identifier
    | '$' name=IDENTIFIER # MetaVariable
    ;

// Whitespace and comments

WS:                 [ \t\r\n\u000C]+ -> skip;
LINE_COMMENT:       ';' ~[\r\n]*    -> channel(HIDDEN);

LEFT_PAREN: '(';
RIGHT_PAREN: ')';
LEFT_BRACKET: '[';  // Split these up so we can distinguish and make sure they match when parsing
RIGHT_BRACKET: ']';

IDENTIFIER: Letter (Letter | Digit)*;  // Lump #t and #f in with identifiers
NUMBER: Digit+;
STRING : '"' ( ~'"' | '\\' '"' )* '"' ;
BOOLEAN: '#' ('t' | 'f');

fragment Digit
    : [0-9]
    ;

fragment Letter
    : [a-zA-Z]  | '?' | '!' | '-' | '<' | '+' | '>' | '=' | '*' | '/' | '_'
    ;

// TODO
// Don't worry about supporting quoted/quasiquoted expressions such as the following yet
// '(+ 3 4) or `(+ 2 ,(+ 3 4))