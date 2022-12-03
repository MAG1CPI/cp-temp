%option noyywrap
%{
    #include <string>
    #include "parser.h"

    #define YY_NO_UNPUT
    #define YY_NO_INPUT
    #define YYSTYPE void *
    /*
    "float"         { return FLOAT; }

    "break"         { return BREAK; }
    "continue"      { return CONTINUE; }
    */
%}

Decimial            ([1-9][0-9]*|0)
Octonary            (0[oO]?[0-7]+)
Hexadecimal         (0[xX][0-9A-Fa-f]+)
Float               (([0-9]*(\.[0-9]+)|[0-9]+\.?)([eE][+-]?[0-9]+)?)

Ident               ([a-zA-Z_][a-zA-Z_0-9]*)

EndLine             (\r\n|\n|\r)
BlankChar           ([ \t\v])

BlockCommentBegin   (\/\*)
BlockCommentEnd     (\*\/)
LineCommentBegin    (\/\/)
CommentBody         ([^\r\n|\n|\r])

%x BLOCKCOMMENT
%x LINECOMMENT

%%
{LineCommentBegin}              { BEGIN LINECOMMENT; }
<LINECOMMENT>{CommentBody}      { /*do nothing*/ }
<LINECOMMENT>{EndLine}          { yylineno++; BEGIN INITIAL; }

{BlockCommentBegin}             { BEGIN BLOCKCOMMENT; }
<BLOCKCOMMENT>{CommentBody}     { /*do nothing*/ }
<BLOCKCOMMENT>{EndLine}         { yylineno++; }
<BLOCKCOMMENT>{BlockCommentEnd} { BEGIN INITIAL; }

{BlankChar}     { /*do nothing*/ }
{EndLine}       { yylineno++; }
"int"           { return INT; }

"void"          { return VOID; }
"const"         { return CONST; }
"if"            { return IF; }
"else"          { return ELSE; }
"while"         { return WHILE; }


"return"        { return RETURN; }
"||"            { return OR; }
"&&"            { return AND; }
"=="            { return EQ; }
"!="            { return NOTEQ; }
"<="            { return LESSEQ; }
">="            { return GREATEREQ; }
"<"             { return LESS; }
">"             { return GREATER; }
"="             { return ASSIGN; }
"+"             { return ADD; }
"-"             { return SUB; }
"*"             { return MUL; }
"/"             { return DIV; }
"%"             { return MOD; }
"!"             { return NOT; }
";"             { return SEMI; }
","             { return COMMA; }
"("             { return L_PAREN; }
")"             { return R_PAREN; }
"["             { return L_SQUARE; }
"]"             { return R_SQUARE; }
"{"             { return L_BRACE; }
"}"             { return R_BRACE; }

"putint" {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;

    SymbolTable* global = identifiers;
    while(global->getPrev() != nullptr)
    {
        global = global->getPrev();
    }
    std::vector<Type*> paramstype;
    paramstype.push_back(TypeSystem::intType);
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    return ID;
}

"getint" {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    
    SymbolTable* global = identifiers;
    while(global->getPrev() != nullptr)
    {
        global = global->getPrev();
    }
    std::vector<Type*> paramstype;
    Type* funcType = new FunctionType(TypeSystem::intType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    return ID;
}

"putch" {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;

    SymbolTable* global = identifiers;
    while(global->getPrev() != nullptr)
    {
        global = global->getPrev();
    }
    std::vector<Type*> paramstype;
    paramstype.push_back(TypeSystem::intType);
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    return ID;
}

"getch" {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    
    SymbolTable* global = identifiers;
    while(global->getPrev() != nullptr)
    {
        global = global->getPrev();
    }
    std::vector<Type*> paramstype;
    Type* funcType = new FunctionType(TypeSystem::intType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    return ID;
}

{Decimial} {
    yylval.itype = atoi(yytext);
    return INT_NUM;
}
{Octonary} {
    int num;
    sscanf(yytext, "%o", &num);
    yylval.itype = num;
    return INT_NUM;
}
{Hexadecimal} {
    int num;
    sscanf(yytext, "%x", &num);
    yylval.itype = num;
    return INT_NUM;
}
{Float} {
    float num;
    sscanf(yytext, "%f", &num);
    yylval.ftype = num;
    //return FLOAT_NUM;
    return INT_NUM;
}
{Ident} {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ID;
}
%%