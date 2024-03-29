%option noyywrap
%{
    #include <string>
    #include "parser.h"
    //#include <iostream>
    #define YY_NO_UNPUT
    #define YY_NO_INPUT
    #define YYSTYPE void *

    #include "Unit.h"
    extern Unit unit;
    // [TODO] sys 库函数识别
%}

Decimial            ([1-9][0-9]*|0)
Octonary            (0[oO]?[0-7]+)
Hexadecimal         (0[xX][0-9A-Fa-f]+)
Float               (([0-9]*(\.[0-9]+)|[0-9]+\.?)([eE][+-]?[0-9]+)?)
HexadecimalFloat    (0[xX](([0-9A-Fa-f]*\.[0-9A-Fa-f]*([pP][+-]?[0-9]+)?)|([0-9A-Fa-f]+[pP][+-]?[0-9]+))[fLlL]?)

Ident               ([a-zA-Z_][a-zA-Z_0-9]*)

EndLine             (\r\n|\n|\r)
BlankChar           ([ \t\v])

BlockCommentBegin   (\/\*)
BlockCommentEnd     (\*\/)
LineCommentBegin    (\/\/)
CommentBody         (\||[^\r\n|\n|\r])

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
"float"         { return FLOAT; }
"void"          { return VOID; }
"const"         { return CONST; }
"if"            { return IF; }
"else"          { return ELSE; }
"while"         { return WHILE; }
"break"         { return BREAK; }
"continue"      { return CONTINUE; }
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
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
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
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
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
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
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
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"putfloat" {
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
    paramstype.push_back(TypeSystem::floatType);
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"getfloat" {
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
    Type* funcType = new FunctionType(TypeSystem::floatType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"putarray" {
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
    ArrayType* array_type = new ArrayType(TypeSystem::intType);
    array_type->pushDim(-1);
    paramstype.push_back(array_type);
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"getarray" {
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
    ArrayType* array_type = new ArrayType(TypeSystem::intType);
    array_type->pushDim(-1);
    paramstype.push_back(array_type);
    Type* funcType = new FunctionType(TypeSystem::intType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"putfarray" {
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
    ArrayType* array_type = new ArrayType(TypeSystem::floatType);
    array_type->pushDim(-1);
    paramstype.push_back(array_type);
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"getfarray" {
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
    ArrayType* array_type = new ArrayType(TypeSystem::floatType);
    array_type->pushDim(-1);
    paramstype.push_back(array_type);
    Type* funcType = new FunctionType(TypeSystem::floatType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"starttime" {
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
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

"stoptime" {
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
    Type* funcType = new FunctionType(TypeSystem::voidType, paramstype);
    SymbolEntry *se = new IdentifierSymbolEntry(funcType, yytext, global->getLevel());
    global->install(yytext, se);
    unit.insertSysYFunc(dynamic_cast<IdentifierSymbolEntry*>(se));
    return ID;
}

{Decimial} {
    yylval.vtype.i = atoi(yytext);
    return INT_NUM;
}
{Octonary} {
    int num;
    sscanf(yytext, "%o", &num);
    yylval.vtype.i = num;
    return INT_NUM;
}
{Hexadecimal} {
    int num;
    sscanf(yytext, "%x", &num);
    yylval.vtype.i = num;
    return INT_NUM;
}
{Float} {
    float num;
    sscanf(yytext, "%f", &num);
    yylval.vtype.f = num;
    return FLOAT_NUM;
}
{HexadecimalFloat} {
    float num;
    sscanf(yytext, "%f", &num);
    yylval.vtype.f = num;
    return FLOAT_NUM;
}
{Ident}/({EndLine}|{BlankChar})*\[ {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ARRAYID;
}
{Ident} {
    char *lexeme;
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ID;
}
%%