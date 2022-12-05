%code top{
    #include <iostream>
    #include <stack>
    #include <assert.h>
    #include "parser.h"
    extern Ast ast;
    int yylex();
    int yyerror( char const * );
    extern int yylineno;
    Type* decl_type;
    bool current_symtab = true;
    bool all_parent_symtab = false;
    std::vector<Type*> func_fparam_type;
    StmtNode* while_stmt_node;
    std::stack<StmtNode*> while_stmt_stack;
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"
}

%union {
    int itype;
    float ftype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
}

%start Program
%token <strtype> ID ARRAYID
%token <itype> INT_NUM
%token <ftype> FLOAT_NUM
%token IF ELSE WHILE
%token CONST
%token INT FLOAT VOID
%token L_PAREN R_PAREN L_BRACE R_BRACE L_SQUARE R_SQUARE SEMI COMMA
%token ADD SUB MUL DIV MOD OR AND LESS LESSEQ GREATER GREATEREQ ASSIGN EQ NOTEQ NOT
%token RETURN BREAK CONTINUE

%nterm <stmttype> Stmts Stmt AssignStmt ExprStmt BlockStmt IfStmt WhileStmt ReturnStmt BreakStmt ContinueStmt NullStmt
%nterm <stmttype> DeclStmt VarDecl VarList VarDef ConstDecl ConstList ConstDef FuncDef FuncFParams FuncFParam

%nterm <exprtype> Exp Cond LVal PrimaryExp UnaryExp FuncRParams MulExp AddExp RelExp EqExp LAndExp LOrExp ConstExp
%nterm <exprtype> InitVal ConstInitVal
%nterm <type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts
    {
        ast.setRoot($1);
    }
    ;
Stmts
    : Stmt { $$ = $1; }
    | Stmts Stmt
    {
        $$ = new SeqNode($1, $2);
    }
    ;
Stmt
    : AssignStmt { $$ = $1; }
    | ExprStmt { $$ = $1; }
    | BlockStmt { $$ = $1; }
    | IfStmt { $$ = $1; }
    | WhileStmt { $$ = $1; }
    | BreakStmt { $$ = $1; }
    | ContinueStmt { $$ = $1; }
    | ReturnStmt { $$ = $1; }
    | DeclStmt { $$ = $1; }
    | FuncDef { $$ = $1; }
    | NullStmt { $$ = $1; }
    ;
AssignStmt
    : LVal ASSIGN Exp SEMI
    {
        $$ = new AssignStmt($1, $3);
    }
    ;
ExprStmt
    : Exp SEMI
    {
        $$ = new ExprStmt($1);
    }
    ;
BlockStmt
    : L_BRACE 
    {
        identifiers = new SymbolTable(identifiers);
    }
    Stmts R_BRACE
    {
        $$ = new CompoundStmt($3);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
    }
    ;
IfStmt
    : IF L_PAREN Cond R_PAREN Stmt %prec THEN
    {
        $$ = new IfStmt($3, $5);
    }
    | IF L_PAREN Cond R_PAREN Stmt ELSE Stmt
    {
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;
WhileStmt
    : WHILE 
    {
        while_stmt_node = new WhileStmt();
        while_stmt_stack.push(while_stmt_node);
    }
    L_PAREN Cond R_PAREN Stmt
    {
        $$ = while_stmt_stack.top();
        while_stmt_stack.pop();
        dynamic_cast<WhileStmt*>($$)->setCond($4);
        dynamic_cast<WhileStmt*>($$)->setStmt($6);
    }
BreakStmt
    : BREAK SEMI
    {
        /*CHECK: break not in while*/
        if(while_stmt_stack.empty()){
            fprintf(stderr, "[CHECKINFO][L%d]break not in while.\n", yylineno);
            $$ = new NullStmt();
        }
        else{
            $$ = new BreakStmt(while_stmt_stack.top());
        }

    }
ContinueStmt
    : CONTINUE SEMI
    {
        /*CHECK: continue not in while*/
        if(while_stmt_stack.empty()){
            fprintf(stderr, "[CHECKINFO][L%d]continue not in while.\n", yylineno);
            $$ = new NullStmt();
        }
        else{
            $$ = new ContinueStmt(while_stmt_stack.top());
        }
    }
ReturnStmt
    : RETURN Exp SEMI
    {
        $$ = new ReturnStmt($2);
    }
    | RETURN SEMI
    {
        $$ = new ReturnStmt();
    }
    ;
NullStmt
    : SEMI
    {
        $$ = new NullStmt();
    }
    | L_BRACE R_BRACE
    {
        $$ = new NullStmt();
    }
    ;
Exp
    : AddExp { $$ = $1; }
    ;
Cond
    : LOrExp { $$ = $1; }
    ;
LVal
    : ID
    {
        SymbolEntry *se;
        se = identifiers->lookup($1, all_parent_symtab);
        /*CHECK: undefined id*/
        if(se == nullptr)
        {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" is undefined.\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }
        $$ = new Id(se);
        delete []$1;
    }
    | ARRAYID
    {
        // TODO: array
        SymbolEntry *se;
        se = identifiers->lookup($1, all_parent_symtab);
        /*CHECK: undefined id*/
        if(se == nullptr)
        {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" is undefined.\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }
        $$ = new Id(se);
        delete []$1;
    }
    ;
PrimaryExp
    : LVal { $$ = $1; }
    | L_PAREN Exp R_PAREN { $$ = $2; }
    | INT_NUM
    {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    | FLOAT_NUM
    {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::floatType, $1);
        $$ = new Constant(se);
    }
    ;
UnaryExp
    : PrimaryExp { $$ = $1; }
    | ID L_PAREN FuncRParams R_PAREN
    {
        SymbolEntry *se;
        se = identifiers->lookup($1, all_parent_symtab);
        /*CHECK: undefined id*/
        if(se == nullptr)
        {
            fprintf(stderr, "[CHECKINFO][L%d]function \"%s\" is undefined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr);
        }
        $$ = new FunctionCall(se, $3);
    }
    | ADD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::ADD, $2);
    }
    | SUB UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
    }
    | NOT UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
    }
    ;
FuncRParams
    : Exp { $$ = $1; }
    | Exp COMMA FuncRParams
    {
        $1->SetSibling($3);
        $$ = $1;
    }
    | %empty { $$ = nullptr; }
MulExp
    : UnaryExp { $$ = $1; }
    | MulExp MUL UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    | MulExp DIV UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    | MulExp MOD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;
AddExp
    : MulExp {$$ = $1;}
    | AddExp ADD MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    | AddExp SUB MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;
RelExp
    : AddExp { $$ = $1; }
    | RelExp LESS AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    | RelExp GREATER AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
    }
    | RelExp LESSEQ AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQ, $1, $3);
    }                                               
    | RelExp GREATEREQ AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATEREQ, $1, $3);
    }
    ;
EqExp
    : RelExp { $$ = $1; }
    | EqExp EQ RelExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQ, $1, $3);
    }
    | EqExp NOTEQ RelExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NOTEQ, $1, $3);
    }
    ;
LAndExp
    : EqExp { $$ = $1; }
    | LAndExp AND EqExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    : LAndExp { $$ = $1; }
    | LOrExp OR LAndExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
ConstExp
    : AddExp { $$ = $1; }
    ;

Type
    : INT
    {
        $$ = TypeSystem::intType;
        decl_type = TypeSystem::intType;
    }
    | FLOAT
    {
        $$ = TypeSystem::floatType;
        decl_type = TypeSystem::floatType;
    }
    | VOID
    {
        $$ = TypeSystem::voidType;
        decl_type = TypeSystem::voidType;
    }
    ;
DeclStmt
    : VarDecl { $$ = $1; }
    | ConstDecl { $$ = $1; }
    ;
VarDecl
    : Type VarList SEMI { $$ = $2; }
    ;
VarList
    : VarDef { $$ = $1; }
    | VarDef COMMA VarList
    {
        $1->SetSibling($3);
        $$ = $1;
    }
    ;
VarDef
    : ID
    {
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id*/
        if(se != nullptr)
        {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr);
        }
        se = new IdentifierSymbolEntry(decl_type, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    | ID ASSIGN InitVal
    {
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id*/
        if(se != nullptr)
        {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr);
        }
        se = new IdentifierSymbolEntry(decl_type, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    ;
InitVal 
    : Exp { $$ = $1; }
    ;
ConstDecl
    : CONST Type ConstList SEMI { $$ = $3; }
    ;
ConstList
    : ConstDef { $$ = $1; }
    | ConstDef COMMA ConstList
    {
        $1->SetSibling($3);
        $$ = $1;
    }
    ;
ConstDef
    : ID ASSIGN ConstInitVal
    {
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id*/
        if(se != nullptr)
        {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr);
        }
        if (decl_type == TypeSystem::intType)
            decl_type = TypeSystem::constintType;
        se = new IdentifierSymbolEntry(decl_type, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    ;
ConstInitVal
    : ConstExp { $$ = $1; }
/*
DeclStmt
    : Type ID SEMI
    {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    ;
*/
/*
FuncDef
    : Type ID
    {
        Type *funcType;
        funcType = new FunctionType($1, {});
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        identifiers = new SymbolTable(identifiers);
    }
    L_PAREN R_PAREN
    BlockStmt
    {
        SymbolEntry *se;
        se = identifiers->lookup($2, all_parent_symtab);
        assert(se != nullptr);
        $$ = new FunctionDef(se, $6);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
*/
FuncDef
    : Type ID
    {
        identifiers = new SymbolTable(identifiers);
    }
    L_PAREN FuncFParams R_PAREN
    {
        Type *funcType;
        std::vector<Type*> paramstype;
        paramstype.swap(func_fparam_type);
        funcType = new FunctionType($1, paramstype);
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getPrev()->getLevel());
        identifiers->getPrev()->install($2, se);
    }
    BlockStmt
    {
        SymbolEntry *se;
        se = identifiers->lookup($2, all_parent_symtab);
        assert(se != nullptr);
        $$ = new FunctionDef(se, $5, $8);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
FuncFParams
    : FuncFParam { $$ = $1; }
    | FuncFParam COMMA FuncFParams
    {
        $1->SetSibling($3);
        $$ = $1;
    }
    | %empty { $$ = nullptr; }
    ;
FuncFParam
    : Type ID
    {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new FuncFParam(new Id(se));
        func_fparam_type.push_back($1);
        delete []$2;
    }
    ;
%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
