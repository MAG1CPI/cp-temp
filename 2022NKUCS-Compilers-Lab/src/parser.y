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
    /* while statment */
    StmtNode* while_stmt_node;
    std::stack<StmtNode*> while_stmt_stack;
    /* function in decl */
    Type* func_ret_type;
    std::vector<Type*> func_fparam_type;
    /* function in use*/
    bool no_ret = true;
}

%code requires {
    #include "Ast.h"
}

%union {
    ValueType vtype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
}

%start Program
%token <strtype> ID ARRAYID
%token <vtype> INT_NUM
%token <vtype> FLOAT_NUM
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
%nterm <exprtype> ArrayIndices ConstArrayIndices InitValList ConstInitValList
%nterm <type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts {
        ast.setRoot($1); }
    ;
Stmts
    : Stmt { $$ = $1; }
    | Stmts Stmt {
        $$ = new SeqNode($1, $2); }
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
    : LVal ASSIGN Exp SEMI {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($3->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new AssignStmt($1, $3); }
    ;
ExprStmt
    : Exp SEMI {
        $$ = new ExprStmt($1); }
    ;
BlockStmt
    : L_BRACE  {
        identifiers = new SymbolTable(identifiers); }
    Stmts R_BRACE {
        $$ = new CompoundStmt($3);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top; }
    ;
IfStmt
    : IF L_PAREN Cond R_PAREN Stmt %prec THEN {
        $$ = new IfStmt($3, $5); }
    | IF L_PAREN Cond R_PAREN Stmt ELSE Stmt {
        $$ = new IfElseStmt($3, $5, $7); }
    ;
WhileStmt
    : WHILE  {
        while_stmt_node = new WhileStmt();
        while_stmt_stack.push(while_stmt_node); }
    L_PAREN Cond R_PAREN Stmt {
        $$ = while_stmt_stack.top();
        while_stmt_stack.pop();
        dynamic_cast<WhileStmt*>($$)->setCond($4);
        dynamic_cast<WhileStmt*>($$)->setStmt($6); }
BreakStmt
    : BREAK SEMI {
        /*CHECK: break not in while - replace with NullStmt*/
        if(while_stmt_stack.empty()) {
            fprintf(stderr, "[CHECKINFO][L%d]break not in while.\n", yylineno);
            $$ = new NullStmt(); }
        else{
            $$ = new BreakStmt(while_stmt_stack.top()); } }
ContinueStmt
    : CONTINUE SEMI {
        /*CHECK: continue not in while - replace with NullStmt*/
        if(while_stmt_stack.empty()) {
            fprintf(stderr, "[CHECKINFO][L%d]continue not in while.\n", yylineno);
            $$ = new NullStmt(); }
        else{
            $$ = new ContinueStmt(while_stmt_stack.top()); } }
ReturnStmt
    : RETURN Exp SEMI {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($2->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }

        no_ret = false;
        /* CHECK: return not in function - replace with NullStmt */
        if(func_ret_type == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]return not in function!\n", yylineno);
            /*TODO: clean*/
            $$ = new NullStmt(); }
        /* CHECK: void function return varb - clean it */
        else if(func_ret_type->isVoid() == true){
            fprintf(stderr, "[CHECKINFO][L%d]void function return a variable!\n", yylineno);
            /*TODO: clean*/
            $$ = new ReturnStmt(); }
        else{
            $$ = new ReturnStmt($2, func_ret_type); } }
    | RETURN SEMI {
        no_ret = false;
        /* CHECK: return not in function - replace with NullStmt */
        if(func_ret_type == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]return not in function!\n", yylineno);
            $$ = new NullStmt(); }
        /* CHECK: non-void function return without varb - cannot fix it, quit */
        else if(func_ret_type->isVoid() == false){
            fprintf(stderr, "[CHECKINFO][L%d]function return without a variable!\n", yylineno);
            assert(func_ret_type->isVoid() == true); }
        else {
            $$ = new ReturnStmt(); } }
    ;
NullStmt
    : SEMI {
        $$ = new NullStmt(); }
    | L_BRACE R_BRACE {
        $$ = new NullStmt(); }
    ;
Exp
    : AddExp { $$ = $1; }
    ;
Cond
    : LOrExp { 
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($1->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = $1; }
    ;
LVal
    : ID {
        SymbolEntry *se;
        se = identifiers->lookup($1, all_parent_symtab);
        /* CHECK: undefined id - cannot fix it, quit */
        if(se == nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" is undefined!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr); }
        /* CHECK: use a function as a varb - cannot fix it, quit */
        else if(se->getType()->isFunc() == true) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" is a function!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se->getType()->isFunc() == false); }
        else{
            $$ = new Id(se);
            delete []$1; } }
    | ARRAYID ArrayIndices{
        /*[DONE]ARRARY*/
        SymbolEntry *se;
        se = identifiers->lookup($1, all_parent_symtab);
        /*CHECK: undefined id - cannot fix it, quit */
        if(se == nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" is undefined!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr); }
        /*CHECK: use a function as a varb - cannot fix it, quit */
        else if(se->getType()->isFunc() == true) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" is a function!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se->getType()->isFunc() == false); }
        $$ = new Id(se);

        dynamic_cast<Id*>($$)->setIndex($2);

        delete []$1; }
    ;
PrimaryExp
    : LVal { $$ = $1; }
    | L_PAREN Exp R_PAREN { 
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($2->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = $2; }
    | INT_NUM {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se); }
    | FLOAT_NUM {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::floatType, $1);
        $$ = new Constant(se); }
    ;
UnaryExp
    : PrimaryExp { $$ = $1; }
    | ID L_PAREN FuncRParams R_PAREN {
        SymbolEntry *se;
        se = identifiers->lookup($1, all_parent_symtab);
        /* CHECK: undefined function - cannot fix it, quit */
        if(se == nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]function \"%s\" is undefined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se != nullptr); }
        /* CHECK: use a varb as a function - use the corresponding variable */
        else if(se->getType()->isFunc() == false) {
            fprintf(stderr, "[CHECKINFO][L%d]\"%s\" is not a function!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            $$ = new Id(se); }
        else {
            /* CHECK: function overload TODO*/
            IdentifierSymbolEntry* func_se = dynamic_cast<IdentifierSymbolEntry*>(se);
            std::vector<Type*> *fparams_type = dynamic_cast<FunctionType*>(func_se->getType())->getParamsType();
            ExprNode *rparam;
            /* int count = 0;
            rparam = rparams_begin;
            while(rparam){
                count++;
                rparam = dynamic_cast<ExprNode*>(rparam->GetSibling());
            }
            fprintf(stderr, "[CHECKINFO][L%d]%d\n", yylineno,count); */
            while(true){
                rparam = $3;
                for(auto& fparam_type: *fparams_type){
                    fparam_type->isArray();// 解除warning
                    if(rparam == nullptr){
                        // rparam too less
                        //fprintf(stderr, "[CHECKINFO][L%d]%srparam too less!\n", yylineno,func_se->getType()->toStr().c_str());
                        goto NextFunction_UnaryExp;
                    }/* else if(!(fparam_type->isArray())&& !(rparam->getOperandType()->isArray()) && 
                            (fparam_type->toStr() != rparam->getOperandType()->toStr() &&
                            (fparam_type->toStr()!="i32" && rparam->getOperandType()->toStr()!="i1"))){
                        // rparam not match
                        //fprintf(stderr, "[CHECKINFO][L%d]%s not match %s!\n",
                        //        yylineno, 
                        //        rparam->getOperandType()->toStr().c_str(), 
                        //        fparam_type->toStr().c_str());
                        goto NextFunction_UnaryExp;
                    }*/
                    //[TODO]array type check
                    rparam = dynamic_cast<ExprNode*>(rparam->GetSibling());
                }
                if(rparam != nullptr){
                    //rparam too more
                    //fprintf(stderr, "[CHECKINFO][L%d]%srparam too more!\n", yylineno,func_se->getType()->toStr().c_str());
                    goto NextFunction_UnaryExp;
                } else{
                    break;
                }
                NextFunction_UnaryExp:
                if(func_se->isOverload()) {
                    func_se = func_se->getOverloadFunc();
                    fparams_type = dynamic_cast<FunctionType*>(func_se->getType())->getParamsType();
                    continue;
                } else{
                    fprintf(stderr, "[CHECKINFO][L%d]cannot find the function with these params!\n", yylineno);
                    assert(func_se->isOverload());
                }
            }
            $$ = new FunctionCall(func_se, $3);
        } }
    | ADD UnaryExp {
        SymbolEntry *se = NewTempSE(1,$2);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new UnaryExpr(se, UnaryExpr::ADD, $2); }
    | SUB UnaryExp {
        SymbolEntry *se = NewTempSE(1,$2);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2); }
    | NOT UnaryExp {
        SymbolEntry *se = NewTempSE(2,$2);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new UnaryExpr(se, UnaryExpr::NOT, $2); }
    ;
FuncRParams
    : Exp { 
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($1->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = $1; }
    | Exp COMMA FuncRParams {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($1->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $1->SetSibling($3);
        $$ = $1; }
    | %empty { $$ = nullptr; }//TODO
MulExp
    : UnaryExp { $$ = $1; }
    | MulExp MUL UnaryExp {
        SymbolEntry *se = NewTempSE(3,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3); }
    | MulExp DIV UnaryExp {
        SymbolEntry *se = NewTempSE(3,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3); }
    | MulExp MOD UnaryExp {
        SymbolEntry *se = NewTempSE(3,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3); }
    ;
AddExp
    : MulExp {$$ = $1;}
    | AddExp ADD MulExp {
        SymbolEntry *se = NewTempSE(3,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3); }
    | AddExp SUB MulExp {
        SymbolEntry *se = NewTempSE(3,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3); }
    ;
RelExp
    : AddExp { $$ = $1; }
    | RelExp LESS AddExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3); }
    | RelExp GREATER AddExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3); }
    | RelExp LESSEQ AddExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQ, $1, $3); }                                               
    | RelExp GREATEREQ AddExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::GREATEREQ, $1, $3); }
    ;
EqExp
    : RelExp { $$ = $1; }
    | EqExp EQ RelExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::EQ, $1, $3); }
    | EqExp NOTEQ RelExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::NOTEQ, $1, $3); }
    ;
LAndExp
    : EqExp { $$ = $1; }
    | LAndExp AND EqExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if(se == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3); }
    ;
LOrExp
    : LAndExp { $$ = $1; }
    | LOrExp OR LAndExp {
        SymbolEntry *se = NewTempSE(4,$1,$3);
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3); }
    ;
ConstExp
    : AddExp { $$ = $1; }
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
        decl_type = TypeSystem::intType; }
    | FLOAT {
        $$ = TypeSystem::floatType;
        decl_type = TypeSystem::floatType; }
    | VOID {
        $$ = TypeSystem::voidType;
        decl_type = TypeSystem::voidType; }
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
    | VarDef COMMA VarList {
        $1->SetSibling($3);
        $$ = $1; }
    ;
VarDef
    : ID {
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /* CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }
        se = new IdentifierSymbolEntry(decl_type, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1; }
    | ID ASSIGN Exp {
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($3->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }

        se = new IdentifierSymbolEntry(decl_type, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1; }
    | ARRAYID ConstArrayIndices{
        /*[DONE]ARRARY*/
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /* CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined!\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }

        Type* type = new ArrayType(decl_type);
        /*[DONE]ARRARY INDEX IN DECL*/
        ExprNode* index_node = $2;
        ExprNode* new_index_node_begin, *new_index_node;
        ValueType index_value;
        SymbolEntry *index_se;

        index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

        index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
        new_index_node_begin = new_index_node = new Constant(index_se);
        
        (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

        index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        while (index_node) {
            index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

            index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
            new_index_node->SetSibling(new Constant(index_se));

            (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

            new_index_node = dynamic_cast<ExprNode*>(new_index_node->GetSibling());
            index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        }
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        identifiers->install($1, se);

        Id* id = new Id(se);

        dynamic_cast<Id*>(id)->setIndex(new_index_node_begin);
        
        $$ = new DeclStmt(id);
        delete []$1; }
    | ARRAYID ConstArrayIndices ASSIGN L_BRACE R_BRACE{
        /*[DONE]ARRARY*/
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }

        Type* type = new ArrayType(decl_type);
        /*[DONE]ARRARY INDEX IN DECL*/
        ExprNode* index_node = $2;
        ExprNode* new_index_node_begin, *new_index_node;
        ValueType index_value;
        SymbolEntry *index_se;

        index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

        index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
        new_index_node_begin = new_index_node = new Constant(index_se);
        
        (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

        index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        while (index_node) {
            index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

            index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
            new_index_node->SetSibling(new Constant(index_se));

            (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

            new_index_node = dynamic_cast<ExprNode*>(new_index_node->GetSibling());
            index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        }
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        identifiers->install($1, se);

        Id* id = new Id(se);

        dynamic_cast<Id*>(id)->setIndex(new_index_node_begin);
        
        ExprNode* init_node = new InitValue();

        /*数组展平*/
        ExprNode* new_init_node_begin,* new_init_node_end;
        uint32_t n=0;
        bool is_float_array = dynamic_cast<ArrayType*>(type)->getElementType()->isFloat();
        dynamic_cast<InitValue*>(init_node)->flatten((dynamic_cast<ArrayType*>(type))->getDim(), 0, new_init_node_begin, new_init_node_end, n, is_float_array);

        $$ = new DeclStmt(id, new_init_node_begin);
        delete []$1; }
    | ARRAYID ConstArrayIndices ASSIGN L_BRACE InitValList R_BRACE{
        /*[DONE]ARRARY*/
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }

        Type* type = new ArrayType(decl_type);
        /*[DONE]ARRARY INDEX IN DECL*/
        ExprNode* index_node = $2;
        ExprNode* new_index_node_begin, *new_index_node;
        ValueType index_value;
        SymbolEntry *index_se;

        index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

        index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
        new_index_node_begin = new_index_node = new Constant(index_se);
        
        (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

        index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        while (index_node) {
            index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

            index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
            new_index_node->SetSibling(new Constant(index_se));

            (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

            new_index_node = dynamic_cast<ExprNode*>(new_index_node->GetSibling());
            index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        }
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        identifiers->install($1, se);

        Id* id = new Id(se);

        dynamic_cast<Id*>(id)->setIndex(new_index_node_begin);

        /*数组展平*/
        ExprNode* new_init_node_begin,* new_init_node_end;
        uint32_t n=0;
        bool is_float_array = dynamic_cast<ArrayType*>(type)->getElementType()->isFloat();
        dynamic_cast<InitValue*>($5)->flatten((dynamic_cast<ArrayType*>(type))->getDim(), 0, new_init_node_begin, new_init_node_end, n, is_float_array);

        $$ = new DeclStmt(id, new_init_node_begin);
        delete []$1; }
    ;
// only for array now
InitValList
    : InitVal COMMA InitValList{
        $1->SetSibling($3);
        $$ = $1; }
    | InitVal{
        $$ = $1; }
    ;
InitVal 
    : Exp {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($1->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = new InitValue();
        (dynamic_cast<InitValue*>($$))->setVal($1); }
    | L_BRACE InitValList R_BRACE{
        $$ = new InitValue();
        (dynamic_cast<InitValue*>($$))->setVal($2); }
    | L_BRACE R_BRACE{
        $$ = new InitValue(); }
    ;
ConstDecl
    : CONST Type ConstList SEMI { $$ = $3; }
    ;
ConstList
    : ConstDef { $$ = $1; }
    | ConstDef COMMA ConstList {
        $1->SetSibling($3);
        $$ = $1; }
    ;
ConstDef
    : ID ASSIGN ConstExp {
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($3->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }

        if (decl_type == TypeSystem::intType)
            decl_type = TypeSystem::constintType;
        else if (decl_type == TypeSystem::floatType)
            decl_type = TypeSystem::constfloatType;
        se = new IdentifierSymbolEntry(decl_type, $1, identifiers->getLevel());
        identifiers->install($1, se);
        
        /*[DONE]CONST INIT*/
        ValueType init_value;
        SymbolEntry *init_se;
        ExprNode* init_node;
        if (decl_type == TypeSystem::constintType){
            init_value.i = $3->getValue();
            init_se = new ConstantSymbolEntry(TypeSystem::intType, init_value);
        }
        else if (decl_type == TypeSystem::constfloatType){
            init_value.f = $3->getValue();
            init_se = new ConstantSymbolEntry(TypeSystem::floatType, init_value);
        }
        (dynamic_cast<IdentifierSymbolEntry*>(se))->setValue(init_value);

        init_node = new Constant(init_se);

        $$ = new DeclStmt(new Id(se), init_node);
        delete []$1; }
    | ARRAYID ConstArrayIndices ASSIGN L_BRACE R_BRACE {
        /*[DONE]ARRARY*/
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }

        if (decl_type == TypeSystem::intType)
            decl_type = TypeSystem::constintType;
        else if (decl_type == TypeSystem::floatType)
            decl_type = TypeSystem::constfloatType;
        Type* type = new ArrayType(decl_type, true);
        /*[DONE]ARRARY INDEX IN DECL*/
        ExprNode* index_node = $2;
        ExprNode* new_index_node_begin, *new_index_node;
        ValueType index_value;
        SymbolEntry *index_se;

        index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

        index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
        new_index_node_begin = new_index_node = new Constant(index_se);
        
        (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

        index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        while (index_node) {
            index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

            index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
            new_index_node->SetSibling(new Constant(index_se));

            (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

            new_index_node = dynamic_cast<ExprNode*>(new_index_node->GetSibling());
            index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        }
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        identifiers->install($1, se);

        Id* id = new Id(se);

        dynamic_cast<Id*>(id)->setIndex(new_index_node_begin);
        
        ExprNode* init_node = new InitValue();
        
        /*数组展平*/
        ExprNode* new_init_node_begin,* new_init_node_end;
        uint32_t n=0;
        bool is_float_array = dynamic_cast<ArrayType*>(type)->getElementType()->isFloat();
        dynamic_cast<InitValue*>(init_node)->flatten((dynamic_cast<ArrayType*>(type))->getDim(), 0, new_init_node_begin, new_init_node_end, n, is_float_array);

        $$ = new DeclStmt(id, new_init_node_begin);
        delete []$1; }
    | ARRAYID ConstArrayIndices ASSIGN L_BRACE ConstInitValList R_BRACE {
        /*[DONE]ARRARY*/
        SymbolEntry *se;
        se = identifiers->lookup($1, current_symtab);
        /*CHECK: duplicate defined id - cannot fix it, quit */
        if(se != nullptr) {
            fprintf(stderr, "[CHECKINFO][L%d]identifier \"%s\" duplicate defined\n", yylineno, (char*)$1);
            delete [](char*)$1;
            assert(se == nullptr); }

        if (decl_type == TypeSystem::intType)
            decl_type = TypeSystem::constintType;
        else if (decl_type == TypeSystem::floatType)
            decl_type = TypeSystem::constfloatType;
        Type* type = new ArrayType(decl_type, true);
        /*[DONE]ARRARY INDEX IN DECL*/
        ExprNode* index_node = $2;
        ExprNode* new_index_node_begin, *new_index_node;
        ValueType index_value;
        SymbolEntry *index_se;

        index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

        index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
        new_index_node_begin = new_index_node = new Constant(index_se);
        
        (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

        index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        while (index_node) {
            index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

            index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
            new_index_node->SetSibling(new Constant(index_se));

            (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

            new_index_node = dynamic_cast<ExprNode*>(new_index_node->GetSibling());
            index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        }
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        identifiers->install($1, se);

        Id* id = new Id(se);

        dynamic_cast<Id*>(id)->setIndex(new_index_node_begin);

        /*数组展平*/
        ExprNode* new_init_node_begin,* new_init_node_end;
        uint32_t n=0;
        bool is_float_array = dynamic_cast<ArrayType*>(type)->getElementType()->isFloat();
        dynamic_cast<InitValue*>($5)->flatten((dynamic_cast<ArrayType*>(type))->getDim(), 0, new_init_node_begin, new_init_node_end, n, is_float_array);

        ExprNode* iter_node = new_init_node_begin;
        ValueType init_value;
        while(iter_node){
            if (decl_type == TypeSystem::constintType){
                init_value.i = (dynamic_cast<InitValue*>(iter_node))->getVal()->getValue();
            }
            else if (decl_type == TypeSystem::constfloatType){
                init_value.f = (dynamic_cast<InitValue*>(iter_node))->getVal()->getValue();
            }
            (dynamic_cast<IdentifierSymbolEntry*>(se))->pushArrayValue(init_value);
            iter_node =  dynamic_cast<ExprNode*>(iter_node->GetSibling());
        }

        $$ = new DeclStmt(id, new_init_node_begin);
        delete []$1; }
    ;
// only for array now
ConstInitValList
    : ConstInitVal COMMA ConstInitValList{
        $1->SetSibling($3);
        $$ = $1; }
    | ConstInitVal{
        $$ = $1; }
    ;
ConstInitVal 
    : ConstExp {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($1->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        /*[DONE]CONST INIT*/
        ValueType init_value;
        SymbolEntry *init_se;
        ExprNode* init_node;
        if (decl_type == TypeSystem::constintType||decl_type == TypeSystem::intType){
            init_value.i = $1->getValue();
            init_se = new ConstantSymbolEntry(TypeSystem::constintType, init_value);
        }
        else if (decl_type == TypeSystem::constfloatType||decl_type == TypeSystem::floatType){
            init_value.f = $1->getValue();
            init_se = new ConstantSymbolEntry(TypeSystem::constfloatType, init_value);
        }
        else{
            assert(0);
        }
        init_node = new Constant(init_se);

        $$ = new InitValue();
        (dynamic_cast<InitValue*>($$))->setVal(init_node); }
    | L_BRACE ConstInitValList R_BRACE{
        $$ = new InitValue();
        (dynamic_cast<InitValue*>($$))->setVal($2); }
    | L_BRACE R_BRACE{
        $$ = new InitValue(); }
    ;
FuncDef
    : Type ID {
        identifiers = new SymbolTable(identifiers);
        func_ret_type = $1; 
        no_ret = true; }
    L_PAREN FuncFParams R_PAREN {
        /* CHECK: function overload  TODO*/
        // actural params type
        std::vector<Type*> paramstype;
        paramstype.swap(func_fparam_type);
        // already had?
        SymbolEntry *se;
        se = identifiers->lookup($2, all_parent_symtab);
        if(se){ // Yes!
            int count = 1;  // use for overload name
            // get its fparams type
            IdentifierSymbolEntry* func_se = dynamic_cast<IdentifierSymbolEntry*>(se);
            std::vector<Type*> *fparams_type = dynamic_cast<FunctionType*>(func_se->getType())->getParamsType();
            // compare
            while(true){
                if(fparams_type->size() != paramstype.size()) {
                    //param num not match
                    goto NextFunction_FuncDef_New;
                }
                /*for(uint32_t i = 0; i < fparams_type->size(); i++) {
                    if(!((*fparams_type)[i]->isArray())&& !(paramstype[i]->isArray())
                        &&(*fparams_type)[i] != paramstype[i]){
                        //param type not match
                        goto NextFunction_FuncDef_New;
                    }
                    //[TODO]array type check
                }*/
                fprintf(stderr, "[CHECKINFO][L%d]function duplicate defined!\n", yylineno);
                assert(0);
                NextFunction_FuncDef_New:
                if(func_se->isOverload()){
                    count++;
                    func_se = func_se->getOverloadFunc();
                } else {
                    break;
                }
            }
            Type *funcType = new FunctionType(func_ret_type, paramstype);
            std::string funcName = std::string($2) + "_x_ol" + std::to_string(count) + "lo_x";
            IdentifierSymbolEntry *se = new IdentifierSymbolEntry(funcType, funcName, identifiers->getPrev()->getLevel());
            func_se->setOverloadFunc(se);
        } else {
            Type *funcType = new FunctionType(func_ret_type, paramstype);
            SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getPrev()->getLevel());
            identifiers->getPrev()->install($2, se); } }
    BlockStmt {
        /* CHECK: function overload  TODO*/
        SymbolEntry *se;
        se = identifiers->lookup($2, all_parent_symtab);
        assert(se != nullptr);
        IdentifierSymbolEntry* func_se = dynamic_cast<IdentifierSymbolEntry*>(se);
        while(func_se->isOverload())
            func_se = func_se->getOverloadFunc();
        $$ = new FunctionDef(func_se, $5, $8);
        /* CHECK: no return stmt - add return stmt(return; or return 0;) */
        if(no_ret)
        {
            //fprintf(stderr, "[CHECKINFO][L%d]function %s has no return statement!\n", yylineno, se->toStr().c_str());
            if(func_ret_type == TypeSystem::voidType)
                fprintf(stderr, "[CHECKINFO][L%d]function %s has no return statement! add return;\n", yylineno, se->toStr().c_str());
            else if(func_ret_type == TypeSystem::intType)
                fprintf(stderr, "[CHECKINFO][L%d]function %s has no return statement! add return 0;\n", yylineno, se->toStr().c_str());
            else if(func_ret_type == TypeSystem::floatType)
                fprintf(stderr, "[CHECKINFO][L%d]function %s has no return statement! add return 0;\n", yylineno, se->toStr().c_str());
        }
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        func_ret_type = nullptr;
        delete top;
        delete []$2; }
    ;
FuncFParams
    : FuncFParam { $$ = $1; }
    | FuncFParam COMMA FuncFParams {
        $1->SetSibling($3);
        $$ = $1; }
    | %empty { $$ = nullptr;/*TODO f(int a,)*/ }
    ;
FuncFParam
    : Type ID {
        /* CHECK: void fparam - cannot fix it, quit */
        if($1->isVoid()){
            fprintf(stderr, "[CHECKINFO][L%d]illegal parameter type!\n", yylineno);
            assert($1->isVoid() == false); }
            
        SymbolEntry *se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new FuncFParam(new Id(se));
        func_fparam_type.push_back($1);
        delete []$2; }
    | Type ARRAYID L_SQUARE R_SQUARE ConstArrayIndices{
        /* CHECK: void fparam - cannot fix it, quit */
        if($1->isVoid()){
            fprintf(stderr, "[CHECKINFO][L%d]illegal parameter type!\n", yylineno);
            assert($1->isVoid() == false); }
        
        Type* type = new ArrayType($1);
        /*[DONE]ARRARY INDEX IN DECL*/
        (dynamic_cast<ArrayType*>(type))->pushDim(-1);
        ExprNode* index_node = $5;
        ExprNode* new_index_node_begin, *new_index_node;
        ValueType index_value;
        SymbolEntry *index_se;

        index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

        index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
        new_index_node_begin = new_index_node = new Constant(index_se);
        
        (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

        index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        while (index_node) {
            index_value.i = (dynamic_cast<ExprNode*>(index_node))->getValue();

            index_se = new ConstantSymbolEntry(TypeSystem::intType, index_value);
            new_index_node->SetSibling(new Constant(index_se));

            (dynamic_cast<ArrayType*>(type))->pushDim(index_value.i);

            new_index_node = dynamic_cast<ExprNode*>(new_index_node->GetSibling());
            index_node = dynamic_cast<ExprNode*>(index_node->GetSibling());
        }

        SymbolEntry *se = new IdentifierSymbolEntry(type, $2, identifiers->getLevel());
        identifiers->install($2, se);
        Id* id = new Id(se);

        dynamic_cast<Id*>(id)->setIndex(new_index_node_begin);
        
        $$ = new FuncFParam(id);
        func_fparam_type.push_back(type);
        }
    | Type ARRAYID L_SQUARE R_SQUARE{
        /* CHECK: void fparam - cannot fix it, quit */
        if($1->isVoid()){
            fprintf(stderr, "[CHECKINFO][L%d]illegal parameter type!\n", yylineno);
            assert($1->isVoid() == false); }
        
        Type* arrayType = new ArrayType($1);
        (dynamic_cast<ArrayType*>(arrayType))->pushDim(-1);

        SymbolEntry *se = new IdentifierSymbolEntry(arrayType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        Id* id = new Id(se);

        $$ = new FuncFParam(id);
        func_fparam_type.push_back(arrayType);
        }
    ;

// 数组下标
ArrayIndices 
    : L_SQUARE Exp R_SQUARE {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($2->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = $2;
    }
    | L_SQUARE Exp R_SQUARE ArrayIndices {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($2->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $2->SetSibling($4);
        $$ = $2;     
    }
    ;
ConstArrayIndices 
    : L_SQUARE ConstExp R_SQUARE {
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($2->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $$ = $2;
    }
    | L_SQUARE ConstExp R_SQUARE ConstArrayIndices{
        /* CHECK: use a void function as an operand - use the corresponding variable - cannot fix it, quit */
        if($2->getOperand() == nullptr){
            fprintf(stderr, "[CHECKINFO][L%d]a void function is used as an operand!\n", yylineno);
            assert(false); }
        $2->SetSibling($4);
        $$ = $2;
    }
    ;
%%

int yyerror(char const* message) {
    std::cerr<<message<<std::endl;
    return -1;
}
