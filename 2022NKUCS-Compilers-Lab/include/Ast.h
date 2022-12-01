#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include <string>
#include <vector>
#include "IRBuilder.h"
#include "Unit.h"

class IRBuilder;

class Node {
   private:
    static int counter;
    int seq;
    Node* next;

   protected:
    static IRBuilder* builder;
    std::vector<Instruction*> true_list;
    std::vector<Instruction*> false_list;

   protected:
    void backPatch(std::vector<Instruction*>&, BasicBlock*);
    std::vector<Instruction*> merge(std::vector<Instruction*>&, std::vector<Instruction*>&);

   public:
    static void setIRBuilder(IRBuilder* ib) { builder = ib; }

    Node()
        : seq(counter++) {}

    int getSeq() const { return seq; }
    Node* getNext() const { return next; }
    std::vector<Instruction*>& trueList() { return true_list; }
    std::vector<Instruction*>& falseList() { return false_list; }

    void setNext(Node*);
    virtual void output(int level) = 0;
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;
};

class ExprNode : public Node {
   protected:
    SymbolEntry* symbolEntry;
    Operand* dst;  // The result of the subtree is stored into dst.
   public:
    ExprNode(SymbolEntry* symbolEntry)
        : symbolEntry(symbolEntry) {}

    Operand* getOperand() { return dst; }
    SymbolEntry* getSymPtr() { return symbolEntry; }
};

class UnaryExpr : public ExprNode {
   private:
    int op;
    ExprNode* expr;

   public:
    enum {
        ADD,
        SUB,
        NOT
    };
    UnaryExpr(SymbolEntry* se, int op, ExprNode* expr)
        : ExprNode(se), op(op), expr(expr) { dst = new Operand(se); }

    void output(int level);
    void typeCheck();
    void genCode();
};

class BinaryExpr : public ExprNode {
   private:
    int op;
    ExprNode *expr1, *expr2;

   public:
    enum {
        ADD,
        SUB,
        MUL,
        DIV,
        MOD,
        OR,
        AND,
        LESS,
        LESSEQ,
        GREATER,
        GREATEREQ,
        EQ,
        NOTEQ
    };
    BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2)
        : ExprNode(se), op(op), expr1(expr1), expr2(expr2) { dst = new Operand(se); }

    void output(int level);
    void typeCheck();
    void genCode();
};

class Constant : public ExprNode {
   public:
    Constant(SymbolEntry* se)
        : ExprNode(se) { dst = new Operand(se); }

    void output(int level);
    void typeCheck();
    void genCode();
};

class Id : public ExprNode {
   public:
    Id(SymbolEntry* se)
        : ExprNode(se) {
        SymbolEntry* temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel());
        dst = new Operand(temp);
    }

    void output(int level);
    void typeCheck();
    void genCode();
};

class FunctionCall : public ExprNode {
   private:
    ExprNode* rparam;

   public:
    FunctionCall(SymbolEntry* se, ExprNode* rparam = nullptr)
        : ExprNode(se), rparam(rparam) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class StmtNode : public Node {
};

class CompoundStmt : public StmtNode {
   private:
    StmtNode* stmt;

   public:
    CompoundStmt(StmtNode* stmt)
        : stmt(stmt) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode {
   private:
    StmtNode *stmt1, *stmt2;
    // StmtNode *head_stmt;

   public:
    SeqNode(StmtNode* stmt1, StmtNode* stmt2)
        : stmt1(stmt1), stmt2(stmt2) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class DeclStmt : public StmtNode {
   private:
    Id* id;
    ExprNode* initval;

   public:
    DeclStmt(Id* id, ExprNode* initval = nullptr)
        : id(id), initval(initval){};

    void output(int level);
    void typeCheck();
    void genCode();
};

class IfStmt : public StmtNode {
   private:
    ExprNode* cond;
    StmtNode* thenStmt;

   public:
    IfStmt(ExprNode* cond, StmtNode* thenStmt)
        : cond(cond), thenStmt(thenStmt) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class IfElseStmt : public StmtNode {
   private:
    ExprNode* cond;
    StmtNode* thenStmt;
    StmtNode* elseStmt;

   public:
    IfElseStmt(ExprNode* cond, StmtNode* thenStmt, StmtNode* elseStmt)
        : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class WhileStmt : public StmtNode {
   private:
    ExprNode* cond;
    StmtNode* stmt;

   public:
    WhileStmt(ExprNode* cond, StmtNode* stmt = nullptr)
        : cond(cond), stmt(stmt) {}

    void setStmt(StmtNode* stmt) { this->stmt = stmt; }
    void output(int level);
    void typeCheck();
    void genCode();
};

class BreakStmt : public StmtNode {
   private:
    StmtNode* whileStmt;

   public:
    BreakStmt(StmtNode* whileStmt)
        : whileStmt(whileStmt) {}
    void output(int level);
    void typeCheck();
    void genCode();
};

class ContinueStmt : public StmtNode {
   private:
    StmtNode* whileStmt;

   public:
    ContinueStmt(StmtNode* whileStmt)
        : whileStmt(whileStmt) {}
    void output(int level);
    void typeCheck();
    void genCode();
};

class ReturnStmt : public StmtNode {
   private:
    ExprNode* retValue;

   public:
    ReturnStmt(ExprNode* retValue = nullptr)
        : retValue(retValue) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class AssignStmt : public StmtNode {
   private:
    ExprNode* lval;
    ExprNode* expr;

   public:
    AssignStmt(ExprNode* lval, ExprNode* expr)
        : lval(lval), expr(expr) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class ExprStmt : public StmtNode {
   private:
    ExprNode* expr;

   public:
    ExprStmt(ExprNode* expr)
        : expr(expr) {}
    void output(int level);
    void typeCheck();
    void genCode();
};

class FunctionDef : public StmtNode {
   private:
    SymbolEntry* se;
    StmtNode* fparam;
    StmtNode* stmt;

   public:
    FunctionDef(SymbolEntry* se, StmtNode* stmt, StmtNode* fparam = nullptr)
        : se(se), stmt(stmt), fparam(fparam) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class FuncFParam : public StmtNode {
   private:
    Id* id;

   public:
    FuncFParam(Id* id)
        : id(id) {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class NullStmt : public StmtNode {
   public:
    NullStmt() {}

    void output(int level);
    void typeCheck();
    void genCode();
};

class Ast {
   private:
    Node* root;

   public:
    Ast() { root = nullptr; }
    void setRoot(Node* n) { root = n; }

    void output();
    void typeCheck();
    void genCode(Unit* unit);
};

#endif
