#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include "Operand.h"

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node {
   private:
    static int counter;
    int seq;
    Node* sibling;

   protected:
    std::vector<Instruction*> true_list;
    std::vector<Instruction*> false_list;
    static IRBuilder* builder;
    void backPatch(std::vector<Instruction*>& list, BasicBlock* bb, bool liststyle);
    std::vector<Instruction*> merge(std::vector<Instruction*>& list1, std::vector<Instruction*>& list2);
    void swapList(std::vector<Instruction*>& truelist, std::vector<Instruction*>& falselist);

   public:
    Node();
    int getSeq() const { return seq; };
    static void setIRBuilder(IRBuilder* ib) { builder = ib; };
    virtual void output(int level) = 0;
    void SetSibling(Node* sibling_node) { sibling = sibling_node; };
    Node* GetSibling() { return sibling; };
    bool HaveSibling() { return (sibling != nullptr) ? true : false; };
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;
    std::vector<Instruction*>& trueList() { return true_list; }
    std::vector<Instruction*>& falseList() { return false_list; }

    void insertCondBrInst(Function* func, Node* cond, BasicBlock* insert_bb);
};

class ExprNode : public Node {
   protected:
    SymbolEntry* symbolEntry;
    Operand* dst;  // The result of the subtree is stored into dst.
    bool is_init_list;
    bool is_cond_op;

   public:
    ExprNode(SymbolEntry* symbolEntry = nullptr)
        : symbolEntry(symbolEntry), is_init_list(false), is_cond_op(false){};
    Operand* getOperand() { return dst; };
    Type* getOperandType() { return dst->getType(); };
    SymbolEntry* getSymPtr() { return symbolEntry; };

    bool isCondOp() { return is_cond_op; }
    bool isInitList() { return is_init_list; }
    virtual double getValue() { return 0.0; };

    void int2bool(BasicBlock* insert_bb);
    void bool2int(BasicBlock* insert_bb);

    void float2int(BasicBlock* insert_bb);
    void int2float(BasicBlock* insert_bb);
};

class UnaryExpr : public ExprNode {
   private:
    int op;
    ExprNode* expr;

   public:
    enum { ADD,
           SUB,
           NOT };
    UnaryExpr(SymbolEntry* se, int op, ExprNode* expr)
        : ExprNode(se), op(op), expr(expr) {
        dst = new Operand(se);
        if (op == NOT)
            is_cond_op = true;
    };
    double getValue();
    void output(int level);
    void typeCheck();
    void genCode();
};

class BinaryExpr : public ExprNode {
   private:
    int op;
    ExprNode *expr1, *expr2;

   public:
    enum { ADD,
           SUB,
           MUL,
           DIV,
           MOD,
           AND,
           OR,
           EQ,
           NOTEQ,
           LESS,
           LESSEQ,
           GREATER,
           GREATEREQ };

    BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2)
        : ExprNode(se), op(op), expr1(expr1), expr2(expr2) {
        dst = new Operand(se);
        if (op >= AND)
            is_cond_op = true;
    };

    double getValue();
    void output(int level);
    void typeCheck();
    void genCode();
};

class Constant : public ExprNode {
   public:
    Constant(SymbolEntry* se)
        : ExprNode(se) { dst = new Operand(se); };
    double getValue();
    void output(int level);
    void typeCheck();
    void genCode();
};

class Id : public ExprNode {
    ExprNode* index;

   public:
    Id(SymbolEntry* se)
        : ExprNode(se) {
        SymbolEntry* temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel());
        dst = new Operand(temp);
    };

    void setIndex(ExprNode* index) { this->index = index; }
    ExprNode* getIndex() { return index; }

    double getValue();
    void output(int level);
    void typeCheck();
    void genCode();
};

class InitValue : public ExprNode {
   private:
    ExprNode* val;
    Type* type;
    Operand* base_addr;
    int offset = 0;

   public:
    InitValue() {
        val = nullptr;
        is_init_list = true;
    }

    void setVal(ExprNode* val) { this->val = val; }
    ExprNode* getVal() { return val; }

    void setType(Type* type) { this->type = type; }
    Type* getType() { return type; }

    void setBaseAddr(Operand* base_addr) { this->base_addr = base_addr; }
    Operand* getBaseAddr() { return base_addr; }

    void setOffset(int offset) { this->offset = offset; }
    int getOffset() { return offset; }

    void flatten(std::vector<int> dims, uint32_t level, ExprNode*& begin, ExprNode*& end, uint32_t& n, bool is_float = false);

    void output(int level);
    void typeCheck();
    void genCode();
};

class StmtNode : public Node {};

class CompoundStmt : public StmtNode {
   private:
    StmtNode* stmt;

   public:
    CompoundStmt(StmtNode* stmt)
        : stmt(stmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode {
   private:
    StmtNode *stmt1, *stmt2;

   public:
    SeqNode(StmtNode* stmt1, StmtNode* stmt2)
        : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class DeclStmt : public StmtNode {
   private:
    Id* id;
    ExprNode* initval;

   public:
    DeclStmt(Id* id, ExprNode* initval)
        : id(id), initval(initval){};
    DeclStmt(Id* id)
        : id(id), initval(nullptr){};
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
        : cond(cond), thenStmt(thenStmt){};
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
        : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ReturnStmt : public StmtNode {
   private:
    ExprNode* retValue;
    Type* retType;

   public:
    ReturnStmt(ExprNode* retValue, Type* retType = nullptr)
        : retValue(retValue), retType(retType){};
    ReturnStmt()
        : retValue(nullptr), retType(nullptr){};
    //void setRetType(Type* ret_type) { retType = ret_type; }
    //Type* getRetType() { return retType; }
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
        : lval(lval), expr(expr){};
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
    FunctionDef(SymbolEntry* se, StmtNode* fparam, StmtNode* stmt)
        : se(se), fparam(fparam), stmt(stmt){};
    FunctionDef(SymbolEntry* se, StmtNode* stmt)
        : se(se), fparam(nullptr), stmt(stmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class FuncFParam : public StmtNode {
   private:
    Id* id;

   public:
    FuncFParam(Id* id)
        : id(id){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ExprStmt : public StmtNode {
   private:
    ExprNode* expr;

   public:
    ExprStmt(ExprNode* expr)
        : expr(expr){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class WhileStmt : public StmtNode {
   private:
    ExprNode* cond;
    StmtNode* stmt;
    BasicBlock* cond_bb;
    BasicBlock* end_bb;

   public:
    WhileStmt(ExprNode* cond = nullptr, StmtNode* stmt = nullptr)
        : cond(cond), stmt(stmt) {}

    BasicBlock* getCondBB() { return cond_bb; }
    BasicBlock* getEndBB() { return end_bb; }

    void setCond(ExprNode* cond) { this->cond = cond; }
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

class FunctionCall : public ExprNode {
   private:
    ExprNode* rparam;

   public:
    FunctionCall(SymbolEntry* se, ExprNode* rparam)
        : ExprNode(se), rparam(rparam) {
        Type* return_type = dynamic_cast<FunctionType*>(se->getType())->getRetType();
        if (return_type->isVoid()) {
            dst = nullptr;
        } else {
            SymbolEntry* temp = new TemporarySymbolEntry(return_type, SymbolTable::getLabel());
            dst = new Operand(temp);
        }
    };
    FunctionCall(SymbolEntry* se)
        : ExprNode(se), rparam(nullptr){};
    ExprNode* getRparam() const { return rparam; }
    void output(int level);
    void typeCheck();
    void genCode();
};

class NullStmt : public StmtNode {
   public:
    NullStmt(){};
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

TemporarySymbolEntry* NewTempSE(int type, ExprNode* expr1, ExprNode* expr2 = nullptr);
#endif
