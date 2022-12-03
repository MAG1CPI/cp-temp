#ifndef __INSTRUCTION_H__
#define __INSTRUCTION_H__

#include <fstream>
#include <map>
#include <vector>
#include "Operand.h"

class BasicBlock;

class Instruction {
   protected:
    unsigned instType;
    unsigned opcode;

    BasicBlock* parent;
    Instruction* prev;
    Instruction* next;
    std::vector<Operand*> operands;

    enum { BINARY,
           COND,
           UNCOND,
           RET,
           LOAD,
           STORE,
           CMP,
           ALLOCA };

   public:
    Instruction(unsigned instType, BasicBlock* insert_bb = nullptr);
    virtual ~Instruction();

    BasicBlock* getParent() { return parent; }
    Instruction* getNext() { return next; }
    Instruction* getPrev() { return prev; }
    bool isUncond() const { return instType == UNCOND; }
    bool isCond() const { return instType == COND; }

    void setParent(BasicBlock* bb) { parent = bb; }
    void setNext(Instruction* inst) { next = inst; }
    void setPrev(Instruction* inst) { prev = inst; }
    virtual void output() const = 0;
};

// meaningless instruction, used as the head node of the instruction list.
class DummyInstruction : public Instruction {
   public:
    DummyInstruction()
        : Instruction(-1, nullptr){};

    void output() const {};
};

class AllocaInstruction : public Instruction {
   private:
    SymbolEntry* se;

   public:
    AllocaInstruction(Operand* dst, SymbolEntry* se, BasicBlock* insert_bb = nullptr);
    ~AllocaInstruction();

    void output() const;
};

class LoadInstruction : public Instruction {
   public:
    LoadInstruction(Operand* dst, Operand* src_addr, BasicBlock* insert_bb = nullptr);
    ~LoadInstruction();

    void output() const;
};

class StoreInstruction : public Instruction {
   public:
    StoreInstruction(Operand* dst_addr, Operand* src, BasicBlock* insert_bb = nullptr);
    ~StoreInstruction();

    void output() const;
};

class BinaryInstruction : public Instruction {
   public:
    enum { ADD,
           SUB,
           MUL,
           DIV,
           MOD,
           AND,
           OR };

   public:
    BinaryInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb = nullptr);
    ~BinaryInstruction();

    void output() const;
};

class CmpInstruction : public Instruction {
   public:
    enum { EQ,
           NOTEQ,
           LESS,
           LESSEQ,
           GREATER,
           GREATEREQ };
   public:
    CmpInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb = nullptr);
    ~CmpInstruction();

    void output() const;
};

// unconditional branch
class UncondBrInstruction : public Instruction {
   protected:
    BasicBlock* branch;

   public:
    UncondBrInstruction(BasicBlock*, BasicBlock* insert_bb = nullptr);

    BasicBlock* getBranch() { return branch; }
    void setBranch(BasicBlock* bb) { branch = bb; }

    void output() const;
};

// conditional branch
class CondBrInstruction : public Instruction {
   protected:
    BasicBlock* true_branch;
    BasicBlock* false_branch;

   public:
    CondBrInstruction(BasicBlock*, BasicBlock*, Operand*, BasicBlock* insert_bb = nullptr);
    ~CondBrInstruction();

    BasicBlock* getTrueBranch() { return true_branch; }
    BasicBlock* getFalseBranch() { return false_branch; }
    void setTrueBranch(BasicBlock* bb) { true_branch = bb; }
    void setFalseBranch(BasicBlock* bb) { false_branch = bb; }

    void output() const;
};

class RetInstruction : public Instruction {
   public:
    RetInstruction(Operand* src, BasicBlock* insert_bb = nullptr);
    ~RetInstruction();

    void output() const;
};

#endif