#ifndef __INSTRUCTION_H__
#define __INSTRUCTION_H__

#include <fstream>
#include <map>
#include <vector>
#include "Operand.h"
#include "AsmBuilder.h"
#include <iostream>

class BasicBlock;

class Instruction {
   protected:
    BasicBlock* parent;
    Instruction* prev;
    Instruction* next;
    std::vector<Operand*> operands;

    unsigned instType;
    unsigned opcode;

    enum { BINARY,
           COND,
           UNCOND,
           CALL,
           RET,
           LOAD,
           STORE,
           CMP,
           ALLOCA,
           UNSIGNEDEXT,
           NEG,
           FLOAT2INT,
           INT2FLOAT };

   public:
    Instruction(unsigned instType, BasicBlock* insert_bb = nullptr);
    virtual ~Instruction();

    bool isUncond() const { return instType == UNCOND; }
    bool isCond() const { return instType == COND; }
    bool isAlloc() const { return instType == ALLOCA; }
    bool isRet() const { return instType == RET; }
    BasicBlock* getParent() { return parent; }
    Instruction* getNext() { return next; }
    Instruction* getPrev() { return prev; }
    void setParent(BasicBlock* bb) { parent = bb; }
    void setNext(Instruction* inst) { next = inst; }
    void setPrev(Instruction* inst) { prev = inst; }

    MachineOperand* genMachineOperand(Operand*);
    MachineOperand* genFloatMachineOperand(Operand*);
    MachineOperand* genMachineReg(int reg);
    MachineOperand* genMachineVReg();
    MachineOperand* genFloatMachineVReg();
    MachineOperand* genMachineImm(int val);
    MachineOperand* genMachineLabel(int block_no);

    virtual void output() const = 0;
    virtual void genMachineCode(AsmBuilder*) = 0;
};

// meaningless instruction, used as the head node of the instruction list.
class DummyInstruction : public Instruction {
   public:
    DummyInstruction()
        : Instruction(-1, nullptr){};

    void output() const {};
    void genMachineCode(AsmBuilder*) {}
};

class AllocaInstruction : public Instruction {
   private:
    SymbolEntry* se;

   public:
    AllocaInstruction(Operand* dst, SymbolEntry* se, BasicBlock* insert_bb = nullptr);
    ~AllocaInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class LoadInstruction : public Instruction {
   public:
    LoadInstruction(Operand* dst, Operand* src_addr, BasicBlock* insert_bb = nullptr);
    ~LoadInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class StoreInstruction : public Instruction {
   public:
    StoreInstruction(Operand* dst_addr, Operand* src, BasicBlock* insert_bb = nullptr);
    ~StoreInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
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
    bool is_array_pointer;

   public:
    BinaryInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb = nullptr, bool is_array_pointer = false);
    ~BinaryInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
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
    void genMachineCode(AsmBuilder*);
};

// unconditional branch
class UncondBrInstruction : public Instruction {
   protected:
    BasicBlock* branch;

   public:
    UncondBrInstruction(BasicBlock*, BasicBlock* insert_bb = nullptr);
    ~UncondBrInstruction() {}

    BasicBlock* getBranch() { return branch; }
    void setBranch(BasicBlock* bb) { branch = bb; }

    void output() const;
    void genMachineCode(AsmBuilder*);
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
    void genMachineCode(AsmBuilder*);
};

class CallInstruction : public Instruction {
   protected:
    SymbolEntry* func;

   public:
    CallInstruction(SymbolEntry* func, std::vector<Operand*> rparams, Operand* dst, BasicBlock* insert_bb = nullptr);
    ~CallInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class RetInstruction : public Instruction {
   public:
    RetInstruction(Operand* src, BasicBlock* insert_bb = nullptr);
    ~RetInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class UnSignedExtInstruction : public Instruction {
   public:
    UnSignedExtInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb = nullptr);
    ~UnSignedExtInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class NEGInstruction : public Instruction {
   public:
    NEGInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb = nullptr);
    ~NEGInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class Float2IntInstruction : public Instruction {
   public:
    Float2IntInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb = nullptr);
    ~Float2IntInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

class Int2FloatInstruction : public Instruction {
   public:
    Int2FloatInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb = nullptr);
    ~Int2FloatInstruction();

    void output() const;
    void genMachineCode(AsmBuilder*);
};

#endif