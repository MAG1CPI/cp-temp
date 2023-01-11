#ifndef __MACHINECODE_H__
#define __MACHINECODE_H__
#include <algorithm>
#include <fstream>
#include <set>
#include <queue>
#include <string>
#include <vector>
#include "SymbolTable.h"
#include <iostream>
/* Hint:
 * MachineUnit: Compiler unit
 * MachineFunction: Function in assembly code
 * MachineInstruction: Single assembly instruction
 * MachineOperand: Operand in assembly instruction, such as immediate number, register, address label */

/* Todo:
 * We only give the example code of "class BinaryMInstruction" and "class AccessMInstruction" (because we believe in you !!!),
 * You need to complete other the member function, especially "output()" ,
 * After that, you can use "output()" to print assembly code . */

class MachineUnit;
class MachineFunction;
class MachineBlock;
class MachineInstruction;

class MachineOperand {
   private:
    MachineInstruction* parent;
    int type;
    int val;            // value of immediate number
    int reg_no;         // register no
    std::string label;  // address label
    bool is_func;

    //float
    bool is_float = false;
    float float_val;

    bool stack_param = false;

   public:
    enum { IMM,
           VREG,
           REG,
           LABEL };
    MachineOperand(int tp, int val);
    MachineOperand(int tp, float float_val, bool is_float = true);
    MachineOperand(std::string label, bool is_func = false);
    bool operator==(const MachineOperand&) const;
    bool operator<(const MachineOperand&) const;
    bool isImm() { return this->type == IMM; };
    bool isReg() { return this->type == REG; };
    bool isVReg() { return this->type == VREG; };
    bool isLabel() { return this->type == LABEL; };

    int getVal() { return this->val; };
    void setVal(int val) { this->val = val; };

    int getReg() { return this->reg_no; };
    void setReg(int regno) {
        this->type = REG;
        this->reg_no = regno;
    };

    std::string getLabel() { return this->label; };
    void setParent(MachineInstruction* p) { this->parent = p; };
    MachineInstruction* getParent() { return this->parent; };
    void PrintReg();

    void setStackParam() { stack_param = true; }
    bool isStackParam() { return stack_param; }

    //float
    bool isFloat() { return this->is_float; }
    float getFloatVal() { return this->float_val; }

    void output();
};

class MachineInstruction {
   protected:
    MachineBlock* parent;
    int no;
    int type;                             // Instruction type
    int cond = MachineInstruction::NONE;  // Instruction execution condition, optional !!
    int op;                               // Instruction opcode
    // Instruction operand list, sorted by appearance order in assembly instruction
    std::vector<MachineOperand*> def_list;
    std::vector<MachineOperand*> use_list;
    void addDef(MachineOperand* ope) { def_list.push_back(ope); };
    void addUse(MachineOperand* ope) { use_list.push_back(ope); };
    // Print execution code after printing opcode
    void PrintCond();
    enum instType { BINARY,
                    LOAD,
                    STORE,
                    MOV,
                    BRANCH,
                    CMP,
                    STACK,
                    VCVT,
                    VMRS };

   public:
    enum condType { EQ,
                    NE,
                    LT,
                    LE,
                    GT,
                    GE,
                    NONE };
    virtual void output() = 0;
    void setNo(int no) { this->no = no; };
    int getNo() { return no; };
    std::vector<MachineOperand*>& getDef() { return def_list; };
    std::vector<MachineOperand*>& getUse() { return use_list; };
    
    bool isBinary() const { return this->type == BINARY; };
    bool isLoad() const { return this->type == LOAD; };
    bool isStore() const { return this->type == STORE; };
    bool isMov() const { return this->type == MOV; };
    bool isBranch() const { return this->type == BRANCH; };
    bool isCmp() const { return this->type == CMP; };
    bool isStack() const { return this->type == STACK; };

    MachineBlock* getParent() { return parent; }
};

class BinaryMInstruction : public MachineInstruction {
   public:
    enum opType { ADD,
                  SUB,
                  MUL,
                  DIV,
                  AND,
                  OR,
                  VADD,
                  VSUB,
                  VMUL,
                  VDIV };
    BinaryMInstruction(MachineBlock* p, int op, MachineOperand* dst, MachineOperand* src1, MachineOperand* src2, int cond = MachineInstruction::NONE);
    void output();
};

class LoadMInstruction : public MachineInstruction {
   public:
    enum opType { LDR,
                  VLDR };
    //'op' is the penult param
    LoadMInstruction(MachineBlock* p,
                     MachineOperand* dst,
                     MachineOperand* src1,
                     MachineOperand* src2 = nullptr,
                     int op = LoadMInstruction::LDR,
                     int cond = MachineInstruction::NONE);
    void output();
};

class StoreMInstruction : public MachineInstruction {
   public:
    enum opType{ STR,
                 VSTR };
    //'op' is the penult param
    StoreMInstruction(MachineBlock* p,
                      MachineOperand* src1,
                      MachineOperand* src2,
                      MachineOperand* src3 = nullptr,
                      int op = StoreMInstruction::STR,
                      int cond = MachineInstruction::NONE);
    void output();
};

class MovMInstruction : public MachineInstruction {
   public:
    enum opType { MOV,
                  MVN, 
                  VMOV };
    MovMInstruction(MachineBlock* p, int op, MachineOperand* dst, MachineOperand* src, int cond = MachineInstruction::NONE);
    void output();
};

class BranchMInstruction : public MachineInstruction {
   public:
    enum opType { B,
                  BL,
                  BX };
    BranchMInstruction(MachineBlock* p, int op, MachineOperand* dst, int cond = MachineInstruction::NONE);
    void output();
};

class CmpMInstruction : public MachineInstruction {
   public:
    enum opType { CMP,
                  VCMP };
    //note that 'op' is the last param, different from load and store
    CmpMInstruction(MachineBlock* p,
                    MachineOperand* src1,
                    MachineOperand* src2,
                    int cond = MachineInstruction::NONE,
                    int op = CmpMInstruction::CMP);
    void output();
};

class StackMInstruction : public MachineInstruction {
   public:
    enum opType { PUSH,
                  POP,
                  VPUSH,
                  VPOP };
    StackMInstruction(MachineBlock* p, int op, MachineOperand* src, int cond = MachineInstruction::NONE);
    StackMInstruction(MachineBlock* p, int op, std::vector<MachineOperand*>& src, int cond = MachineInstruction::NONE);
    void output();
};

class VcvtMInstruction : public MachineInstruction {
   public:
    enum opType { F2I,
                  I2F };
    VcvtMInstruction(MachineBlock* p, int op, MachineOperand* dst, MachineOperand* src, int cond = MachineInstruction::NONE);
    void output();
};

class VmrsMInstruction : public MachineInstruction {
   public:
    VmrsMInstruction(MachineBlock* p);
    void output();
};

class MachineBlock {
   private:
    MachineFunction* parent;
    int no;
    std::vector<MachineBlock*> pred, succ;
    std::vector<MachineInstruction*> inst_list;
    std::set<MachineOperand*> live_in;
    std::set<MachineOperand*> live_out;

    int cmp_cond;
    static int label_no;

   public:
    std::vector<MachineInstruction*>& getInsts() { return inst_list; };
    std::vector<MachineInstruction*>::iterator begin() { return inst_list.begin(); };
    std::vector<MachineInstruction*>::iterator end() { return inst_list.end(); };
    std::vector<MachineInstruction*>::reverse_iterator rbegin() { return inst_list.rbegin(); };
    std::vector<MachineInstruction*>::reverse_iterator rend() { return inst_list.rend(); };
    MachineBlock(MachineFunction* p, int no) {
        this->parent = p;
        this->no = no;
    };
    void InsertInst(MachineInstruction* inst) { this->inst_list.push_back(inst); };
    void addPred(MachineBlock* p) { this->pred.push_back(p); };
    void addSucc(MachineBlock* s) { this->succ.push_back(s); };
    std::set<MachineOperand*>& getLiveIn() { return live_in; };
    std::set<MachineOperand*>& getLiveOut() { return live_out; };
    std::vector<MachineBlock*>& getPreds() { return pred; };
    std::vector<MachineBlock*>& getSuccs() { return succ; };

    void setCmpCond(int cond) { cmp_cond = cond; }
    int getCmpCond() const { return cmp_cond; }
    MachineFunction* getParent() { return this->parent; }
    int getInstNum() { return inst_list.size(); }

    void insertBefore(MachineInstruction* before, MachineInstruction* cur);
    void insertAfter(MachineInstruction* after, MachineInstruction* cur);

    void output();
};

class MachineFunction {
   private:
    MachineUnit* parent;
    std::vector<MachineBlock*> block_list;
    int stack_size;
    std::set<int> saved_regs;
    SymbolEntry* sym_ptr;

   public:
    std::vector<MachineBlock*>& getBlocks() { return block_list; };
    std::vector<MachineBlock*>::iterator begin() { return block_list.begin(); };
    std::vector<MachineBlock*>::iterator end() { return block_list.end(); };
    MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr);
    /* HINT:
     * Alloc stack space for local variable;
     * return current frame offset ;
     * we store offset in symbol entry of this variable in function AllocInstruction::genMachineCode()
     * you can use this function in LinearScan::genSpillCode() */
    int AllocSpace(int size) {
        this->stack_size += size;
        return this->stack_size;
    };
    void InsertBlock(MachineBlock* block) { this->block_list.push_back(block); };

    void addSavedRegs(int regno) { saved_regs.insert(regno); }
    std::vector<MachineOperand*> getSavedRegs();
    MachineUnit* getParent() { return parent; }
    int getInstNum();

    void output();
};

class MachineUnit {
   private:
    std::vector<MachineFunction*> func_list;
    std::vector<IdentifierSymbolEntry*> globalvar_list;
    void PrintGlobalDecl();
    int label_no = 0;

   public:
    std::vector<MachineFunction*>& getFuncs() { return func_list; }
    std::vector<MachineFunction*>::iterator begin() { return func_list.begin(); }
    std::vector<MachineFunction*>::iterator end() { return func_list.end(); }

    void insertGlobalVar(IdentifierSymbolEntry* sym_ptr) { globalvar_list.push_back(sym_ptr); }
    void InsertFunc(MachineFunction* func) { func_list.push_back(func); }

    void PrintGlobalLabel();
    int getLabelNo() { return label_no; }

    void output();
};

#endif