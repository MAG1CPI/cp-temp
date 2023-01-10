#ifndef __OPERAND_H__
#define __OPERAND_H__

#include <algorithm>
#include <vector>
#include "SymbolTable.h"

class Instruction;

// class Operand - The operand of an instruction.
class Operand {
   private:
    Instruction* def;                // The instruction where this operand is defined.
    std::vector<Instruction*> uses;  // Intructions that use this operand.

    SymbolEntry* se;                // The symbol entry of this operand.
    bool is_array_pointer_in_rparam;          // 在函数参数是数组指针时使用

   public:
    Operand(SymbolEntry* se)
        : se(se) { def = nullptr; is_array_pointer_in_rparam = false; }

    int usersNum() const { return uses.size(); }
    SymbolEntry* getEntry() { return se; }
    Instruction* getDef() { return def; }
    Type* getType() { return se->getType(); }

    void setDef(Instruction* inst) { def = inst; }
    void addUse(Instruction* inst) { uses.push_back(inst); }
    void removeUse(Instruction* inst);

    void setArrayPointer(bool is_array_pointer_in_rparam){ this->is_array_pointer_in_rparam = is_array_pointer_in_rparam; }
    bool isArrayPointer(){ return is_array_pointer_in_rparam; }

    std::string toStr() const;

   private:
    typedef std::vector<Instruction*>::iterator use_iterator;

   public:
    use_iterator use_begin() { return uses.begin(); }
    use_iterator use_end() { return uses.end(); }
};

#endif