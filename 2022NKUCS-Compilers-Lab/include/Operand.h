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
    
    SymbolEntry* se;                 // The symbol entry of this operand.

   public:
    Operand(SymbolEntry* se)
        : se(se) { def = nullptr; }

    int usersNum() const { return uses.size(); }
    Type* getType() { return se->getType(); }

    void setDef(Instruction* inst) { def = inst; }
    void addUse(Instruction* inst) { uses.push_back(inst); }
    void removeUse(Instruction* inst);

    std::string toStr() const;

   private:
    typedef std::vector<Instruction*>::iterator use_iterator;

   public:
    use_iterator use_begin() { return uses.begin(); }
    use_iterator use_end() { return uses.end(); }
};

#endif