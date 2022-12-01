#ifndef __OPERAND_H__
#define __OPERAND_H__

#include <fstream>
#include <string>
#include <vector>
#include "SymbolTable.h"

class Instruction;
class Function;

// class Operand - The operand of an instruction.
class Operand {
    typedef std::vector<Instruction*>::iterator use_iterator;

   private:
    Instruction* def;                // The instruction where this operand is defined.
    SymbolEntry* se;                 // The symbol entry of this operand.
    std::vector<Instruction*> uses;  // Intructions that use this operand.

   public:
    Operand(SymbolEntry* se)
        : se(se) { def = nullptr; };

    use_iterator use_begin() { return uses.begin(); }
    use_iterator use_end() { return uses.end(); }

    int usersNum() const { return uses.size(); }
    Type* getType() { return se->getType(); }

    void setDef(Instruction* inst) { def = inst; }
    void addUse(Instruction* inst);
    void removeUse(Instruction* inst);

    std::string toStr() const;
};

#endif