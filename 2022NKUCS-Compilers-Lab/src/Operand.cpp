#include "Operand.h"

std::string Operand::toStr() const {
    return se->toStr();
}

void Operand::addUse(Instruction* inst) {
    uses.push_back(inst);
}

void Operand::removeUse(Instruction* inst) {
    auto i = std::find(uses.begin(), uses.end(), inst);
    if (i != uses.end())
        uses.erase(i);
}
