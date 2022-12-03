#include "Operand.h"

void Operand::removeUse(Instruction* inst) {
    auto i = std::find(uses.begin(), uses.end(), inst);
    if (i != uses.end())
        uses.erase(i);
}

std::string Operand::toStr() const {
    return se->toStr();
}
