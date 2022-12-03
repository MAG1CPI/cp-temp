#include "Unit.h"
#include "Type.h"
extern FILE* yyout;

Unit::~Unit() {
    for (auto& func : func_list)
        delete func;
}

void Unit::insertFunc(Function* func) {
    func_list.push_back(func);
}

void Unit::removeFunc(Function* func) {
    func_list.erase(std::find(func_list.begin(), func_list.end(), func));
}

void Unit::insertGlobalVar(IdentifierSymbolEntry *id_se) {
    globalvar_list.push_back(id_se);
}

void Unit::output() const {
    for (auto id_se : globalvar_list)
    {
        if (id_se->getType()->isInt())
        {
            std::string name, type;
            name = id_se->toStr();
            type = id_se->getType()->toStr();
            int value = id_se->getIntValue();
            fprintf(yyout, "%s = global %s %d, align 4\n", name.c_str(), type.c_str(), value);
        }
    }
    for (auto &func : func_list)
        func->output();
}
