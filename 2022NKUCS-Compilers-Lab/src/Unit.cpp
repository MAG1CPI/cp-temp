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

void Unit::insertSysYFunc(IdentifierSymbolEntry *func_se)
{
    bool not_insert = true;
    for (auto se : SysY_func)
    {
        if(func_se == se)
        {
            not_insert = false;
            break;
        }
    }
    if (not_insert == true)
        SysY_func.push_back(func_se);
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
    fprintf(yyout, "\n");

    for (auto &func : func_list)
        func->output();
        
    fprintf(yyout, "\n");
    for (auto func_se : SysY_func)
    {
        std::string name, return_type, fparam_type;
        FunctionType* func_type = (FunctionType*)(func_se->getType());
        name = func_se->toStr();
        return_type = func_type->getRetType()->toStr();
        fparam_type = func_type->paramTypeToStr();
        fprintf(yyout, "declare %s %s(%s)\n", return_type.c_str(), name.c_str(), fparam_type.c_str());
    }
}
