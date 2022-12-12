#include "Unit.h"
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

void Unit::insertGlobalVar(IdentifierSymbolEntry* id_se) {
    globalvar_list.push_back(id_se);
}

void Unit::insertSysYFunc(IdentifierSymbolEntry* func_se) {
    bool not_insert = true;
    for (auto se : SysY_func) {
        if (func_se->toStr() == se->toStr()) {
            not_insert = false;
            break;
        }
    }
    if (not_insert == true)
        SysY_func.push_back(func_se);
}

void Unit::output() const {
    // 全局变量
    for (auto id_se : globalvar_list) {
        if (id_se->getType()->isInt()) {
            std::string name, type;
            name = id_se->toStr();
            type = id_se->getType()->toStr();
            int value = id_se->getValue().i;
            fprintf(yyout, "%s = global %s %d, align 4\n", name.c_str(), type.c_str(), value);
        }
    }
    fprintf(yyout, "\n");

    // 一般函数
    for (auto& func : func_list)
        func->output();
    fprintf(yyout, "\n");

    // SysY运行时库函数
    FunctionType* func_type;
    std::string name, return_type, fparam_type;
    std::vector<Type*>* paramsType;
    for (auto func_se : SysY_func) {
        func_type = (FunctionType*)(func_se->getType());
        paramsType = func_type->getParamsType();

        name = func_se->toStr();

        return_type = func_type->getRetType()->toStr();

        if (paramsType->size())
            fparam_type = (*paramsType)[0]->toStr();
        for (uint32_t i = 1; i < paramsType->size(); i++)
            fparam_type += ", " + (*paramsType)[i]->toStr();

        fprintf(yyout, "declare %s %s(%s)\n", return_type.c_str(), name.c_str(), fparam_type.c_str());
    }
}
