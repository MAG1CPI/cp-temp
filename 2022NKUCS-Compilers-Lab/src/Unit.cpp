#include "Unit.h"

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

void Unit::output() const {
    for (auto& func : func_list)
        func->output();
}
