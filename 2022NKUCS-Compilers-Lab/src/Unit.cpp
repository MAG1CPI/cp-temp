#include "Unit.h"

void Unit::insertFunc(Function* f) {
    func_list.push_back(f);
}

void Unit::removeFunc(Function* f) {
    func_list.erase(std::find(func_list.begin(), func_list.end(), f));
}

void Unit::output() const {
    for (auto& func : func_list)
        func->output();
}

Unit::~Unit() {
    for (auto& func : func_list)
        delete func;
}
