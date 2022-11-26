#include "SymbolTable.h"

std::string ConstantSymbolEntry::toStr() {
    return std::to_string(value);
}

std::string IdentifierSymbolEntry::toStr() {
    return "@" + name;
}

std::string TemporarySymbolEntry::toStr() {
    return "%t" + std::to_string(label);
}

SymbolTable::SymbolTable() {
    prev = nullptr;
    level = 0;
}

SymbolTable::SymbolTable(SymbolTable* prev) {
    assert(prev);
    this->prev = prev;
    this->level = prev->level + 1;
}

SymbolEntry* SymbolTable::lookup(std::string name, bool current) {
    SymbolTable* scope = this;
    if (scope == nullptr)
        return nullptr;
    switch (current) {
        case true:
            if (scope->symbolTable.count(name) )
                return scope->symbolTable[name];
            return nullptr;
        case false:
            while (scope != nullptr) {
                if (scope->symbolTable.count(name))
                    return scope->symbolTable[name];
                scope = scope->prev;
            }
            return nullptr;
        default:
            return nullptr;
    }
}

void SymbolTable::install(std::string name, SymbolEntry* entry) {
    symbolTable[name] = entry;
    counter++;
}

int SymbolTable::counter = 0;

static SymbolTable t;

SymbolTable* identifiers = &t;
SymbolTable* globals = &t;
