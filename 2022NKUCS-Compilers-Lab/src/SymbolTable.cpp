#include "SymbolTable.h"

SymbolEntry::SymbolEntry(Type* type, int kind) {
    this->type = type;
    this->kind = kind;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type, int value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    this->value = value;
}

std::string ConstantSymbolEntry::toStr() {
    return std::to_string(value);
}

IdentifierSymbolEntry::IdentifierSymbolEntry(Type* type, std::string name, int scope)
    : SymbolEntry(type, SymbolEntry::VARIABLE), name(name) {
    this->scope = scope;
    addr = nullptr;
    overload = false;
    next_overload = nullptr;
}

void IdentifierSymbolEntry::setOverloadFunc(IdentifierSymbolEntry* func_se) {
    next_overload = func_se;
    overload = func_se;
}

std::string IdentifierSymbolEntry::toStr() {
    return "@" + name;
}

TemporarySymbolEntry::TemporarySymbolEntry(Type* type, int label)
    : SymbolEntry(type, SymbolEntry::TEMPORARY) {
    this->label = label;
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
    if (current) {
        if (scope->symbolTable.count(name))
            return scope->symbolTable[name];
    } else {
        while (scope != nullptr) {
            if (scope->symbolTable.count(name))
                return scope->symbolTable[name];
            scope = scope->prev;
        }
    }
    return nullptr;
}

void SymbolTable::install(std::string name, SymbolEntry* entry) {
    symbolTable[name] = entry;
    counter++;
}

int SymbolTable::counter = 0;

static SymbolTable t;

SymbolTable* identifiers = &t;
SymbolTable* globals = &t;
