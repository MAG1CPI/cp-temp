#ifndef __UNIT_H__
#define __UNIT_H__

#include <fstream>
#include <vector>
#include "Function.h"
#include "SymbolTable.h"

class Unit {
   private:
    std::vector<Function*> func_list;
    std::vector<IdentifierSymbolEntry*> globalvar_list;

   public:
    Unit() = default;
    ~Unit();

    void insertFunc(Function*);
    void removeFunc(Function*);
    void insertGlobalVar(IdentifierSymbolEntry*);
    void output() const;

   private:
    typedef std::vector<Function*>::iterator iterator;
    typedef std::vector<Function*>::reverse_iterator r_iterator;

   public:
    iterator begin() { return func_list.begin(); }
    iterator end() { return func_list.end(); }
    r_iterator rbegin() { return func_list.rbegin(); }
    r_iterator rend() { return func_list.rend(); }
};

#endif