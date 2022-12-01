#ifndef __FUNCTION_H__
#define __FUNCTION_H__

#include <algorithm>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <vector>
#include "BasicBlock.h"
#include "SymbolTable.h"
#include "Unit.h"

class Unit;

class Function {
    typedef std::vector<BasicBlock*>::iterator iterator;
    typedef std::vector<BasicBlock*>::reverse_iterator r_iterator;

   private:
    Unit* parent;
    SymbolEntry* sym_ptr;
    BasicBlock* entry;
    std::vector<BasicBlock*> block_list;

   public:
    Function(Unit*, SymbolEntry*);
    ~Function();

    iterator begin() { return block_list.begin(); };
    iterator end() { return block_list.end(); };
    r_iterator rbegin() { return block_list.rbegin(); };
    r_iterator rend() { return block_list.rend(); };

    BasicBlock* getEntry() { return entry; };
    std::vector<BasicBlock*>& getBlockList() { return block_list; };
    SymbolEntry* getSymPtr() { return sym_ptr; };

    void insertBlock(BasicBlock* bb) { block_list.push_back(bb); };
    void remove(BasicBlock* bb);
    void output() const;
};

#endif
