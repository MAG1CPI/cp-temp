#ifndef __FUNCTION_H__
#define __FUNCTION_H__

#include <fstream>
#include <set>
#include <vector>
#include "AsmBuilder.h"
#include "BasicBlock.h"

class Unit;

class Function {
   private:
    Unit* parent;
    SymbolEntry* sym_ptr;
    BasicBlock* entry;
    std::vector<BasicBlock*> block_list;
    std::vector<TemporarySymbolEntry*> fparams_symtab;

   public:
    Function(Unit*, SymbolEntry*);
    ~Function();

    BasicBlock* getEntry() { return entry; }
    SymbolEntry* getSymPtr() { return sym_ptr; }
    std::vector<BasicBlock*>& getBlockList() { return block_list; }
    bool inBlockList(BasicBlock* bb);

    void insertBlock(BasicBlock* bb);
    void remove(BasicBlock* bb);
    void insertFParamSE(TemporarySymbolEntry* se);

    void output() const;

    void DFS(AsmBuilder*, BasicBlock*, std::map<BasicBlock*, MachineBlock*>&, std::set<BasicBlock*>&);
    void genMachineCode(AsmBuilder*);

   private:
    typedef std::vector<BasicBlock*>::iterator iterator;
    typedef std::vector<BasicBlock*>::reverse_iterator r_iterator;

   public:
    iterator begin() { return block_list.begin(); }
    iterator end() { return block_list.end(); }
    r_iterator rbegin() { return block_list.rbegin(); }
    r_iterator rend() { return block_list.rend(); }
};

#endif
