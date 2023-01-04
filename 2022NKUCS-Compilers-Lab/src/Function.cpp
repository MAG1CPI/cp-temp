#include "Function.h"
#include "Unit.h"

extern FILE* yyout;

Function::Function(Unit* u, SymbolEntry* s) {
    entry = new BasicBlock(this);
    sym_ptr = s;
    parent = u;

    u->insertFunc(this);
}

Function::~Function() {
    // why?
    /*
    auto delete_list = block_list;
    for (auto& i : delete_list)
        delete i;
    parent->removeFunc(this);
    */
}

bool Function::inBlockList(BasicBlock* bb) {
    if (std::find(block_list.begin(), block_list.end(), bb) != block_list.end())
        return true;
    else
        return false;
}

// insert the basicblock bb from its block_list.
void Function::insertBlock(BasicBlock* bb) {
    block_list.push_back(bb);
}

// remove the basicblock bb from its block_list.
void Function::remove(BasicBlock* bb) {
    block_list.erase(std::find(block_list.begin(), block_list.end(), bb));
}

void Function::insertFParamSE(TemporarySymbolEntry* se) {
    fparams_symtab.push_back(se);
}

void Function::output() const {
    FunctionType* funcType = dynamic_cast<FunctionType*>(sym_ptr->getType());
    Type* retType = funcType->getRetType();
    if (fparams_symtab.empty())
        fprintf(yyout, "define %s %s() {\n", retType->toStr().c_str(), sym_ptr->toStr().c_str());
    else {
        fprintf(yyout, "define %s %s(", retType->toStr().c_str(), sym_ptr->toStr().c_str());
        for (auto se : fparams_symtab) {
            std::string type, se_string;
            type = se->getType()->toStr();
            se_string = se->toStr();
            if (se != *(fparams_symtab.begin()))
                fprintf(yyout, ", %s %s", type.c_str(), se_string.c_str());
            else
                fprintf(yyout, "%s %s", type.c_str(), se_string.c_str());
        }
        fprintf(yyout, ") {\n");
    }
    std::set<BasicBlock*> visited;
    std::vector<BasicBlock*> waiting;
    visited.insert(entry);
    waiting.push_back(entry);
    while (!waiting.empty()) {
        auto bb = waiting.front();
        waiting.erase(waiting.begin());
        bb->output();

        for (auto succ = bb->succ_begin(); succ != bb->succ_end(); succ++) {
            if (visited.find(*succ) == visited.end()) {
                visited.insert(*succ);
                waiting.push_back(*succ);
            }
        }
    }

    fprintf(yyout, "}\n");
}

void Function::DFS(AsmBuilder* builder, BasicBlock* block, std::map<BasicBlock*, MachineBlock*>& map, std::set<BasicBlock*>& visited) {
    block->genMachineCode(builder);
    map[block] = builder->getBlock();
    visited.insert(block);
    for (auto it = block->succ_begin(); it != block->succ_end(); it++) 
        if (visited.find(*it) == visited.end()) 
            DFS(builder, *it, map, visited);
}

void Function::genMachineCode(AsmBuilder* builder) {
    //std::cout<<"FUNC\n";
    auto cur_unit = builder->getUnit();
    auto cur_func = new MachineFunction(cur_unit, this->sym_ptr);
    builder->setFunction(cur_func);
    std::map<BasicBlock*, MachineBlock*> map;
    for (auto block : block_list) {
        block->genMachineCode(builder);
        map[block] = builder->getBlock();
    }
    //std::set<BasicBlock*> visited;
    //DFS(builder, entry, map, visited);
    // Add pred and succ for every block
    for (auto block : block_list) {
        auto mblock = map[block];
        for (auto pred = block->pred_begin(); pred != block->pred_end(); pred++)
            mblock->addPred(map[*pred]);
        for (auto succ = block->succ_begin(); succ != block->succ_end(); succ++)
            mblock->addSucc(map[*succ]);
    }
    cur_unit->InsertFunc(cur_func);
    //std::cout<<"FUNC E\n";
}