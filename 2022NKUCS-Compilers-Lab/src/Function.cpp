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

// insert the basicblock bb from its block_list.
void Function::insertBlock(BasicBlock* bb) {
    block_list.push_back(bb);
}

// remove the basicblock bb from its block_list.
void Function::remove(BasicBlock* bb) {
    block_list.erase(std::find(block_list.begin(), block_list.end(), bb));
}

void Function::output() const {
    FunctionType* funcType = dynamic_cast<FunctionType*>(sym_ptr->getType());
    Type* retType = funcType->getRetType();
    /*fprintf(yyout, "define %s %s() {\n",
    retType->toStr().c_str(),
    sym_ptr->toStr().c_str());*/
    fprintf(yyout, "define %s %s(%s) {\n",
            retType->toStr().c_str(),
            sym_ptr->toStr().c_str(),
            funcType->paramTypeToStr().c_str());
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
