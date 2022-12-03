#include "BasicBlock.h"
#include "Function.h"

extern FILE* yyout;

BasicBlock::BasicBlock(Function* func) {
    no = SymbolTable::getLabel();
    parent = func;
    head = new DummyInstruction();

    head->setParent(this);
    func->insertBlock(this);
}

BasicBlock::~BasicBlock() {
    Instruction *inst = head->getNext(), *temp;
    while (inst != head) {
        temp = inst;
        inst = inst->getNext();
        delete temp;
    }
    // delete head;
    for (auto& bb : pred)
        bb->removeSucc(this);
    for (auto& bb : succ)
        bb->removePred(this);
    parent->remove(this);
}

// add the successor basicclock bb.
void BasicBlock::addSucc(BasicBlock* bb) {
    succ.push_back(bb);
}

// remove the successor basicclock bb.
void BasicBlock::removeSucc(BasicBlock* bb) {
    succ.erase(std::find(succ.begin(), succ.end(), bb));
}

// add the predecessor basicclock bb.
void BasicBlock::addPred(BasicBlock* bb) {
    pred.push_back(bb);
}

// remove the predecessor basicblock bb.
void BasicBlock::removePred(BasicBlock* bb) {
    pred.erase(std::find(pred.begin(), pred.end(), bb));
}

// insert the instruction to the front of the basicblock.
void BasicBlock::insertFront(Instruction* inst) {
    Instruction* next_inst = head->getNext();
    inst->setPrev(head);
    inst->setNext(next_inst);
    inst->setParent(this);

    head->setNext(inst);
    next_inst->setPrev(inst);
    return;
}

// insert the instruction to the back of the basicblock.
void BasicBlock::insertBack(Instruction* inst) {
    Instruction* prev_inst = head->getPrev();
    inst->setPrev(prev_inst);
    inst->setNext(head);
    inst->setParent(this);

    prev_inst->setNext(inst);
    head->setPrev(inst);
    return;
}

// insert the instruction dst before src.
void BasicBlock::insertBefore(Instruction* dst, Instruction* src) {
    // Todo
    assert(src);
    Instruction* before_src = src->getPrev();
    before_src->setNext(dst);
    dst->setNext(src);
    src->setPrev(dst);
    dst->setPrev(before_src);

    dst->setParent(this);
}

// remove the instruction from intruction list.
void BasicBlock::remove(Instruction* inst) {
    inst->getPrev()->setNext(inst->getNext());
    inst->getNext()->setPrev(inst->getPrev());
}

void BasicBlock::output() const {
    fprintf(yyout, "B%d:", no);
    // print info of predecessor as comment
    if (!pred.empty()) {
        fprintf(yyout, "%*c; preds = %%B%d", 32, '\t', pred[0]->getNo());
        for (auto i = pred.begin() + 1; i != pred.end(); i++)
            fprintf(yyout, ", %%B%d", (*i)->getNo());
    }
    // output all inst of this basicblock
    fprintf(yyout, "\n");
    for (auto i = head->getNext(); i != head; i = i->getNext())
        i->output();
}
