#ifndef __BASIC_BLOCK_H__
#define __BASIC_BLOCK_H__

#include <fstream>
#include <set>
#include <vector>
#include "Instruction.h"

class Function;

class BasicBlock {
   private:
    int no;
    Function* parent;
    Instruction* head;
    std::vector<BasicBlock*> pred, succ;  // relation info

   public:
    BasicBlock(Function*);
    ~BasicBlock();

    Instruction* begin() { return head->getNext(); }
    Instruction* end() { return head; }
    Instruction* rbegin() { return head->getPrev(); }
    Instruction* rend() { return head; }

    bool succEmpty() const { return succ.empty(); }
    bool predEmpty() const { return pred.empty(); }
    int getNumOfPred() const { return pred.size(); }
    int getNumOfSucc() const { return succ.size(); }

    Function* getParent() { return parent; }
    int getNo() { return no; }
    bool empty() const { return head->getNext() == head; }

    void addSucc(BasicBlock*);
    void removeSucc(BasicBlock*);
    void addPred(BasicBlock*);
    void removePred(BasicBlock*);

    void insertFront(Instruction*);
    void insertBack(Instruction*);
    void insertBefore(Instruction*, Instruction*);
    void remove(Instruction*);

    void output() const;

   private:
    typedef std::vector<BasicBlock*>::iterator bb_iterator;

   public:
    bb_iterator succ_begin() { return succ.begin(); }
    bb_iterator succ_end() { return succ.end(); }
    bb_iterator pred_begin() { return pred.begin(); }
    bb_iterator pred_end() { return pred.end(); }
};

#endif