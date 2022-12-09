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

    Function* getParent() { return parent; }
    int getNo() { return no; }

    bool predEmpty() const { return pred.empty(); }
    bool succEmpty() const { return succ.empty(); }
    bool havePred(BasicBlock* bb) { return std::find(pred.begin(), pred.end(), bb) != pred.end(); }
    bool haveSucc(BasicBlock* bb) { return std::find(succ.begin(), succ.end(), bb) != pred.end(); }
    int getNumOfPred() const { return pred.size(); }
    int getNumOfSucc() const { return succ.size(); }

    void addPred(BasicBlock*);
    void removePred(BasicBlock*);
    void addSucc(BasicBlock*);
    void removeSucc(BasicBlock*);

    bool empty() const { return head->getNext() == head; }
    Instruction* begin() { return head->getNext(); }
    Instruction* end() { return head; }
    Instruction* rbegin() { return head->getPrev(); }
    Instruction* rend() { return head; }

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