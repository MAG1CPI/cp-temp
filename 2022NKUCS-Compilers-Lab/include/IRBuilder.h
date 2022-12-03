#ifndef __IRBUILDER_H__
#define __IRBUILDER_H__
#include "Unit.h"

class IRBuilder {
   private:
    Unit* unit;
    BasicBlock* insertBB;  // The current basicblock that instructions should be inserted into.

   public:
    IRBuilder(Unit* unit)
        : unit(unit) {}
    Unit* getUnit() { return unit; }
    BasicBlock* getInsertBB() { return insertBB; }
    void setInsertBB(BasicBlock* bb) { insertBB = bb; }
};

#endif