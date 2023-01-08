#include "Ast.h"
#include <string>
#include "IRBuilder.h"
#include "Instruction.h"
#include "SymbolTable.h"
#include "Type.h"
#include "Unit.h"

extern Unit unit;
extern FILE* yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;

Node::Node() {
    seq = counter++;
    sibling = nullptr;
}

/*
void Node::backPatch(std::vector<Instruction*> &list, BasicBlock* bb)
{
    for(auto &inst:list)
    {
        if(inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if(inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}
*/
void Node::backPatch(std::vector<Instruction*>& list, BasicBlock* bb, bool liststyle) {
    for (auto& inst : list) {
        if (inst->isCond()) {
            if (liststyle == true)
                dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
            else if (liststyle == false)
                dynamic_cast<CondBrInstruction*>(inst)->setFalseBranch(bb);
        } else if (inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

std::vector<Instruction*> Node::merge(std::vector<Instruction*>& list1, std::vector<Instruction*>& list2) {
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Node::swapList(std::vector<Instruction*>& truelist, std::vector<Instruction*>& falselist) {
    truelist.swap(falselist);
}

void ExprNode::int2bool(BasicBlock* insert_bb) {
    Operand* new_dst = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
    Operand* zero = new Operand(new ConstantSymbolEntry(TypeSystem::intType, kZERO));
    new CmpInstruction(CmpInstruction::NOTEQ, new_dst, this->dst, zero, insert_bb);
    this->dst = new_dst;
}

void ExprNode::bool2int(BasicBlock* insert_bb) {
    Operand* new_dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
    new UnSignedExtInstruction(new_dst, this->dst, insert_bb);
    this->dst = new_dst;
}

void Node::insertCondBrInst(Function* func, Node* cond, BasicBlock* insert_bb) {
    BasicBlock *truebranch, *falsebranch;
    truebranch = new BasicBlock(func);
    falsebranch = new BasicBlock(func);

    Operand* dst = dynamic_cast<ExprNode*>(cond)->getOperand();
    Instruction* branch = new CondBrInstruction(truebranch, falsebranch, dst, insert_bb);
    cond->trueList().push_back(branch);
    cond->falseList().push_back(branch);
}

void Ast::genCode(Unit* unit) {
    IRBuilder* builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void FunctionDef::genCode() {
    Unit* unit = builder->getUnit();
    Function* func = new Function(unit, se);
    BasicBlock* entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);
    if (fparam != nullptr)
        fparam->genCode();
    stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
     */
    for (auto basicblock = func->begin(); basicblock != func->end(); basicblock++) {
        Instruction* lastinst = (*basicblock)->rbegin();
        if (lastinst->isUncond()) {
            BasicBlock* branch = dynamic_cast<UncondBrInstruction*>(lastinst)->getBranch();

            branch->addPred(*basicblock);
            (*basicblock)->addSucc(branch);
        } else if (lastinst->isCond()) {
            BasicBlock *truebranch, *falsebranch;
            truebranch = dynamic_cast<CondBrInstruction*>(lastinst)->getTrueBranch();
            falsebranch = dynamic_cast<CondBrInstruction*>(lastinst)->getFalseBranch();

            truebranch->addPred(*basicblock);
            falsebranch->addPred(*basicblock);
            (*basicblock)->addSucc(truebranch);
            (*basicblock)->addSucc(falsebranch);
        }
    }

    for (auto basicblock = func->begin(); basicblock != func->end(); basicblock++) {
        if ((*basicblock)->empty()) {
            for (auto bb = (*basicblock)->pred_begin(); bb != (*basicblock)->pred_end(); bb++) {
                //(*basicblock)->removePred(*bb);
                (*bb)->removeSucc(*basicblock);
                (*bb)->remove((*bb)->rbegin());
            }
        }
        for (auto inst = (*basicblock)->begin(); inst != (*basicblock)->end()->getPrev(); inst = inst->getNext()) {
            if (inst->isCond()) {
                BasicBlock* truebb = dynamic_cast<CondBrInstruction*>(inst)->getTrueBranch();
                BasicBlock* falsebb = dynamic_cast<CondBrInstruction*>(inst)->getFalseBranch();
                if (func->inBlockList(truebb) == false || func->inBlockList(falsebb))
                    (*basicblock)->remove(inst);
            }
        }
    }
    // if func has no returnstmt add return; or return 0;
    for (auto basicblock = func->begin(); basicblock != func->end(); basicblock++) {
        Instruction* lastinst = (*basicblock)->rbegin();
        if (lastinst->isUncond() == false && lastinst->isCond() == false && lastinst->isRet() == false) {
            FunctionType* funcType = dynamic_cast<FunctionType*>(func->getSymPtr()->getType());
            Type* retType = funcType->getRetType();
            if (retType->isVoid())
                new RetInstruction(nullptr, *basicblock);
            else if (retType->isInt()) {
                SymbolEntry* zero_se = new ConstantSymbolEntry(TypeSystem::intType, kZERO);
                Operand* zero = new Operand(zero_se);
                new RetInstruction(zero, *basicblock);
            } else if (retType->isFloat()) {
                SymbolEntry* zero_se = new ConstantSymbolEntry(TypeSystem::floatType, kZERO);
                Operand* zero = new Operand(zero_se);
                new RetInstruction(zero, *basicblock);
            }
        }
    }
}

void UnaryExpr::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    // Function *func = bb->getParent();
    if (op == ADD || op == SUB) {
        //[TODO]FLOAT
        expr->genCode();
        if (expr->getOperandType()->toStr() == "i1")
            expr->bool2int(bb);
        SymbolEntry* se = new ConstantSymbolEntry(dst->getType(), kZERO);
        Operand* src1 = new Operand(se);
        Operand* src2 = expr->getOperand();
        int opcode;
        if (op == ADD)
            opcode = BinaryInstruction::ADD;
        else if (op == SUB)
            opcode = BinaryInstruction::SUB;
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    } else if (op == NOT) {
        expr->genCode();
        //[TODO]FLOAT
        // swapList(expr->trueList(), expr->falseList());
        if (expr->getOperandType()->toStr() == "i32")
            expr->int2bool(bb);
        new NEGInstruction(dst, expr->getOperand(), bb);

        /*
        Function* func = bb->getParent();
        BasicBlock *truebranch, *falsebranch;
        truebranch = new BasicBlock(func);
        falsebranch = new BasicBlock(func);

        Instruction* branch = new CondBrInstruction(truebranch, falsebranch, dst, bb);
        trueList().push_back(branch);
        falseList().push_back(branch);
        */
    }
}

void BinaryExpr::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    if (op == AND) {
        //[TODO]FLOAT
        BasicBlock* trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
        expr1->genCode();
        BasicBlock* expr1_bb = builder->getInsertBB();
        if (expr1->getOperandType()->toStr() == "i32") {
            expr1->int2bool(expr1_bb);
            insertCondBrInst(func, expr1, expr1_bb);
        }

        backPatch(expr1->trueList(), trueBB, true);
        builder->setInsertBB(trueBB);  // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
        expr2->genCode();
        BasicBlock* expr2_bb = builder->getInsertBB();
        if (expr2->getOperandType()->toStr() == "i32") {
            expr2->int2bool(expr2_bb);
            insertCondBrInst(func, expr2, expr2_bb);
        }

        true_list = expr2->trueList();
        false_list = merge(expr1->falseList(), expr2->falseList());

    } else if (op == OR) {
        //[TODO]FLOAT
        BasicBlock* trueBB = new BasicBlock(func);
        expr1->genCode();
        BasicBlock* expr1_bb = builder->getInsertBB();
        if (expr1->getOperandType()->toStr() == "i32") {
            expr1->int2bool(expr1_bb);
            insertCondBrInst(func, expr1, expr1_bb);
        }

        backPatch(expr1->falseList(), trueBB, false);
        builder->setInsertBB(trueBB);
        expr2->genCode();
        BasicBlock* expr2_bb = builder->getInsertBB();
        if (expr2->getOperandType()->toStr() == "i32") {
            expr2->int2bool(expr2_bb);
            insertCondBrInst(func, expr2, expr2_bb);
        }

        true_list = merge(expr1->trueList(), expr2->trueList());
        false_list = expr2->falseList();

    } else if (op >= EQ && op <= GREATEREQ) {
        //[TODO]FLOAT
        expr1->genCode();
        if (expr1->getOperandType()->toStr() == "i1")
            expr1->bool2int(bb);

        expr2->genCode();
        if (expr2->getOperandType()->toStr() == "i1")
            expr2->bool2int(bb);

        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode;
        switch (op) {
            case EQ:
                opcode = CmpInstruction::EQ;
                break;
            case NOTEQ:
                opcode = CmpInstruction::NOTEQ;
                break;
            case LESS:
                opcode = CmpInstruction::LESS;
                break;
            case LESSEQ:
                opcode = CmpInstruction::LESSEQ;
                break;
            case GREATER:
                opcode = CmpInstruction::GREATER;
                break;
            case GREATEREQ:
                opcode = CmpInstruction::GREATEREQ;
                break;
        }
        new CmpInstruction(opcode, dst, src1, src2, bb);

        // true_list = merge(expr1->trueList(), expr2->trueList());
        // false_list = merge(expr1->falseList(), expr2->falseList());

        this->insertCondBrInst(func, this, bb);

    } else if (op >= ADD && op <= MOD) {
        //[TODO]FLOAT
        expr1->genCode();
        if (expr1->getOperandType()->toStr() == "i1")
            expr1->bool2int(bb);

        expr2->genCode();
        if (expr2->getOperandType()->toStr() == "i1")
            expr2->bool2int(bb);

        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode;
        switch (op) {
            case ADD:
                opcode = BinaryInstruction::ADD;
                break;
            case SUB:
                opcode = BinaryInstruction::SUB;
                break;
            case MUL:
                opcode = BinaryInstruction::MUL;
                break;
            case DIV:
                opcode = BinaryInstruction::DIV;
                break;
            case MOD:
                opcode = BinaryInstruction::MOD;
                break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void Constant::genCode() {
    // we don't need to generate code.
}

void Id::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Operand* addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();

    Type* type = symbolEntry->getType();
    Node* index = getIndex();
    if (type->isArray()) {
        // if(dynamic_cast<ArrayType*>(type)->getElementType()->isInt())
        dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
        // else
        //     dst = new Operand(new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel()));
        std::vector<int> dims = dynamic_cast<ArrayType*>(type)->getDim();
        while (index) {
            index->genCode();
            index = index->GetSibling();
        }

        int i = 1;
        ValueType dim_value;
        ConstantSymbolEntry* dim_se;
        Operand* dim_op;
        Operand *offset1_op, *offset2_op;
        TemporarySymbolEntry *offset1_se, *offset2_se;

        index = getIndex();
        if (index) {
            offset1_op = dynamic_cast<ExprNode*>(index)->getOperand();
            index = index->GetSibling();
            // 函数形参
            if (dims[0] == -1) {
                TemporarySymbolEntry* se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
                Operand* new_addr = new Operand(se);
                new LoadInstruction(new_addr, addr, bb);
                addr = new_addr;
            }

            while (index) {
                dim_value.i = dims[i];
                dim_se = new ConstantSymbolEntry(TypeSystem::constintType, dim_value);
                dim_op = new Operand(dim_se);

                offset2_se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
                offset2_op = new Operand(offset2_se);
                new BinaryInstruction(BinaryInstruction::MUL, offset2_op, offset1_op, dim_op, bb);

                offset1_se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
                offset1_op = new Operand(offset1_se);
                new BinaryInstruction(BinaryInstruction::ADD, offset1_op, offset2_op, dynamic_cast<ExprNode*>(index)->getOperand(), bb);
                index = index->GetSibling();
            }

            ValueType align_value;
            offset2_se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            offset2_op = new Operand(offset2_se);
            align_value.i = 4;
            Operand* align = new Operand(new ConstantSymbolEntry(TypeSystem::constintType, align_value));
            new BinaryInstruction(BinaryInstruction::MUL, offset2_op, offset1_op, align, bb);

            TemporarySymbolEntry* final_offset_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            Operand* final_offset = new Operand(final_offset_se);
            // 全局变量地址
            if (dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->isGlobal()) {
                TemporarySymbolEntry* temp_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
                Operand* new_addr = new Operand(temp_se);
                new LoadInstruction(new_addr, addr, bb);
                addr = new_addr;
            }

            new BinaryInstruction(BinaryInstruction::ADD, final_offset, offset2_op, addr, bb);

            new LoadInstruction(dst, final_offset, bb);
        } else {  // 数组指针的函数实参
            Operand* zero_op = new Operand(new ConstantSymbolEntry(TypeSystem::constintType, kZERO));
            new BinaryInstruction(BinaryInstruction::ADD, dst, addr, zero_op, bb, true);
        }
    } else {
        new LoadInstruction(dst, addr, bb);
    }
}

void IfStmt::genCode() {
    Function* func;
    BasicBlock *cond_bb, *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    cond_bb = builder->getInsertBB();
    //[TODO]FLOAT
    if (cond->getOperandType()->toStr() == "i32") {
        cond->int2bool(cond_bb);
        insertCondBrInst(func, cond, cond_bb);
    }

    backPatch(cond->trueList(), then_bb, true);
    backPatch(cond->falseList(), end_bb, false);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode() {
    // Todo
    Function* func;
    BasicBlock *cond_bb, *then_bb, *else_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    cond_bb = builder->getInsertBB();
    //[TODO]FLOAT
    if (cond->getOperandType()->toStr() == "i32") {
        cond->int2bool(cond_bb);
        insertCondBrInst(func, cond, cond_bb);
    }

    backPatch(cond->trueList(), then_bb, true);
    backPatch(cond->falseList(), else_bb, false);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);

    builder->setInsertBB(end_bb);
}

void CompoundStmt::genCode() {
    // Todo
    stmt->genCode();
}

void SeqNode::genCode() {
    // Todo
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode() {
    IdentifierSymbolEntry* se = dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr());
    if (se->isGlobal()) {
        Operand* addr;
        SymbolEntry* addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        if (initval != nullptr) {
            ValueType value;
            // sscanf(initval->getOperand()->toStr().c_str(), "%d", &value.i);
            // TODO!!!
            if (se->getType()->isInt())
                value.i = (int)initval->getValue();
            else if (se->getType()->isFloat())
                value.f = initval->getValue();
            dynamic_cast<IdentifierSymbolEntry*>(se)->setValue(value);
            // std::cout << "add a global var value:" << value << std::endl;
        } else {
            dynamic_cast<IdentifierSymbolEntry*>(se)->setValue(kZERO);
            // std::cout << "add a global var value:" << 0 << std::endl;
        }
        unit.insertGlobalVar(se);
    } else if (se->isLocal()) {
        Function* func = builder->getInsertBB()->getParent();
        BasicBlock* entry = func->getEntry();
        Instruction* alloca;
        Operand* addr;
        SymbolEntry* addr_se;
        Type* type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);  // allocate space for local id in function stack.
        entry->insertFront(alloca);                // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                         // set the addr operand in symbol entry so that we can use it in subsequent code generation.
        if (initval != nullptr) {
            BasicBlock* bb = builder->getInsertBB();
            if (!se->getType()->isArray()) {
                initval->genCode();
                Operand* src = initval->getOperand();
                new StoreInstruction(addr, src, bb);
            } else {
                // initval->genCode();
                // Operand* src = initval->getOperand();
                // new StoreInstruction(addr, src, bb);
            }
        }
    }
    if (HaveSibling())
        GetSibling()->genCode();
}

void ReturnStmt::genCode() {
    // Todo
    BasicBlock* return_bb = builder->getInsertBB();
    if (retValue != nullptr) {
        retValue->genCode();
        new RetInstruction(retValue->getOperand(), return_bb);
    } else
        new RetInstruction(nullptr, return_bb);
}

void AssignStmt::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    expr->genCode();
    Operand* addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    Operand* src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just store the result of the `expr` to the addr of the id.
     * If you want to implement array, you have to caculate the address first and then store the result into it.
     */
    ///*
    Type* type = lval->getSymPtr()->getType();
    if (type->isArray()) {
        Node* index = dynamic_cast<Id*>(lval)->getIndex();
        std::vector<int> dims = dynamic_cast<ArrayType*>(type)->getDim();
        while (index) {
            index->genCode();
            index = index->GetSibling();
        }

        int i = 1;
        ValueType dim_value;
        ConstantSymbolEntry* dim_se;
        Operand* dim_op;
        Operand *offset1_op, *offset2_op;
        TemporarySymbolEntry *offset1_se, *offset2_se;

        index = dynamic_cast<Id*>(lval)->getIndex();
        offset1_op = dynamic_cast<ExprNode*>(index)->getOperand();
        index = index->GetSibling();
        // 函数形参
        if (dims[0] == -1) {
            TemporarySymbolEntry* se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            Operand* new_addr = new Operand(se);
            new LoadInstruction(new_addr, addr, bb);
            addr = new_addr;
        }

        while (index) {
            dim_value.i = dims[i];
            dim_se = new ConstantSymbolEntry(TypeSystem::constintType, dim_value);
            dim_op = new Operand(dim_se);

            offset2_se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            offset2_op = new Operand(offset2_se);
            new BinaryInstruction(BinaryInstruction::MUL, offset2_op, offset1_op, dim_op, bb);

            offset1_se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            offset1_op = new Operand(offset1_se);
            new BinaryInstruction(BinaryInstruction::ADD, offset1_op, offset2_op, dynamic_cast<ExprNode*>(index)->getOperand(), bb);
            index = index->GetSibling();
        }

        ValueType align_value;
        offset2_se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        offset2_op = new Operand(offset2_se);
        align_value.i = 4;
        Operand* align = new Operand(new ConstantSymbolEntry(TypeSystem::constintType, align_value));
        new BinaryInstruction(BinaryInstruction::MUL, offset2_op, offset1_op, align, bb);

        TemporarySymbolEntry* final_offset_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        Operand* final_offset = new Operand(final_offset_se);
        // 全局变量地址
        if (dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->isGlobal()) {
            TemporarySymbolEntry* temp_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            Operand* new_addr = new Operand(temp_se);
            new LoadInstruction(new_addr, addr, bb);
            addr = new_addr;
        }

        new BinaryInstruction(BinaryInstruction::ADD, final_offset, offset2_op, addr, bb);
        new StoreInstruction(final_offset, src, bb);
    } else {  //*/
        new StoreInstruction(addr, src, bb);
    }
}

void ExprStmt::genCode() {
    expr->genCode();
}

void WhileStmt::genCode() {
    BasicBlock* now_bb = builder->getInsertBB();
    Function* func = now_bb->getParent();
    cond_bb = new BasicBlock(func);
    BasicBlock* stmt_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    new UncondBrInstruction(cond_bb, now_bb);

    builder->setInsertBB(cond_bb);
    cond->genCode();
    cond_bb = builder->getInsertBB();
    //[TODO]FLOAT
    if (cond->getOperandType()->toStr() == "i32") {
        cond->int2bool(cond_bb);
        insertCondBrInst(func, cond, cond_bb);
    }

    backPatch(cond->trueList(), stmt_bb, true);
    backPatch(cond->falseList(), end_bb, false);
    // new CondBrInstruction(stmt_bb, end_bb, cond->getOperand(), cond_bb);

    builder->setInsertBB(stmt_bb);
    stmt->genCode();
    stmt_bb = builder->getInsertBB();
    new UncondBrInstruction(cond_bb, stmt_bb);

    builder->setInsertBB(end_bb);
}

void BreakStmt::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->getEndBB(), bb);

    BasicBlock* next_bb = new BasicBlock(func);
    builder->setInsertBB(next_bb);
}

void ContinueStmt::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->getCondBB(), bb);

    BasicBlock* next_bb = new BasicBlock(func);
    builder->setInsertBB(next_bb);
}

void FuncFParam::genCode() {
    // Todo !!!
    IdentifierSymbolEntry* se = dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr());
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* entry = func->getEntry();
    Instruction* alloca;
    Operand *param, *addr;
    SymbolEntry *param_se, *addr_se;
    Type *param_type, *type;

    param_type = se->getType();
    param_se = new TemporarySymbolEntry(param_type, SymbolTable::getLabel());
    param = new Operand(param_se);

    type = new PointerType(se->getType());
    addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
    addr = new Operand(addr_se);
    alloca = new AllocaInstruction(addr, se);  // allocate space for local id in function stack.
    entry->insertFront(alloca);                // allocate instructions should be inserted into the begin of the entry block.
    se->setAddr(addr);                         // set the addr operand in symbol entry so that we can use it in subsequent code generation.

    func->insertFParamSE(dynamic_cast<TemporarySymbolEntry*>(param_se));
    BasicBlock* bb = builder->getInsertBB();
    new StoreInstruction(addr, param, bb);

    if (HaveSibling())
        GetSibling()->genCode();
}

void FunctionCall::genCode() {
    std::vector<Operand*> operands;
    ExprNode* p = rparam;
    while (p) {
        p->genCode();
        operands.push_back(p->getOperand());
        p = ((ExprNode*)p->GetSibling());
    }
    BasicBlock* bb = builder->getInsertBB();
    new CallInstruction(symbolEntry, operands, dst, bb);
}

void NullStmt::genCode() {
    // do nothing
}

void InitValue::genCode() {
    // [TODO]
}

void Ast::typeCheck() {
    if (root != nullptr)
        root->typeCheck();
}

void FunctionDef::typeCheck() {
    // Todo
}

void BinaryExpr::typeCheck() {
    // Todo
}

void Constant::typeCheck() {
    // Todo
}

void Id::typeCheck() {
    // Todo
}

void IfStmt::typeCheck() {
    // Todo
}

void IfElseStmt::typeCheck() {
    // Todo
}

void CompoundStmt::typeCheck() {
    // Todo
}

void SeqNode::typeCheck() {
    // Todo
}

void DeclStmt::typeCheck() {
    // Todo
}

void ReturnStmt::typeCheck() {
    // Todo
}

void AssignStmt::typeCheck() {
    // Todo
}

void UnaryExpr::typeCheck() {
    // Todo
}

void ExprStmt::typeCheck() {
    // Todo
}

void WhileStmt::typeCheck() {
    // Todo
}

void BreakStmt::typeCheck() {
    // Todo
}

void ContinueStmt::typeCheck() {
    // Todo
}

void FuncFParam::typeCheck() {
    // Todo
}

void FunctionCall::typeCheck() {
    // Todo
}

void NullStmt::typeCheck() {
    // do nothing
}

void InitValue::typeCheck() {
}

void Ast::output() {
    fprintf(yyout, "program\n");
    if (root != nullptr)
        root->output(4);
}

void UnaryExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case ADD:
            op_str = "positive";
            break;
        case SUB:
            op_str = "negative";
            break;
        case NOT:
            op_str = "not";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}

void BinaryExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case OR:
            op_str = "or";
            break;
        case AND:
            op_str = "and";
            break;
        case LESS:
            op_str = "less";
            break;
        case LESSEQ:
            op_str = "less_equal";
            break;
        case GREATER:
            op_str = "greater";
            break;
        case GREATEREQ:
            op_str = "greater_equal";
            break;
        case EQ:
            op_str = "equal";
            break;
        case NOTEQ:
            op_str = "not_equal";
            break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void Constant::output(int level) {
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

void Id::output(int level) {
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
    Node* index = getIndex();
    while (index) {
        index->output(level + 4);
        index = index->GetSibling();
    }
}

void CompoundStmt::output(int level) {
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level) {
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level) {
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
    if (initval != nullptr)
        initval->output(level + 4);
    if (HaveSibling()) {
        DeclStmt* sibdeclstmt = dynamic_cast<DeclStmt*>(GetSibling());
        sibdeclstmt->output(level);
    }
}

void IfStmt::output(int level) {
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level) {
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void ReturnStmt::output(int level) {
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if (retValue != nullptr)
        retValue->output(level + 4);
}

void AssignStmt::output(int level) {
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void FunctionDef::output(int level) {
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ',
            name.c_str(), type.c_str());
    if (fparam != nullptr)
        fparam->output(level + 4);
    stmt->output(level + 4);
}

void FuncFParam::output(int level) {
    fprintf(yyout, "%*cFuncFParam\n", level, ' ');
    id->output(level + 4);
    if (HaveSibling()) {
        FuncFParam* sibfparam = dynamic_cast<FuncFParam*>(GetSibling());
        sibfparam->output(level);
    }
}

void ExprStmt::output(int level) {
    fprintf(yyout, "%*cExprStmt\n", level, ' ');
    expr->output(level + 4);
}

void WhileStmt::output(int level) {
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    stmt->output(level + 4);
}

void BreakStmt::output(int level) {
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level) {
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}

void FunctionCall::output(int level) {
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cFunctionCall function name: %s, scope: %d, type: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
    if (rparam != nullptr) {
        rparam->output(level + 4);
        if (rparam->HaveSibling()) {
            ExprNode* next_rparam = rparam;
            while (next_rparam->GetSibling() != nullptr) {
                next_rparam = dynamic_cast<ExprNode*>(next_rparam->GetSibling());
                next_rparam->output(level + 4);
            }
        }
    }
}

void NullStmt::output(int level) {
    fprintf(yyout, "%*cNullStmt\n", level, ' ');
}

void InitValue::output(int level) {
    // [TODO]output
    if (val) {
        val->output(level + 4);
    }
    if (GetSibling()) {
        GetSibling()->output(level);
    }
}

//[TODO]
double UnaryExpr::getValue() {
    switch (op) {
        case ADD:
            return expr->getValue();
        case SUB:
            return -(expr->getValue());
        case NOT:
            return !(expr->getValue());
    }
    return 0;
}

//[TODO]
double BinaryExpr::getValue() {
    switch (op) {
        case ADD:
            return expr1->getValue() + expr2->getValue();
        case SUB:
            return expr1->getValue() - expr2->getValue();
        case MUL:
            return expr1->getValue() * expr2->getValue();
        case DIV:
            if (expr2->getValue() != 0) {
                if (dst->getType()->isInt())
                    return (int)expr1->getValue() / (int)expr2->getValue();
                else if (dst->getType()->isFloat())
                    return expr1->getValue() / expr2->getValue();
            }
        case MOD:
            return (int)(expr1->getValue()) % (int)(expr2->getValue());
        case AND:
            return expr1->getValue() && expr2->getValue();
        case OR:
            return expr1->getValue() || expr2->getValue();
        case EQ:
            return expr1->getValue() == expr2->getValue();
        case NOTEQ:
            return expr1->getValue() != expr2->getValue();
        case LESS:
            return expr1->getValue() < expr2->getValue();
        case LESSEQ:
            return expr1->getValue() <= expr2->getValue();
        case GREATER:
            return expr1->getValue() > expr2->getValue();
        case GREATEREQ:
            return expr1->getValue() >= expr2->getValue();
    }
    return 0;
}

//[TODO]
double Constant::getValue() {
    if (symbolEntry->getType()->isInt())
        return (dynamic_cast<ConstantSymbolEntry*>(symbolEntry))->getValue().i;
    else if (symbolEntry->getType()->isFloat())
        return (dynamic_cast<ConstantSymbolEntry*>(symbolEntry))->getValue().f;
    return 0;
}

//[TODO]
double Id::getValue() {
    Type* type = symbolEntry->getType();
    if (type->isInt())
        return (dynamic_cast<IdentifierSymbolEntry*>(symbolEntry))->getValue().i;
    else if (type->isFloat())
        return (dynamic_cast<IdentifierSymbolEntry*>(symbolEntry))->getValue().f;
    else if (type->isArray()) {
        std::vector<int> dim = (dynamic_cast<ArrayType*>(type))->getDim();
        Type* elementType = (dynamic_cast<ArrayType*>(type))->getElementType();
        Node* index_node = GetSibling();
        int i = 1;
        int index = (dynamic_cast<ExprNode*>(index_node))->getValue();
        index_node = index_node->GetSibling();
        while (index_node) {
            index *= dim[i];
            index += (dynamic_cast<ExprNode*>(index_node))->getValue();
            index_node = index_node->GetSibling();
        }
        if (elementType->isInt())
            return (dynamic_cast<IdentifierSymbolEntry*>(symbolEntry))->getArrayValue(index).i;
        else if (elementType->isFloat())
            return (dynamic_cast<IdentifierSymbolEntry*>(symbolEntry))->getArrayValue(index).f;
    }
    return 0;
}

/******************************
    type:
        1:UnaryExpr
        2:NotExpr
        3:BinaryExpr
        4:logicalExpr
    ******************************/
TemporarySymbolEntry* NewTempSE(int type, ExprNode* expr1, ExprNode* expr2 /* =nullptr */) {
    if (expr1->getOperand() == nullptr)
        return nullptr;
    switch (type) {
        case 1: /*UnaryExpr*/
            if (expr1->getOperandType() == TypeSystem::floatType)
                return new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            else
                return new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        case 2: /*NotExpr*/
            return new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        case 3: /*BinaryExpr*/
            if (expr2->getOperand() == nullptr)
                return nullptr;

            if (expr1->getOperandType() == TypeSystem::floatType || expr2->getOperandType() == TypeSystem::floatType)
                return new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            else
                return new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        case 4: /*logicalExpr*/
            if (expr2->getOperand() == nullptr)
                return nullptr;

            return new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        default:
            return nullptr;
    }
}