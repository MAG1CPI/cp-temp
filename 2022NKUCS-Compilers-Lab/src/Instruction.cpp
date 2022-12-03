#include "Instruction.h"
#include "BasicBlock.h"

extern FILE* yyout;

Instruction::Instruction(unsigned instType, BasicBlock* insert_bb) {
    prev = next = this;
    opcode = -1;
    this->instType = instType;
    if (insert_bb != nullptr) {
        insert_bb->insertBack(this);
        parent = insert_bb;
    }
}

Instruction::~Instruction() {
    parent->remove(this);
}

BinaryInstruction::BinaryInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb)
    : Instruction(BINARY, insert_bb) {
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);

    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

BinaryInstruction::~BinaryInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void BinaryInstruction::output() const {
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[0]->getType()->toStr();
    switch (opcode) {
        case ADD:
            if (type == "float")
                op = "fadd";
            else
                op = "add";
            break;
        case SUB:
            if (type == "float")
                op = "fsub";
            else
                op = "sub";
            break;
        case MUL:
            if (type == "float")
                op = "fmul";
            else
                op = "mul";
            break;
        case DIV:
            if (type == "float")
                op = "fdiv";
            else
                op = "sdiv";
            break;
        case MOD:
            if (type == "float")
                op = "frem";
            else
                op = "srem";
            break;
        /*
        case AND:
            op = "and";
            break;
        case OR:
            op = "or";
            break;
        */
        default:
            op = "";
            break;
    }
    fprintf(yyout, "  %s = %s %s %s, %s\n",
            s1.c_str(),
            op.c_str(),
            type.c_str(),
            s2.c_str(),
            s3.c_str());
}

CmpInstruction::CmpInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb)
    : Instruction(CMP, insert_bb) {
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);

    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

CmpInstruction::~CmpInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void CmpInstruction::output() const {
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[1]->getType()->toStr();
    switch (opcode) {
        case EQ:
            op = "eq";
            break;
        case NOTEQ:
            op = "ne";
            break;
        case LESS:
            op = "slt";
            break;
        case LESSEQ:
            op = "sle";
            break;
        case GREATER:
            op = "sgt";
            break;
        case GREATEREQ:
            op = "sge";
            break;
        default:
            op = "";
            break;
    }
    fprintf(yyout, "  %s = icmp %s %s %s, %s\n",
            s1.c_str(),
            op.c_str(),
            type.c_str(),
            s2.c_str(),
            s3.c_str());
}

UncondBrInstruction::UncondBrInstruction(BasicBlock* to, BasicBlock* insert_bb)
    : Instruction(UNCOND, insert_bb) {
    branch = to;
}

void UncondBrInstruction::output() const {
    fprintf(yyout, "  br label %%B%d\n", branch->getNo());
}

CondBrInstruction::CondBrInstruction(BasicBlock* true_branch, BasicBlock* false_branch, Operand* cond, BasicBlock* insert_bb)
    : Instruction(COND, insert_bb) {
    this->true_branch = true_branch;
    this->false_branch = false_branch;

    cond->addUse(this);
    operands.push_back(cond);
}

CondBrInstruction::~CondBrInstruction() {
    operands[0]->removeUse(this);
}

void CondBrInstruction::output() const {
    std::string cond, type;
    cond = operands[0]->toStr();
    type = operands[0]->getType()->toStr();
    int true_label = true_branch->getNo();
    int false_label = false_branch->getNo();
    fprintf(yyout, "  br %s %s, label %%B%d, label %%B%d\n",
            type.c_str(), 
            cond.c_str(), 
            true_label, 
            false_label);
}

CallInstruction::CallInstruction(SymbolEntry* func, std::vector<Operand*> rparams, Operand* dst, BasicBlock* insert_bb)
    : Instruction(CALL, insert_bb), dst(dst), func(func) {
    if (dst)
        dst->setDef(this);
    for (auto rparam : rparams) {
        operands.push_back(rparam);
        rparam->addUse(this);
    }
}

CallInstruction::~CallInstruction() {
    if (dst) {
        dst->setDef(nullptr);
        if (dst->usersNum() == 0)
            delete dst;
    }
    for (int i = 0; i < operands.size(); i++)
        operands[i]->removeUse(this);
}

void CallInstruction::output() const {
    fprintf(yyout, "  ");
    if (dst)
        fprintf(yyout, "%s = ", 
                dst->toStr().c_str());

    FunctionType* type = (FunctionType*)(func->getType());
    fprintf(yyout, "call %s %s(",
            type->getRetType()->toStr().c_str(),
            func->toStr().c_str());
    if(operands.size())
        fprintf(yyout, "%s %s",
                operands[0]->getType()->toStr().c_str(),
                operands[0]->toStr().c_str());
    for (int i = 1; i < operands.size(); i++)
        fprintf(yyout, ", %s %s",
                operands[i]->getType()->toStr().c_str(),
                operands[i]->toStr().c_str());
    fprintf(yyout, ")\n");
}

RetInstruction::RetInstruction(Operand* src, BasicBlock* insert_bb)
    : Instruction(RET, insert_bb) {
    if (src != nullptr) {
        operands.push_back(src);
        src->addUse(this);
    }
}

RetInstruction::~RetInstruction() {
    if (!operands.empty())
        operands[0]->removeUse(this);
}

void RetInstruction::output() const {
    if (operands.empty()) {
        fprintf(yyout, "  ret void\n");
    } else {
        std::string ret, type;
        ret = operands[0]->toStr();
        type = operands[0]->getType()->toStr();
        fprintf(yyout, "  ret %s %s\n",
                type.c_str(),
                ret.c_str());
    }
}

AllocaInstruction::AllocaInstruction(Operand* dst, SymbolEntry* se, BasicBlock* insert_bb)
    : Instruction(ALLOCA, insert_bb) {
    operands.push_back(dst);
    dst->setDef(this);
    this->se = se;
}

AllocaInstruction::~AllocaInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}

void AllocaInstruction::output() const {
    std::string dst, type;
    dst = operands[0]->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "  %s = alloca %s, align 4\n",
            dst.c_str(),
            type.c_str());
}

LoadInstruction::LoadInstruction(Operand* dst, Operand* src_addr, BasicBlock* insert_bb)
    : Instruction(LOAD, insert_bb) {
    operands.push_back(dst);
    operands.push_back(src_addr);

    dst->setDef(this);
    src_addr->addUse(this);
}

LoadInstruction::~LoadInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void LoadInstruction::output() const {
    std::string dst, src, src_type, dst_type;
    dst = operands[0]->toStr();
    src = operands[1]->toStr();
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  %s = load %s, %s %s, align 4\n",
            dst.c_str(),
            dst_type.c_str(),
            src_type.c_str(),
            src.c_str());
}

StoreInstruction::StoreInstruction(Operand* dst_addr, Operand* src, BasicBlock* insert_bb)
    : Instruction(STORE, insert_bb) {
    operands.push_back(dst_addr);
    operands.push_back(src);

    dst_addr->addUse(this);
    src->addUse(this);
}

StoreInstruction::~StoreInstruction() {
    operands[0]->removeUse(this);
    operands[1]->removeUse(this);
}

void StoreInstruction::output() const {
    std::string dst, src, dst_type, src_type;
    dst = operands[0]->toStr();
    src = operands[1]->toStr();
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  store %s %s, %s %s, align 4\n",
            src_type.c_str(),
            src.c_str(),
            dst_type.c_str(),
            dst.c_str());
}
