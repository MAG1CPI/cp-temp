#include "Instruction.h"
#include "BasicBlock.h"
#include "Function.h"

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

BinaryInstruction::BinaryInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb, bool is_array_pointer)
    : Instruction(BINARY, insert_bb) {
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);

    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
    this->is_array_pointer = is_array_pointer;
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
    : Instruction(CALL, insert_bb), func(func) {
    operands.push_back(dst);
    if (dst)
        dst->setDef(this);
    for (auto rparam : rparams) {
        operands.push_back(rparam);
        rparam->addUse(this);
    }
}

CallInstruction::~CallInstruction() {
    if (operands[0]) {
        operands[0]->setDef(nullptr);
        if (operands[0]->usersNum() == 0)
            delete operands[0];
    }
    for (uint32_t i = 1; i < operands.size(); i++)
        operands[i]->removeUse(this);
}

void CallInstruction::output() const {
    fprintf(yyout, "  ");
    // get ret
    if (operands[0])
        fprintf(yyout, "%s = ",
                operands[0]->toStr().c_str());
    // body
    FunctionType* type = (FunctionType*)(func->getType());
    fprintf(yyout, "call %s %s(",
            type->getRetType()->toStr().c_str(),
            func->toStr().c_str());
    if (operands.size() > 1)
        fprintf(yyout, "%s %s",
                operands[1]->getType()->toStr().c_str(),
                operands[1]->toStr().c_str());
    for (uint32_t i = 2; i < operands.size(); i++)
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

UnSignedExtInstruction::UnSignedExtInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb)
    : Instruction(UNSIGNEDEXT, insert_bb) {
    operands.push_back(dst);
    operands.push_back(src);

    dst->setDef(this);
    src->addUse(this);
}

UnSignedExtInstruction::~UnSignedExtInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void UnSignedExtInstruction::output() const {
    Operand* dst = operands[0];
    Operand* src = operands[1];

    std::string dst_str, src_str;
    dst_str = dst->toStr();
    src_str = src->toStr();
    if (src->getType()->toStr() == "i1" && dst->getType()->toStr() == "i32")
        fprintf(yyout, "  %s = zext i1 %s to i32\n", dst_str.c_str(), src_str.c_str());
    // more
}

NEGInstruction::NEGInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb)
    : Instruction(NEG, insert_bb) {
    operands.push_back(dst);
    operands.push_back(src);

    dst->setDef(this);
    src->addUse(this);
}

NEGInstruction::~NEGInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void NEGInstruction::output() const {
    Operand* dst = operands[0];
    Operand* src = operands[1];

    std::string dst_str, src_str;
    dst_str = dst->toStr();
    src_str = src->toStr();
    if (src->getType()->toStr() == "i1")
        fprintf(yyout, "  %s = xor i1 %s, true\n", dst_str.c_str(), src_str.c_str());
    // more
}

MachineOperand* Instruction::genMachineOperand(Operand* ope) {
    auto se = ope->getEntry();
    MachineOperand* mope = nullptr;
    if (se->isConstant()) {
        mope = new MachineOperand(MachineOperand::IMM, dynamic_cast<ConstantSymbolEntry*>(se)->getValue().i);
    } else if (se->isTemporary()) {
        Function* func = this->parent->getParent();
        auto temp_se = dynamic_cast<TemporarySymbolEntry*>(se);
        if (func->haveFParam(temp_se)) {
            int reg_no = find(func->getFParams().begin(), func->getFParams().end(), temp_se) - func->getFParams().begin();
            if (reg_no < 4) {
                mope = new MachineOperand(MachineOperand::REG, reg_no);
            } else  // TODO fparams more than 4
            {
                mope = new MachineOperand(MachineOperand::REG, 3);
                mope->setStackParam();
            }
        } else if (se->getType()->isPtr()) {
            if (dynamic_cast<PointerType*>(se->getType())->getValueType()->isArray()) {
                mope = new MachineOperand(MachineOperand::IMM, dynamic_cast<TemporarySymbolEntry*>(se)->getOffset());
            }
        } else
            mope = new MachineOperand(MachineOperand::VREG, temp_se->getLabel());
    } else if (se->isVariable()) {
        auto id_se = dynamic_cast<IdentifierSymbolEntry*>(se);
        if (id_se->isGlobal())
            mope = new MachineOperand(id_se->toStr().c_str());
        else
            exit(0);
    }
    return mope;
}

MachineOperand* Instruction::genMachineReg(int reg) {
    return new MachineOperand(MachineOperand::REG, reg);
}

MachineOperand* Instruction::genMachineVReg() {
    return new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
}

MachineOperand* Instruction::genMachineImm(int val) {
    return new MachineOperand(MachineOperand::IMM, val);
}

MachineOperand* Instruction::genMachineLabel(int block_no) {
    std::string label = ".L" + std::to_string(block_no);
    return new MachineOperand(label);
}

void AllocaInstruction::genMachineCode(AsmBuilder* builder) {
    /* HINT:
     * Allocate stack space for local variabel
     * Store frame offset in symbol entry */
    // std::cout << "i Alloca \n";
    auto cur_func = builder->getFunction();
    // offset may change for array
    int size = se->getType()->getSize() / 8;
    if (size < 0)
        size = 4;
    int offset = cur_func->AllocSpace(size);
    dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->setOffset(-offset);
    // std::cout << "i Alloca E\n";
}

void LoadInstruction::genMachineCode(AsmBuilder* builder) {
    /*
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    // Load global operand
    if (operands[1]->getEntry()->isVariable() && dynamic_cast<IdentifierSymbolEntry*>(operands[1]->getEntry())->isGlobal()) {
        auto dst = genMachineOperand(operands[0]);
        auto internal_reg1 = genMachineVReg();
        auto internal_reg2 = new MachineOperand(*internal_reg1);
        auto src = genMachineOperand(operands[1]);
        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, src);
        cur_block->InsertInst(cur_inst);
        // example: load r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, dst, internal_reg2);
        cur_block->InsertInst(cur_inst);
    }
    // Load local operand
    else if (operands[1]->getEntry()->isTemporary() && operands[1]->getDef() && operands[1]->getDef()->isAlloc()) {
        // example: load r1, [r0, #4]
        auto dst = genMachineOperand(operands[0]);
        auto src1 = genMachineReg(11);
        auto src2 = genMachineImm(dynamic_cast<TemporarySymbolEntry*>(operands[1]->getEntry())->getOffset());
        cur_inst = new LoadMInstruction(cur_block, dst, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
    // Load operand from temporary variable
    else {
        // example: load r1, [r0]
        auto dst = genMachineOperand(operands[0]);
        auto src = genMachineOperand(operands[1]);
        cur_inst = new LoadMInstruction(cur_block, dst, src);
        cur_block->InsertInst(cur_inst);
    }
    */
    // std::cout << "i Load\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;

    // if (operands[0]->getType()->isInt()) {
    {
        MachineOperand* dst = genMachineOperand(operands[0]);
        // Load global operand
        if (operands[1]->getEntry()->isVariable() &&
            dynamic_cast<IdentifierSymbolEntry*>(operands[1]->getEntry())->isGlobal()) {
            MachineOperand* src = genMachineOperand(operands[1]);
            if (operands[0]->getType()->isArray()) {
                cur_inst = new LoadMInstruction(cur_block, dst, src);
                cur_block->InsertInst(cur_inst);
            } else {
                MachineOperand* internal_reg1 = genMachineVReg();
                MachineOperand* internal_reg2 = new MachineOperand(*internal_reg1);
                // example: load r0, addr_a
                cur_inst = new LoadMInstruction(cur_block, internal_reg1, src);
                cur_block->InsertInst(cur_inst);
                // example: load r1, [r0]
                cur_inst = new LoadMInstruction(cur_block, dst, internal_reg2);
                cur_block->InsertInst(cur_inst);
            }
        }
        // Load local operand
        else if (operands[1]->getEntry()->isTemporary() &&
                 operands[1]->getDef() && operands[1]->getDef()->isAlloc()) {
            // example: load r1, [r0, #4]
            MachineOperand* src1 = genMachineReg(11);
            int off = dynamic_cast<TemporarySymbolEntry*>(operands[1]->getEntry())->getOffset();
            MachineOperand* src2 = genMachineImm(off);
            // 合法立即数的简单判定
            if (off > 255 || off < -255) {
                MachineOperand* temp_operand = genMachineVReg();
                cur_inst = new LoadMInstruction(cur_block, temp_operand, src2);
                cur_block->InsertInst(cur_inst);
                src2 = temp_operand;
            }
            cur_inst = new LoadMInstruction(cur_block, dst, src1, src2);
            cur_block->InsertInst(cur_inst);
        }
        // Load operand from temporary variable
        else if (operands[1]->getEntry()->getType()->isArray()) {
            std::vector<int> dims = dynamic_cast<ArrayType*>(operands[1]->getType())->getDim();
            if (dims[0] == -1 || dynamic_cast<TemporarySymbolEntry*>(operands[1]->getEntry())->isGlobalArray()) {
                auto dst = genMachineOperand(operands[0]);
                auto src_addr = genMachineOperand(operands[1]);
                cur_inst = new LoadMInstruction(cur_block, dst, src_addr);
                cur_block->InsertInst(cur_inst);
            } else {
                MachineOperand* src_addr = genMachineVReg();
                MachineOperand* fp = genMachineReg(11);
                MachineOperand* offset = genMachineOperand(operands[1]);

                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, src_addr, fp, offset);
                cur_block->InsertInst(cur_inst);

                cur_inst = new LoadMInstruction(cur_block, dst, src_addr);
                cur_block->InsertInst(cur_inst);
            }
        } else {
            // example: load r1, [r0]
            MachineOperand* src = genMachineOperand(operands[1]);
            cur_inst = new LoadMInstruction(cur_block, dst, src);
            cur_block->InsertInst(cur_inst);
        }
    }
    // std::cout << "i Load E\n";
}

void StoreInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i Store\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;

    // if (operands[1]->getType()->isInt()) {
    {
        MachineOperand* src = genMachineOperand(operands[1]);

        // if src is a immediate
        if (operands[1]->getEntry()->isConstant()) {
            MachineOperand* temp_operand = genMachineVReg();
            cur_inst = new LoadMInstruction(cur_block, temp_operand, src);
            cur_block->InsertInst(cur_inst);

            src = new MachineOperand(*temp_operand);
        }

        // store global
        if (operands[0]->getEntry()->isVariable() &&
            dynamic_cast<IdentifierSymbolEntry*>(operands[0]->getEntry())->isGlobal()) {
            MachineOperand* dst = genMachineOperand(operands[0]);
            MachineOperand* internal_reg1 = genMachineVReg();
            MachineOperand* internal_reg2 = new MachineOperand(*internal_reg1);
            // example: load r0, addr_a
            cur_inst = new LoadMInstruction(cur_block, internal_reg1, dst);
            cur_block->InsertInst(cur_inst);
            // example: store r1, [r0]
            cur_inst = new StoreMInstruction(cur_block, src, internal_reg2);
            cur_block->InsertInst(cur_inst);

        }
        // store local
        else if (operands[0]->getEntry()->isTemporary() &&
                 operands[0]->getDef() && operands[0]->getDef()->isAlloc()) {
            // example: store r1, [r0, #4]
            MachineOperand* dst1 = genMachineReg(11);
            int off = dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->getOffset();
            MachineOperand* dst2 = genMachineImm(off);
            // 合法立即数的简单判定
            if (off > 255 || off < -255) {
                MachineOperand* temp_operand = genMachineVReg();
                cur_block->InsertInst((new LoadMInstruction(cur_block, temp_operand, dst2)));
                dst2 = temp_operand;
            }
            cur_inst = new StoreMInstruction(cur_block, src, dst1, dst2);
            cur_block->InsertInst(cur_inst);
        }
        // store pointer
        else if (operands[0]->getType()->isPtr()) {
            MachineOperand* dst = genMachineOperand(operands[0]);
            cur_inst = new StoreMInstruction(cur_block, src, dst);
            cur_block->InsertInst(cur_inst);
        } else if (operands[0]->getType()->isArray()) {
            std::vector<int> dims = dynamic_cast<ArrayType*>(operands[0]->getType())->getDim();
            if (dims[0] == -1 || dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->isGlobalArray()) {
                auto dst_addr = genMachineOperand(operands[0]);
                cur_inst = new StoreMInstruction(cur_block, src, dst_addr);
                cur_block->InsertInst(cur_inst);
            } else {
                MachineOperand* dst_addr = genMachineVReg();
                MachineOperand* fp = genMachineReg(11);
                MachineOperand* offset = genMachineOperand(operands[0]);
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst_addr, fp, offset);
                cur_block->InsertInst(cur_inst);

                cur_inst = new StoreMInstruction(cur_block, src, dst_addr);
                cur_block->InsertInst(cur_inst);
            }
        }
    }
    // std::cout << "i Store E\n";
}

void BinaryInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO:
    // complete other instructions
    // std::cout << "i Binary\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineOperand *src1 = nullptr, *src2 = nullptr, *dst = nullptr;
    MachineOperand* internal_reg = nullptr;
    MachineInstruction* cur_inst = nullptr;
    /* HINT:
     * The source operands of ADD instruction in ir code both can be immediate num.
     * However, it's not allowed in assembly code.
     * So you need to insert LOAD/MOV instrucrion to load immediate num into register.
     * As to other instructions, such as MUL, CMP, you need to deal with this situation, too.*/
    /*
    if (src1->isImm()) {
        auto internal_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
        cur_block->InsertInst(cur_inst);
        src1 = new MachineOperand(*internal_reg);
    }
    switch (opcode) {
        case ADD:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, src1, src2);
            break;
        default:
            break;
    }
    cur_block->InsertInst(cur_inst);
    */
    // if (operands[0]->getType()->isInt()) {
    {
        dst = genMachineOperand(operands[0]);
        src1 = genMachineOperand(operands[1]);

        if (is_array_pointer) {
            MachineOperand* fp = genMachineReg(11);
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, fp, src1);
            cur_block->InsertInst(cur_inst);
            return;
        }

        src2 = genMachineOperand(operands[2]);

        // 特殊情况的优化: 两个操作数都是立即数 且 其中之一为零
        if (src1->isImm() && src2->isImm() && opcode == ADD &&
            (src1->getVal() == 0 || src2->getVal() == 0)) {
            // 第一个操作数为零
            if (src1->getVal() == 0) {
                if (src2->getVal() > 255 || src2->getVal() < -256) {
                    internal_reg = genMachineVReg();
                    cur_inst = new LoadMInstruction(cur_block, internal_reg, src2);
                    cur_block->InsertInst(cur_inst);

                    src2 = new MachineOperand(*internal_reg);
                }
                cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src2);
            }
            // 第二个操作数为零
            else {
                if (src1->getVal() > 255 || src1->getVal() < -256) {
                    internal_reg = genMachineVReg();
                    cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
                    cur_block->InsertInst(cur_inst);

                    src1 = new MachineOperand(*internal_reg);
                }
                cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src1);
            }
            cur_block->InsertInst(cur_inst);
            return;
        }

        if (src1->isImm()) {
            internal_reg = genMachineVReg();
            cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
            cur_block->InsertInst(cur_inst);

            src1 = new MachineOperand(*internal_reg);
        }
        // 合法立即数的简单判定: 255以上均load
        if (src2->isImm() &&
            ((opcode >= MUL && opcode <= MOD) || src2->getVal() > 255 || src2->getVal() < -256)) {
            internal_reg = genMachineVReg();
            cur_inst = new LoadMInstruction(cur_block, internal_reg, src2);
            cur_block->InsertInst(cur_inst);

            src2 = new MachineOperand(*internal_reg);
        }

        switch (opcode) {
            case ADD:
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, src1, src2);
                break;
            case SUB:
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, dst, src1, src2);
                break;
            case AND:
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::AND, dst, src1, src2);
                break;
            case OR:
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::OR, dst, src1, src2);
                break;
            case MUL:
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, dst, src1, src2);
                break;
            case DIV:
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, src2);
                break;
            case MOD:
                MachineOperand *div_dst, *mul_dst;

                div_dst = genMachineVReg();
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, div_dst, src1, src2);
                cur_block->InsertInst(cur_inst);

                src2 = new MachineOperand(*src2);
                div_dst = new MachineOperand(*div_dst);
                mul_dst = genMachineVReg();
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, mul_dst, div_dst, src2);
                cur_block->InsertInst(cur_inst);

                src1 = new MachineOperand(*src1);
                mul_dst = new MachineOperand(*mul_dst);
                cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, dst, src1, mul_dst);
                break;
            default:
                break;
        }
        cur_block->InsertInst(cur_inst);
    }
    // std::cout << "i Binary E\n";
}

void CmpInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i Cmp\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineOperand *src1 = nullptr, *src2 = nullptr, *dst = nullptr;
    MachineOperand* internal_reg = nullptr;
    MachineInstruction* cur_inst = nullptr;

    if (operands[1]->getType()->isInt()) {
        src1 = genMachineOperand(operands[1]);
        src2 = genMachineOperand(operands[2]);

        if (src1->isImm()) {
            internal_reg = genMachineVReg();
            cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
            cur_block->InsertInst(cur_inst);

            src1 = new MachineOperand(*internal_reg);
        }
        // 合法立即数的简单判定: 255以上均load, 不考虑负数
        if (src2->isImm() && src2->getVal() > 255) {
            internal_reg = genMachineVReg();
            cur_inst = new LoadMInstruction(cur_block, internal_reg, src2);
            cur_block->InsertInst(cur_inst);

            src2 = new MachineOperand(*internal_reg);
        }

        cur_inst = new CmpMInstruction(cur_block, src1, src2, opcode);
        cur_block->InsertInst(cur_inst);

        uint32_t anti_opcode = 7 - opcode;
        if (opcode <= CmpInstruction::NOTEQ)
            anti_opcode = 1 - opcode;

        dst = genMachineOperand(operands[0]);

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineImm(1), opcode);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineImm(0), anti_opcode);
        cur_block->InsertInst(cur_inst);
    }
    // std::cout << "i Cmp E\n";
}

void UncondBrInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i uncond\n";
    MachineBlock* cur_block = builder->getBlock();
    std::string str = ".L" + std::to_string(branch->getNo());
    MachineOperand* dst = new MachineOperand(str);
    MachineInstruction* cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
    // std::cout << "i uncond E\n";
}

void CondBrInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i cond\n";
    MachineBlock* cur_block = builder->getBlock();
    std::string str;
    MachineOperand* dst;
    MachineInstruction* cur_inst;

    str = ".L" + std::to_string(true_branch->getNo());
    dst = new MachineOperand(str);
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst, cur_block->getCmpCond());
    cur_block->InsertInst(cur_inst);

    str = ".L" + std::to_string(false_branch->getNo());
    dst = new MachineOperand(str);
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
    // std::cout << "i cond E\n";
}

void RetInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    /* HINT:
     * 1. Generate mov instruction to save return value in r0
     * 2. Restore callee saved registers and sp, fp
     * 3. Generate bx instruction */
    // MachineFunction* cur_func = builder->getFunction();
    // std::cout << "i ret\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;

    MachineOperand* dst;
    // save return value
    if (!operands.empty()) {
        MachineOperand* src;
        if (operands[0]->getType()->isInt()) {
            src = genMachineOperand(operands[0]);
            dst = new MachineOperand(MachineOperand::REG, 0);
            if (src->isImm()) {
                cur_inst = new LoadMInstruction(cur_block, dst, src);
            } else {
                cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
            }
        }
        cur_block->InsertInst(cur_inst);
    }

    // goto func end
    std::string func_name = getParent()->getParent()->getSymPtr()->toStr().substr(1);
    dst = new MachineOperand(".L" + func_name + "_END");
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
    // std::cout << "i ret E\n";
}

void CallInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i call\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    MachineOperand *dst, *src;

    uint32_t i = 0;
    uint32_t reg_arg_num = std::min(std::size_t(5), operands.size());
    // std::cout << reg_arg_num << "\n";
    std::vector<MachineOperand*> stack_arg;
    for (i = 1; i < reg_arg_num; i++) {
        dst = new MachineOperand(MachineOperand::REG, i - 1);
        if (operands[i]->isArrayPointer()) {
            // operands[i]->setArrayPointer(false);
            // Type* temp_type = operands[i]->getEntry()->getType();
            cur_inst = new LoadMInstruction(cur_block, dst, genMachineOperand(operands[i]));
            cur_block->InsertInst(cur_inst);
        } else {
            cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineOperand(operands[i]));
            cur_block->InsertInst(cur_inst);
        }
    }
    // std::cout << "i M\n";
    for (; i < operands.size(); i++) {
        stack_arg.push_back(genMachineOperand(operands[i]));
    }
    if (stack_arg.size()) {
        std::reverse(stack_arg.begin(), stack_arg.end());
        cur_inst = new StackMInstruction(cur_block, StackMInstruction::PUSH, stack_arg);
        cur_block->InsertInst(cur_inst);
    }

    std::string func_name = ((IdentifierSymbolEntry*)func)->toStr().substr(1);
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::BL, new MachineOperand(func_name, true));
    cur_block->InsertInst(cur_inst);

    if (!stack_arg.empty()) {
        MachineOperand* sp = genMachineReg(13);
        MachineOperand* off = genMachineImm(stack_arg.size() * 4);
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, sp, sp, off);
        cur_block->InsertInst(cur_inst);
    }

    if (operands[0]) {
        dst = genMachineOperand(operands[0]);
        src = genMachineReg(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
    }
    // std::cout << "i call E\n";
}

void UnSignedExtInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i Ext\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineOperand* dst = genMachineOperand(operands[0]);
    MachineOperand* src = genMachineOperand(operands[1]);

    MovMInstruction* cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
    cur_block->InsertInst(cur_inst);
    // std::cout << "i Ext E\n";
}

void NEGInstruction::genMachineCode(AsmBuilder* builder) {
    // TODO
    // std::cout << "i NEG\n";
    MachineBlock* cur_block = builder->getBlock();
    MachineOperand* dst = genMachineOperand(operands[0]);
    MovMInstruction* cur_inst = nullptr;

    cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineImm(1), MachineInstruction::EQ);
    cur_block->InsertInst(cur_inst);
    cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineImm(0), MachineInstruction::NE);
    cur_block->InsertInst(cur_inst);
    // std::cout << "i NEG E\n";
}
