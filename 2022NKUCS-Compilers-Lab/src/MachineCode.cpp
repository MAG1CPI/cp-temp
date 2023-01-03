#include "MachineCode.h"
extern FILE* yyout;

MachineOperand::MachineOperand(int tp, int val) {
    this->type = tp;
    if (tp == MachineOperand::IMM)
        this->val = val;
    else
        this->reg_no = val;
}

MachineOperand::MachineOperand(std::string label, bool is_func) {
    this->type = MachineOperand::LABEL;
    this->label = label;
    this->is_func = is_func;
}

bool MachineOperand::operator==(const MachineOperand& a) const {
    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    else if (this->type == LABEL)
        return this->label == a.label;
    return this->reg_no == a.reg_no;
}

bool MachineOperand::operator<(const MachineOperand& a) const {
    if (this->type == a.type) {
        if (this->type == IMM)
            return this->val < a.val;
        else if (this->type == LABEL)
            return this->label < a.label;
        return this->reg_no < a.reg_no;
    }
    return this->type < a.type;
}

void MachineOperand::PrintReg() {
    switch (reg_no) {
        case 11:
            fprintf(yyout, "fp");
            break;
        case 13:
            fprintf(yyout, "sp");
            break;
        case 14:
            fprintf(yyout, "lr");
            break;
        case 15:
            fprintf(yyout, "pc");
            break;
        default:
            fprintf(yyout, "r%d", reg_no);
            break;
    }
}

void MachineOperand::output() {
    /* HINT：print operand
     * Example:
     * immediate num 1 -> print #1;
     * register 1 -> print r1;
     * lable addr_a -> print addr_a; */
    switch (this->type) {
        case IMM:
            fprintf(yyout, "#%d", this->val);
            break;
        case VREG:
            fprintf(yyout, "v%d", this->reg_no);
            break;
        case REG:
            PrintReg();
            break;
        case LABEL:
            if (this->label.substr(0, 2) == ".L" || is_func)
                fprintf(yyout, "%s", this->label.c_str());
            else
                fprintf(yyout, "addr_%s", this->label.c_str());
        default:
            break;
    }
}

void MachineInstruction::PrintCond() {
    // TODO
    switch (cond) {
        case EQ:
            fprintf(yyout, "eq");
            break;
        case NE:
            fprintf(yyout, "ne");
            break;
        case LT:
            fprintf(yyout, "lt");
            break;
        case LE:
            fprintf(yyout, "le");
            break;
        case GT:
            fprintf(yyout, "gt");
            break;
        case GE:
            fprintf(yyout, "ge");
            break;
        default:
            break;
    }
}

BinaryMInstruction::BinaryMInstruction(MachineBlock* p,
                                       int op,
                                       MachineOperand* dst,
                                       MachineOperand* src1,
                                       MachineOperand* src2,
                                       int cond) {
    this->parent = p;
    this->type = MachineInstruction::BINARY;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    src2->setParent(this);
}

void BinaryMInstruction::output() {
    // TODO:
    // Complete other instructions
    switch (this->op) {
        case BinaryMInstruction::ADD:
            fprintf(yyout, "\tadd ");
            break;
        case BinaryMInstruction::SUB:
            fprintf(yyout, "\tsub ");
            break;
        case BinaryMInstruction::AND:
            fprintf(yyout, "\tand ");
            break;
        case BinaryMInstruction::OR:
            fprintf(yyout, "\torr ");
            break;
        case BinaryMInstruction::MUL:
            fprintf(yyout, "\tmul ");
            break;
        case BinaryMInstruction::DIV:
            fprintf(yyout, "\tsdiv ");
            break;
        case BinaryMInstruction::VADD:
            fprintf(yyout, "\tvadd.f32 ");
            break;
        case BinaryMInstruction::VSUB:
            fprintf(yyout, "\tvsub.f32 ");
            break;
        case BinaryMInstruction::VMUL:
            fprintf(yyout, "\tvmul.f32 ");
            break;
        case BinaryMInstruction::VDIV:
            fprintf(yyout, "\tvdiv.f32 ");
            break;
        default:
            break;
    }
    this->PrintCond();
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

LoadMInstruction::LoadMInstruction(MachineBlock* p,
                                   MachineOperand* dst,
                                   MachineOperand* src1,
                                   MachineOperand* src2,
                                   int cond) {
    this->parent = p;
    this->type = MachineInstruction::LOAD;
    this->op = -1;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    if (src2)
        this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    if (src2)
        src2->setParent(this);
}

void LoadMInstruction::output() {
    fprintf(yyout, "\tldr ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: ldr r1, =8
    if (this->use_list[0]->isImm()) {
        fprintf(yyout, "=%d\n", this->use_list[0]->getVal());
        return;
    }

    // Load address
    if (this->use_list[0]->isReg() || this->use_list[0]->isVReg())
        fprintf(yyout, "[");

    this->use_list[0]->output();
    if (this->use_list.size() > 1) {
        fprintf(yyout, ", ");
        this->use_list[1]->output();
    }

    if (this->use_list[0]->isReg() || this->use_list[0]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

StoreMInstruction::StoreMInstruction(MachineBlock* p,
                                     MachineOperand* src1,
                                     MachineOperand* src2,
                                     MachineOperand* src3,
                                     int cond) {
    this->parent = p;
    this->type = MachineInstruction::STORE;
    this->op = -1;
    this->cond = cond;
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    if (src3)
        this->use_list.push_back(src3);
    src1->setParent(this);
    src2->setParent(this);
    if (src3)
        src3->setParent(this);
}

void StoreMInstruction::output() {
    // TODO
    fprintf(yyout, "\tstr ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");

    // store address
    if (this->use_list[1]->isReg() || this->use_list[1]->isVReg())
        fprintf(yyout, "[");

    this->use_list[1]->output();
    if (this->use_list.size() > 2) {
        fprintf(yyout, ", ");
        this->use_list[2]->output();
    }

    if (this->use_list[1]->isReg() || this->use_list[1]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

MovMInstruction::MovMInstruction(MachineBlock* p,
                                 int op,
                                 MachineOperand* dst,
                                 MachineOperand* src,
                                 int cond) {
    // TODO
    this->parent = p;
    this->type = MachineInstruction::MOV;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}

void MovMInstruction::output() {
    // TODO
    fprintf(yyout, "\tmov");
    PrintCond();
    fprintf(yyout, " ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

BranchMInstruction::BranchMInstruction(MachineBlock* p,
                                       int op,
                                       MachineOperand* dst,
                                       int cond) {
    // TODO
    this->parent = p;
    this->type = MachineInstruction::BRANCH;
    this->op = op;
    this->cond = cond;
    this->use_list.push_back(dst);
    dst->setParent(this);

    MachineOperand* reg = new MachineOperand(MachineOperand::REG, 0);
    reg->setParent(this);
    this->use_list.push_back(reg);
}

void BranchMInstruction::output() {
    // TODO
    switch (op) {
        case B:
            fprintf(yyout, "\tb");
            break;
        case BL:
            fprintf(yyout, "\tbl");
            break;
        case BX:
            fprintf(yyout, "\tbx");
            break;
    }
    PrintCond();
    fprintf(yyout, " ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

CmpMInstruction::CmpMInstruction(MachineBlock* p,
                                 MachineOperand* src1,
                                 MachineOperand* src2,
                                 int cond) {
    // TODO
    this->parent = p;
    this->type = MachineInstruction::CMP;
    this->op = op;
    this->cond = cond;
    p->setCmpCond(cond);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    src1->setParent(this);
    src2->setParent(this);
}

void CmpMInstruction::output() {
    // TODO
    // Jsut for reg alloca test
    // delete it after test
    switch (this->op) {
        case CmpMInstruction::CMP:
            fprintf(yyout, "\tcmp ");
            break;
        case CmpMInstruction::VCMP:
            fprintf(yyout, "\tvcmp.f32 ");
            break;
        default:
            break;
    }
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

StackMInstruction::StackMInstruction(MachineBlock* p,
                                     int op,
                                     MachineOperand* src,
                                     int cond) {
    // TODO
    this->parent = p;
    this->type = MachineInstruction::STACK;
    this->op = op;
    this->cond = cond;
    this->use_list.push_back(src);
    src->setParent(this);
}

StackMInstruction::StackMInstruction(MachineBlock* p,
                                     int op,
                                     std::vector<MachineOperand*>& srcs,
                                     int cond = MachineInstruction::NONE) {
    // TODO
    this->parent = p;
    this->type = MachineInstruction::STACK;
    this->op = op;
    this->cond = cond;
    this->use_list = srcs;
    for (auto src : srcs)
        src->setParent(this);
}
void StackMInstruction::output() {
    // TODO
    if (!use_list.empty()) {
        switch (op) {
            case PUSH:
                fprintf(yyout, "\tpush ");
                break;
            case POP:
                fprintf(yyout, "\tpop ");
                break;
        }
        fprintf(yyout, "{");
        auto reg = use_list.begin();
        (*reg)->output();
        for (reg++; reg < use_list.end(); reg++) {
            fprintf(yyout, ", ");
            (*reg)->output();
        }
        fprintf(yyout, "}\n");
    }
}

MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr) {
    this->parent = p;
    this->sym_ptr = sym_ptr;
    this->stack_size = 0;
};

void MachineBlock::output() {
    if (this->inst_list.empty())
        return;
    fprintf(yyout, ".L%d:\n", this->no);
    for (auto iter : inst_list)
        iter->output();
}
std::vector<MachineOperand*> MachineFunction::getSavedRegs() {
    std::vector<MachineOperand*> regs;
    for (auto reg_no : saved_regs)
        regs.push_back(new MachineOperand(MachineOperand::REG, reg_no));
    return regs;
}

void MachineFunction::output() {
    // const char* func_name = this->sym_ptr->toStr().c_str() + 1;
    std::string func_name = sym_ptr->toStr().substr(1);
    fprintf(yyout, "\t.global %s\n", func_name.c_str());
    fprintf(yyout, "\t.type %s , %%function\n", func_name.c_str());
    fprintf(yyout, "%s:\n", func_name.c_str());
    // TODO
    /* Hint:
     *  1. Save fp
     *  2. fp = sp
     *  3. Save callee saved register
     *  4. Allocate stack space for local variable */

    // save
    fprintf(yyout, "\tpush {");
    for (auto reg : getSavedRegs()) {
        reg->output();
        fprintf(yyout, ", ");
    }
    fprintf(yyout, "fp, lr}\n");

    fprintf(yyout, "\tmov fp, sp\n");

    if (stack_size != 0) {
        fprintf(yyout, "\tsub sp, sp, #%d\n", stack_size);
    }

    // Traverse all the block in block_list to print assembly code.
    for (auto iter : block_list)
        iter->output();

    // recover
    fprintf(yyout, ".L%s_END:\n", func_name.c_str());

    if (stack_size != 0) {
        fprintf(yyout, "\tadd sp, sp, #%d\n", stack_size);
    }
    fprintf(yyout, "\tpop {");
    for (auto reg : getSavedRegs()) {
        reg->output();
        fprintf(yyout, ", ");
    }
    fprintf(yyout, "fp, lr}\n");

    fprintf(yyout, "\tbx lr\n\n");
}

void MachineUnit::PrintGlobalDecl() {
    // TODO:
    // You need to print global variable/const declarition code;
    fprintf(yyout, "\t.data\n");
    std::string globalvar_name;
    for (auto id_se : globalvar_list) {
        globalvar_name = id_se->toStr().substr(1);
        fprintf(yyout, "\t.global %s\n", globalvar_name.c_str());
        fprintf(yyout, "\t.align 4\n");
        fprintf(yyout, "\t.size %s, %d\n", globalvar_name.c_str(), id_se->getType()->getSize());
        fprintf(yyout, "%s:\n", globalvar_name.c_str());
        if (id_se->getType()->isInt()) {
            fprintf(yyout, "\t.word %d\n", id_se->getValue().i);
        }
    }
}

void MachineUnit::output() {
    // TODO
    /* Hint:
     * 1. You need to print global variable/const declarition code;
     * 2. Traverse all the function in func_list to print assembly code;
     * 3. Don't forget print bridge label at the end of assembly code!! */
    fprintf(yyout, "\t.arch armv8-a\n");
    fprintf(yyout, "\t.arch_extension crc\n");
    fprintf(yyout, "\t.arm\n");

    PrintGlobalDecl();

    for (auto iter : func_list)
        iter->output();

    std::string globalvar_name;
    for (auto id_se : globalvar_list) {
        globalvar_name = id_se->toStr().substr(1);
        fprintf(yyout, "addr_%s:\n", globalvar_name.c_str());
        fprintf(yyout, "\t.word %s\n", globalvar_name.c_str());
    }
}
