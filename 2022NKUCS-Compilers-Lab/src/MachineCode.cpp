#include "MachineCode.h"
extern FILE* yyout;

int MachineBlock::label_no = 0;

MachineOperand::MachineOperand(int tp, int val) {
    this->type = tp;
    if (tp == MachineOperand::IMM)
        this->val = val;
    else
        this->reg_no = val;
}

MachineOperand::MachineOperand(int tp, float float_val, bool is_float) {
    this->is_float = is_float;
    this->type = tp;
    if (tp == MachineOperand::IMM)
        this->float_val = float_val;
    else
        assert(0);
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
            if (reg_no >= 16)
            {
                int float_reg_no = reg_no - 16;
                if (float_reg_no <= 31)
                    fprintf(yyout, "s%d", float_reg_no);
                else if (float_reg_no == 32)
                    fprintf(yyout, "FPSCR");
            }
            else
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
            if (is_float)
            {
                uint32_t value = reinterpret_cast<uint32_t&>(this->float_val);
                fprintf(yyout, "#%u", value);
            }
            else
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
            else {
                // TODO
                if (this->label[0] == '@')
                    fprintf(yyout, "addr_%s_%d", this->label.substr(1).c_str(), parent->getParent()->getParent()->getParent()->getLabelNo());
                else
                    fprintf(yyout, "addr_%s_%d", this->label.c_str(), parent->getParent()->getParent()->getParent()->getLabelNo());
            }
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
                                   int op,
                                   int cond) {
    this->parent = p;
    this->type = MachineInstruction::LOAD;
    this->op = op;
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
    if (this->op == LoadMInstruction::LDR)
        fprintf(yyout, "\tldr ");
    else if (this->op == LoadMInstruction::VLDR)
        fprintf(yyout, "\tvldr.32 ");
    else
        assert(0);
    
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: ldr r1, =8
    if (this->use_list[0]->isImm()) {
        if (this->use_list[0]->isFloat())
        {
            float float_val = this->use_list[0]->getFloatVal();
            uint32_t value = reinterpret_cast<uint32_t&>(float_val);
            fprintf(yyout, "=%u\n", value);
        }
        else
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
                                     int op,
                                     int cond) {
    this->parent = p;
    this->type = MachineInstruction::STORE;
    this->op = op;
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
    if (this->op == StoreMInstruction::STR)
        fprintf(yyout, "\tstr ");
    else if (this->op == StoreMInstruction::VSTR)
        fprintf(yyout, "\tvstr.32 ");
    else
        assert(0);

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
    switch(this->op)
    {
        case MOV:
            fprintf(yyout, "\tmov");
            break;
        case VMOV:
            fprintf(yyout, "\tvmov");
            break;
        //add more if need
        default:
            break;
    }

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
                                 int cond,
                                 int op) {
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
        case CMP:
            fprintf(yyout, "\tcmp ");
            break;
        case VCMP:
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
                                     int cond) {
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
            case VPUSH:
                fprintf(yyout, "\tvpush ");
                break;
            case VPOP:
                fprintf(yyout, "\tvpop ");
                break;
            }
            fprintf(yyout, "{");
        if(use_list.size() > 16)
        {
            auto reg = use_list.begin();
            (*reg)->output();
            reg++;
            int count = 1;
            while(count < 16)
            {
                fprintf(yyout, ", ");
                (*reg)->output();
                reg++;
                count++;
            }
            fprintf(yyout, "}\n");

            if (op == VPUSH)
                fprintf(yyout, "\tvpush ");
            else if (op == VPOP)
                fprintf(yyout, "\tvpop ");
            else
                assert(0);
            
            fprintf(yyout, "{");
            (*reg)->output();
            for (reg++; reg < use_list.end(); reg++)
            {
                fprintf(yyout, ", ");
                (*reg)->output();
            }
        }
        else
        {
            auto reg = use_list.begin();
            (*reg)->output();
            for (reg++; reg < use_list.end(); reg++)
            {
                fprintf(yyout, ", ");
                (*reg)->output();
            }
        }

        fprintf(yyout, "}\n");
    }
}

//[TODO] vcvt vmrs
VcvtMInstruction::VcvtMInstruction(MachineBlock* p,
                                   int op,
                                   MachineOperand* dst,
                                   MachineOperand* src,
                                   int cond) {
    this->parent = p;
    this->type = MachineInstruction::VCVT;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}

void VcvtMInstruction::output() {
    if (this->op == VcvtMInstruction::F2I)
        fprintf(yyout, "\tvcvt.s32.f32 ");
    else if (this->op == VcvtMInstruction::I2F)
        fprintf(yyout, "\tvcvt.f32.s32 ");
    else
        assert(0);
    
    PrintCond();
    fprintf(yyout, " ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

VmrsMInstruction::VmrsMInstruction(MachineBlock* p) {
    this->parent = p;
    this->type = MachineInstruction::VMRS;
}

void VmrsMInstruction::output() {
    fprintf(yyout, "\tvmrs APSR_nzcv, FPSCR\n");
}

MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr) {
    this->parent = p;
    this->sym_ptr = sym_ptr;
    this->stack_size = 0;
};

void MachineBlock::insertBefore(MachineInstruction* before, MachineInstruction* cur) {
    std::vector<MachineInstruction*>::iterator position;
    position = find(inst_list.begin(), inst_list.end(), cur);
    inst_list.insert(position, before);
}

void MachineBlock::insertAfter(MachineInstruction* after, MachineInstruction* cur) {
    std::vector<MachineInstruction*>::iterator position;
    position = find(inst_list.begin(), inst_list.end(), cur);
    inst_list.insert(position + 1, after);
}

void MachineBlock::output() {
    if (this->inst_list.empty())
        return;

    std::vector<MachineInstruction*> store_list;
    int base_offset = 4 * (parent->getSavedRegs().size() + 2);  // fp and lr
    for (auto iter = inst_list.begin(); iter != inst_list.end(); iter++) {
        MachineOperand* operand = (*iter)->getUse()[0];
        if ((*iter)->isStore() && operand->isStackParam())
            store_list.push_back((*iter));
    }
    for (auto iter = store_list.begin(); iter != store_list.end(); iter++) {
        MachineOperand* fp = new MachineOperand(MachineOperand::REG, 11);
        MachineOperand* r3 = new MachineOperand(MachineOperand::REG, 3);
        int offset = base_offset + 4 * (store_list.size() - (iter - store_list.begin()) - 1);
        MachineOperand* stack_offset = new MachineOperand(MachineOperand::IMM, offset);
        LoadMInstruction* load_minst = new LoadMInstruction(this, r3, fp, stack_offset);
        this->insertBefore(load_minst, (*iter));
    }

    fprintf(yyout, ".L%d:\n", this->no);
    //int count = 0;
    for(auto iter : inst_list) {
        iter->output();
        /*count++;
        if(count % 500 == 0) {
            fprintf(yyout, "\tb .B%d\n", label_no);
            fprintf(yyout, ".LTORG\n");
            parent->getParent()->PrintGlobalLabel();
            fprintf(yyout, ".B%d:\n", label_no + 1);
            label_no++;
        }*/
    }
}
std::vector<MachineOperand*> MachineFunction::getSavedRegs() {
    std::vector<MachineOperand*> regs;
    for (auto reg_no : saved_regs)
        regs.push_back(new MachineOperand(MachineOperand::REG, reg_no));
    return regs;
}

int MachineFunction::getInstNum()
{
    int inst_num = 0;
    for (auto block : block_list)
        inst_num += block->getInstNum();
    return inst_num;
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
    //for (auto iter : block_list) {
    //    iter->output();
    //int count = 0;
    for (auto iter : block_list) {
        iter->output();
        /*count += iter->getInstNum();
        if (count > 160) {
            fprintf(yyout, "\tb .F%d\n", parent->getLabelNo());
            fprintf(yyout, ".LTORG\n");
            parent->PrintGlobalLabel();
            fprintf(yyout, ".F%d:\n", parent->getLabelNo() - 1);
            count = 0;
        }*/
    }


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
    if (globalvar_list.size() > 0) {
        fprintf(yyout, "\t.data\n");
        std::string globalvar_name;
        for (auto id_se : globalvar_list)
        {
            if(id_se->getType()->isArray()) //array
            {
                globalvar_name = id_se->toStr().substr(1);
                if(id_se->ArrayInitValueNum() != 0)
                {
                    fprintf(yyout, "\t.global %s\n", globalvar_name.c_str());
                    fprintf(yyout, "\t.align 4\n");
                    fprintf(yyout, "\t.size %s, %d\n", globalvar_name.c_str(), id_se->getType()->getSize() / 8);
                    fprintf(yyout, "%s:\n", globalvar_name.c_str());
                    if(dynamic_cast<ArrayType *>(id_se->getType())->getElementType()->isInt())
                    {
                        for(int i = 0; i < id_se->ArrayInitValueNum(); i++)
                        {
                            fprintf(yyout, "\t.word %d\n", id_se->getArrayValue(i).i);
                        }
                        if(id_se->getType()->getSize() / 8 - id_se->ArrayInitValueNum() * 4 != 0)
                            fprintf(yyout, "\t.space %d\n", id_se->getType()->getSize() / 8 - id_se->ArrayInitValueNum() * 4);
                    }
                    else if (dynamic_cast<ArrayType *>(id_se->getType())->getElementType()->isFloat())
                    {
                        for(int i = 0; i < id_se->ArrayInitValueNum(); i++)
                        {
                            float value = id_se->getArrayValue(i).f;
                            uint32_t print_value = reinterpret_cast<uint32_t&>(value);
                            fprintf(yyout, "\t.word %u\n", print_value);
                        }
                        if(id_se->getType()->getSize() / 8 - id_se->ArrayInitValueNum() * 4 != 0)
                            fprintf(yyout, "\t.space %d\n", id_se->getType()->getSize() / 8 - id_se->ArrayInitValueNum() * 4);
                    }
                    else
                        assert(0);
                }
                else
                {
                    fprintf(yyout, "\t.comm\t%s,%d,4\n", globalvar_name.c_str(), id_se->getType()->getSize() / 8);
                }
            }
            else //var
            {
                globalvar_name = id_se->toStr().substr(1);
                fprintf(yyout, "\t.global %s\n", globalvar_name.c_str());
                fprintf(yyout, "\t.align 4\n");
                fprintf(yyout, "\t.size %s, %d\n", globalvar_name.c_str(), id_se->getType()->getSize() / 8);
                fprintf(yyout, "%s:\n", globalvar_name.c_str());
                if (id_se->getType()->isInt())
                {
                    fprintf(yyout, "\t.word %d\n", id_se->getValue().i);
                }
                else if (id_se->getType()->isFloat())
                {
                    float value = id_se->getValue().f;
                    uint32_t print_value = reinterpret_cast<uint32_t&>(value);
                    fprintf(yyout, "\t.word %u\n", print_value);
                }
                else
                    assert(0);
            }
            
        }
    }
}

void MachineUnit::PrintGlobalLabel()
{
    std::string globalvar_name;
    for (auto id_se : globalvar_list) {
        globalvar_name = id_se->toStr().substr(1);
        fprintf(yyout, "addr_%s_%d:\n", globalvar_name.c_str(), label_no);
        fprintf(yyout, "\t.word %s\n", globalvar_name.c_str());
    }
    label_no++;
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
    fprintf(yyout, "\t.text\n");
    //for (auto iter : func_list)
    //    iter->output();
    //int count = 0;
    for (auto iter : func_list) {
        iter->output();
        /*count += iter->getInstNum();
        if (count > 600) {
            fprintf(yyout, "\tb .F%d\n", label_no);
            fprintf(yyout, ".LTORG\n");
            PrintGlobalLabel();
            fprintf(yyout, ".F%d:\n", label_no - 1);
            count = 0;
        }*/
    }
    /*
    std::string globalvar_name;
    for (auto id_se : globalvar_list) {
        globalvar_name = id_se->toStr().substr(1);
        fprintf(yyout, "addr_%s:\n", globalvar_name.c_str());
        fprintf(yyout, "\t.word %s\n", globalvar_name.c_str());
    }*/
    PrintGlobalLabel();
}
