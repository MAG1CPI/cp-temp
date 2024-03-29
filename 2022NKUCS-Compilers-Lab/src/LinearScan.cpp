#include <algorithm>
#include "LinearScan.h"
#include "MachineCode.h"
#include "LiveVariableAnalysis.h"

LinearScan::LinearScan(MachineUnit* unit) {
    this->unit = unit;
    for (int i = 4; i < 11; i++)
        regs.push_back(i);
    for (int i = 21; i < 48; i++)
        float_regs.push_back(i);
}

void LinearScan::allocateRegisters() {
    for (auto& f : unit->getFuncs()) {
        func = f;
        bool success;
        success = false;
        while (!success)  // repeat until all vregs can be mapped
        {
            computeLiveIntervals();
            success = linearScanRegisterAllocation();
            if (success)  // all vregs can be mapped to real regs
                modifyCode();
            else  // spill vregs that can't be mapped to real regs
                genSpillCode();
        }
    }
}

void LinearScan::makeDuChains() {
    LiveVariableAnalysis lva;
    lva.pass(func);
    du_chains.clear();
    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand*>> liveVar;
    for (auto& bb : func->getBlocks()) {
        liveVar.clear();
        for (auto& t : bb->getLiveOut())
            liveVar[*t].insert(t);
        int no;
        no = i = bb->getInsts().size() + i;
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend(); inst++) {
            (*inst)->setNo(no--);
            for (auto& def : (*inst)->getDef()) {
                if (def->isVReg()) {
                    auto& uses = liveVar[*def];
                    du_chains[def].insert(uses.begin(), uses.end());
                    auto& kill = lva.getAllUses()[*def];
                    std::set<MachineOperand*> res;
                    set_difference(uses.begin(), uses.end(), kill.begin(), kill.end(), inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }
            for (auto& use : (*inst)->getUse()) {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}

void LinearScan::computeLiveIntervals() {
    makeDuChains();
    intervals.clear();
    for (auto& du_chain : du_chains) {
        int t = -1;
        for (auto& use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());
        Interval* interval = new Interval({du_chain.first->getParent()->getNo(), t, false, 0, 0, du_chain.first->isFloat(), {du_chain.first}, du_chain.second});
        intervals.push_back(interval);
    }
    for (auto& interval : intervals) {
        auto uses = interval->uses;
        auto begin = interval->start;
        auto end = interval->end;
        for (auto block : func->getBlocks()) {
            auto liveIn = block->getLiveIn();
            auto liveOut = block->getLiveOut();
            bool in = false;
            bool out = false;
            for (auto use : uses)
                if (liveIn.count(use)) {
                    in = true;
                    break;
                }
            for (auto use : uses)
                if (liveOut.count(use)) {
                    out = true;
                    break;
                }
            if (in && out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                end = std::max(end, (*(block->rbegin()))->getNo());
            } else if (!in && out) {
                for (auto i : block->getInsts())
                    if (i->getDef().size() > 0 &&
                        i->getDef()[0] == *(uses.begin())) {
                        begin = std::min(begin, i->getNo());
                        break;
                    }
                end = std::max(end, (*(block->rbegin()))->getNo());
            } else if (in && !out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                int temp = 0;
                for (auto use : uses)
                    if (use->getParent()->getParent() == block)
                        temp = std::max(temp, use->getParent()->getNo());
                end = std::max(temp, end);
            }
        }
        interval->start = begin;
        interval->end = end;
    }
    bool change;
    change = true;
    while (change) {
        change = false;
        std::vector<Interval*> t(intervals.begin(), intervals.end());
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++) {
                Interval* w1 = t[i];
                Interval* w2 = t[j];
                if (**w1->defs.begin() == **w2->defs.begin()) {
                    std::set<MachineOperand*> temp;
                    set_intersection(w1->uses.begin(), w1->uses.end(), w2->uses.begin(), w2->uses.end(), inserter(temp, temp.end()));
                    if (!temp.empty()) {
                        change = true;
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());
                        // w1->start = std::min(w1->start, w2->start);
                        // w1->end = std::max(w1->end, w2->end);
                        auto w1Min = std::min(w1->start, w1->end);
                        auto w1Max = std::max(w1->start, w1->end);
                        auto w2Min = std::min(w2->start, w2->end);
                        auto w2Max = std::max(w2->start, w2->end);
                        w1->start = std::min(w1Min, w2Min);
                        w1->end = std::max(w1Max, w2Max);
                        auto it = std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }
    sort(intervals.begin(), intervals.end(), compareStart);
}

bool LinearScan::linearScanRegisterAllocation() {
    // Todo
    bool no_spill = true;
    active.clear();
    regs.clear();
    float_regs.clear();

    for (int i = 4; i < 11; i++)
        regs.push_back(i);
    for (int i = 21; i < 48; i++)
        float_regs.push_back(i);

    for (auto& i : intervals)
    {
        expireOldIntervals(i);
        if (i->is_float == false)
        {
            if (regs.empty())
            {
                spillAtInterval(i);
                no_spill = false;
            }
            else
            {
                i->rreg = *(regs.begin());
                regs.erase(regs.begin());
                active.push_back(i);
                sort(active.begin(), active.end(), [](Interval *x, Interval *y) { return x->end < y->end; });
            }
        }
        else
        {
            if (float_regs.empty())
            {
                spillAtInterval(i);
                no_spill = false;
            }
            else
            {
                i->rreg = *(float_regs.begin());
                float_regs.erase(float_regs.begin());
                active.push_back(i);
                sort(active.begin(), active.end(), [](Interval *x, Interval *y) { return x->end < y->end; });
            }
        }
    }
    return no_spill;

    //return true;
}

void LinearScan::modifyCode() {
    for (auto& interval : intervals) {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg);
    }
}

void LinearScan::genSpillCode() {
    for (auto& interval : intervals) {
        if (!interval->spill)
            continue;
        // TODO
        /* HINT:
         * The vreg should be spilled to memory.
         * 1. insert ldr inst before the use of vreg
         * 2. insert str inst after the def of vreg
         */
        interval->disp = -func->AllocSpace(4);

        MachineOperand *fp = new MachineOperand(MachineOperand::REG, 11);
        MachineOperand *stack_offset = new MachineOperand(MachineOperand::IMM, interval->disp);
        for (MachineOperand *use : interval->uses)
        {
            MachineBlock* mblock = use->getParent()->getParent();
            MachineInstruction *cur_minst = use->getParent();
            LoadMInstruction *load_minst = nullptr;
            if (use->isFloat() == false)
                load_minst = new LoadMInstruction(mblock, new MachineOperand(*use), fp, stack_offset);
            else
                load_minst = new LoadMInstruction(mblock, new MachineOperand(*use), fp, stack_offset, LoadMInstruction::VLDR);

            mblock->insertBefore(load_minst, cur_minst);
        }
        for (MachineOperand *def : interval->defs)
        {
            MachineBlock* mblock = def->getParent()->getParent();
            MachineInstruction *cur_minst = def->getParent();
            StoreMInstruction *store_minst = nullptr;
            if (def->isFloat() == false)
                store_minst = new StoreMInstruction(mblock, new MachineOperand(*def), fp, stack_offset);
            else
                store_minst = new StoreMInstruction(mblock, new MachineOperand(*def), fp, stack_offset, StoreMInstruction::VSTR);
            
            mblock->insertAfter(store_minst, cur_minst);
        }
    }
}

void LinearScan::expireOldIntervals(Interval* interval) {
    // Todo
    std::vector<Interval*>::iterator iter = active.begin();
    while (iter != active.end())
    {
        if ((*iter)->end >= interval->start)
            return;
        else
        {
            if ((*iter)->rreg < 11)
            {
                regs.push_back((*iter)->rreg);
                iter = active.erase(iter);
                //不排序每次还是用新的reg，而不是回收的reg
                sort(regs.begin(), regs.end());
            }
            else if ((*iter)->rreg >= 20 && (*iter)->rreg < 48)
            {
                float_regs.push_back((*iter)->rreg);
                iter = active.erase(iter);
                sort(float_regs.begin(), float_regs.end());
            }
            
        }
    }
}

void LinearScan::spillAtInterval(Interval* interval) {
    // Todo
    Interval *spill = *(active.rbegin());
    if(spill->end > interval->end)
    {
        spill->spill = true;
        interval->rreg = spill->rreg;
        active.pop_back();
        active.push_back(interval);
        sort(active.begin(), active.end(), [](Interval *x, Interval *y) { return x->end < y->end; });
    }
    else
        interval->spill = true;
}

bool LinearScan::compareStart(Interval* a, Interval* b) {
    return a->start < b->start;
}