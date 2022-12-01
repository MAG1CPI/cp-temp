#ifndef __UNIT_H__
#define __UNIT_H__

#include <vector>
#include "Function.h"

class Unit {
    typedef std::vector<Function*>::iterator iterator;
    typedef std::vector<Function*>::reverse_iterator r_iterator;

   private:
    std::vector<Function*> func_list;

   public:
    Unit() = default;
    ~Unit();

    iterator begin() { return func_list.begin(); }
    iterator end() { return func_list.end(); }
    r_iterator rbegin() { return func_list.rbegin(); }
    r_iterator rend() { return func_list.rend(); }

    void insertFunc(Function*);
    void removeFunc(Function*);
    void output() const;
};

#endif