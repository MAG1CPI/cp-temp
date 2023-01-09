#ifndef __SYMBOLTABLE_H__
#define __SYMBOLTABLE_H__

#include <assert.h>
#include <map>
#include <string>
#include "Type.h"

class Operand;

union ValueType {
    int i;
    float f;
    int* ip;
    float* fp;
};
const ValueType kZERO = {.f = 0};

class SymbolEntry {
   private:
    int kind;

   protected:
    enum { CONSTANT,
           VARIABLE,
           TEMPORARY };
    Type* type;

   public:
    SymbolEntry(Type* type, int kind);
    virtual ~SymbolEntry() {}

    bool isConstant() const { return kind == CONSTANT; }
    bool isTemporary() const { return kind == TEMPORARY; }
    bool isVariable() const { return kind == VARIABLE; }

    Type* getType() { return type; }
    void setType(Type* type) { this->type = type; }

    virtual std::string toStr() = 0;
};

/*  Symbol entry for literal constant. Example:

    int a = 1;

    Compiler should create constant symbol entry for literal constant '1'.
*/
class ConstantSymbolEntry : public SymbolEntry {
   private:
    ValueType value;

   public:
    ConstantSymbolEntry(Type* type, ValueType value);
    virtual ~ConstantSymbolEntry() {}

    ValueType getValue() const { return value; }

    std::string toStr();
};

/*  Symbol entry for identifier. Example:

    int a;
    int b;
    void f(int c)
    {
        int d;
        {
            int e;
        }
    }

    Compiler should create identifier symbol entries for variables a, b, c, d and e:

    | variable | scope    |
    | a        | GLOBAL   |
    | b        | GLOBAL   |
    | c        | PARAM    |
    | d        | LOCAL    |
    | e        | LOCAL +1 |
*/
class IdentifierSymbolEntry : public SymbolEntry {
   private:
    enum { GLOBAL,
           PARAM,
           LOCAL };
    std::string name;
    int scope;
    Operand* addr;  // The address of the identifier.
                    // You can add any field you need here.
    ValueType value;
    std::vector<ValueType> array_value;

    bool overload;                         // Function overloading.
    IdentifierSymbolEntry* next_overload;  // Next overloading function

   public:
    IdentifierSymbolEntry(Type* type, std::string name, int scope);
    virtual ~IdentifierSymbolEntry() {}

    bool isGlobal() const { return scope == GLOBAL; }
    bool isParam() const { return scope == PARAM; }
    bool isLocal() const { return scope >= LOCAL; }
    bool isOverload() const { return overload; }

    int getScope() const { return scope; }

    void setOverloadFunc(IdentifierSymbolEntry*);
    IdentifierSymbolEntry* getOverloadFunc() const { return next_overload; }

    void setAddr(Operand* addr) { this->addr = addr; }
    Operand* getAddr() { return addr; }

    void setValue(ValueType value) { this->value = value; }
    ValueType getValue() { return value; }

    void pushArrayValue(ValueType value) { array_value.push_back(value); }
    ValueType getArrayValue(int n) { return array_value[n]; }
    int ArrayInitValueNum() { return array_value.size(); }

    std::string toStr();
};

/*  Symbol entry for temporary variable created by compiler. Example:

    int a;
    a = 1 + 2 + 3;

    The compiler would generate intermediate code like:

    t1 = 1 + 2
    t2 = t1 + 3
    a = t2

    So compiler should create temporary symbol entries for t1 and t2:

    | temporary variable | label |
    | t1                 | 1     |
    | t2                 | 2     |
*/
class TemporarySymbolEntry : public SymbolEntry {
   private:
    int stack_offset;
    int label;
    bool is_global_array;

   public:
    TemporarySymbolEntry(Type* type, int label);
    virtual ~TemporarySymbolEntry() {}

    int getLabel() const { return label; }
    void setOffset(int offset) { this->stack_offset = offset; };
    int getOffset() { return this->stack_offset; };

    void setGlobalArray(bool is_global_array) { this->is_global_array = is_global_array; }
    bool isGlobalArray() { return is_global_array; }

    std::string toStr();
};

// symbol table managing identifier symbol entries
class SymbolTable {
   private:
    static int counter;

   private:
    std::map<std::string, SymbolEntry*> symbolTable;
    SymbolTable* prev;
    int level;

   public:
    SymbolTable();
    SymbolTable(SymbolTable* prev);
    ~SymbolTable(){};

    SymbolTable* getPrev() { return prev; }
    int getLevel() { return level; }

    static int getLabel() { return counter++; }

    void install(std::string name, SymbolEntry* entry);
    SymbolEntry* lookup(std::string name, bool current);
};

extern SymbolTable* identifiers;
extern SymbolTable* globals;

#endif
