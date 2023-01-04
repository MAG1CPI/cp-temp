#ifndef __TYPE_H__
#define __TYPE_H__

#include <assert.h>
#include <fstream>
#include <string>
#include <vector>

class Type {
   private:
    int kind;

   protected:
    enum { INT,
           FLOAT,
           VOID,
           ARRAY,
           FUNC,
           PTR };
    int size;
    bool is_const;

   public:
    Type(int kind, int size, bool is_const = false)
        : kind(kind), size(size), is_const(is_const) {}
    virtual ~Type() {}

    bool isInt() const { return kind == INT; }
    bool isFloat() const { return kind == FLOAT; }
    bool isVoid() const { return kind == VOID; }
    bool isArray() const { return kind == ARRAY; }
    bool isFunc() const { return kind == FUNC; }
    bool isPtr() const { return kind == PTR; }

    bool isConst() const { return is_const; }
    int getSize() const { return this->size; }

    virtual std::string toStr() = 0;
};

class IntType : public Type {
   public:
    IntType(int size, bool is_const = false)
        : Type(Type::INT, size, is_const) {}

    std::string toStr();
};

class FloatType : public Type {
   public:
    FloatType(bool is_const = false)
        : Type(Type::FLOAT, 32, is_const) {}

    std::string toStr();
};

class VoidType : public Type {
   public:
    VoidType()
        : Type(Type::VOID, 0) {}

    std::string toStr();
};

class ArrayType : public Type {
   private:
    Type* elementType;
    int length;

   public:
    ArrayType(Type* elementType, int length, bool is_const = false)
        : Type(Type::ARRAY, 0, is_const), elementType(elementType), length(length) {}

    Type* getElementType() const { return elementType; }
    int getLength() const { return length; }

    std::string toStr();
};

class FunctionType : public Type {
   protected:
    Type* returnType;
    std::vector<Type*> paramsType;

   public:
    FunctionType(Type* returnType, std::vector<Type*> paramsType)
        : Type(Type::FUNC, 0), returnType(returnType), paramsType(paramsType) {}

    Type* getRetType() { return returnType; }
    std::vector<Type*>* getParamsType() { return &paramsType; }

    std::string toStr();
};

class PointerType : public Type {
   protected:
    Type* valueType;

   public:
    PointerType(Type* valueType)
        : Type(Type::PTR, 32), valueType(valueType) {}

    std::string toStr();
};

class TypeSystem {
   private:
    static IntType commonBool;
    static IntType commonInt;
    static IntType commonConstInt;
    static FloatType commonFloat;
    static FloatType commonConstFloat;
    static VoidType commonVoid;

   public:
    static Type* boolType;
    static Type* intType;
    static Type* constintType;
    static Type* floatType;
    static Type* constfloatType;
    static Type* voidType;
};

#endif
