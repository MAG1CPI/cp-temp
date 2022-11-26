#ifndef __TYPE_H__
#define __TYPE_H__

#include <sstream>
#include <string>
#include <vector>

class Type {
   private:
    int kind;

   protected:
    enum { INT,
           FLOAT,
           VOID,
           FUNC,
           PTR };

   public:
    Type(int kind)
        : kind(kind){};
    virtual ~Type(){};
    virtual std::string toStr() = 0;

    bool isInt() const { return kind == INT; };
    bool isFloat() const { return kind == FLOAT; };
    bool isVoid() const { return kind == VOID; };
    bool isFunc() const { return kind == FUNC; };
    bool isPTR() const { return kind == PTR; };
};

class IntType : public Type {
   private:
    int size;

   public:
    IntType(int size)
        : Type(Type::INT), size(size){};
    std::string toStr();
};

class ConstIntType : public IntType {
   public:
    ConstIntType(int size)
        : IntType(size){};
    std::string toStr();
};

class FloatType : public Type {
   private:
    int size;

   public:
    FloatType(int size)
        : Type(Type::FLOAT), size(size){};
    std::string toStr();
};

class ConstFloatType : public FloatType {
   public:
    ConstFloatType(int size)
        : FloatType(size){};
    std::string toStr();
};

class VoidType : public Type {
   public:
    VoidType()
        : Type(Type::VOID){};
    std::string toStr();
};

class FunctionType : public Type {
   private:
    Type* returnType;
    std::vector<Type*> paramsType;

   public:
    FunctionType(Type* returnType, std::vector<Type*> paramsType)
        : Type(Type::FUNC), returnType(returnType), paramsType(paramsType){};
    Type* getRetType() { return returnType; };
    std::string toStr();
};

class PointerType : public Type {
   private:
    Type* valueType;

   public:
    PointerType(Type* valueType)
        : Type(Type::PTR), valueType(valueType){};
    std::string toStr();
};

class TypeSystem {
   private:
    static IntType commonInt;
    static IntType commonBool;
    static ConstIntType commonConstInt;
    static FloatType commonFloat;
    static ConstFloatType commonConstFloat;
    static VoidType commonVoid;

   public:
    static Type* intType;
    static Type* boolType;
    static Type* constIntType;
    static Type floatType;
    static Type constFloatType;
    static Type* voidType;
};

#endif
