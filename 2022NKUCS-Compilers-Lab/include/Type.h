#ifndef __TYPE_H__
#define __TYPE_H__

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
   protected:
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
   public:
    FloatType()
        : Type(Type::FLOAT){};
    std::string toStr();
};

class ConstFloatType : public FloatType {
   public:
    ConstFloatType(){};
    std::string toStr();
};

class VoidType : public Type {
   public:
    VoidType()
        : Type(Type::VOID){};
    std::string toStr();
};

class FunctionType : public Type {
   protected:
    Type* returnType;
    std::vector<Type*> paramsType;

   public:
    FunctionType(Type* returnType, std::vector<Type*> paramsType)
        : Type(Type::FUNC), returnType(returnType), paramsType(paramsType) {}

    std::vector<Type*> getParamType() { return paramsType; };
    Type* getRetType() { return returnType; };
    std::string paramTypeToStr();
    std::string toStr();
};

class PointerType : public Type {
   protected:
    Type* valueType;

   public:
    PointerType(Type* valueType)
        : Type(Type::PTR), valueType(valueType){};
    std::string toStr();
};

class TypeSystem {
   private:
    static IntType commonBool;
    static IntType commonInt;
    static ConstIntType commonConstInt;
    static FloatType commonFloat;
    static ConstFloatType commonConstFloat;
    static VoidType commonVoid;
    // FunctionType
    // PointerType
   public:
    static Type* boolType;
    static Type* intType;
    static Type* constintType;
    static Type* floatType;
    static Type* constfloatType;
    static Type* voidType;
};

#endif
