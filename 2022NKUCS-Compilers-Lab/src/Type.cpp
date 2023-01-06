#include "Type.h"

IntType TypeSystem::commonBool = IntType(1);
IntType TypeSystem::commonInt = IntType(32);
IntType TypeSystem::commonConstInt = IntType(32, true);
FloatType TypeSystem::commonFloat = FloatType();
FloatType TypeSystem::commonConstFloat = FloatType(true);
VoidType TypeSystem::commonVoid = VoidType();

Type* TypeSystem::boolType = &commonBool;
Type* TypeSystem::intType = &commonInt;
Type* TypeSystem::constintType = &commonConstInt;
Type* TypeSystem::floatType = &commonFloat;
Type* TypeSystem::constfloatType = &commonConstFloat;
Type* TypeSystem::voidType = &commonVoid;

std::string IntType::toStr() {
    return "i" + std::to_string(size);
}

std::string FloatType::toStr() {
    return "float";
}

void ArrayType::pushDim(int num) {
    size *= num;
    dim.push_back(num);
}

std::string ArrayType::toStr() {
    // not sure
    //*
    std::string str = "";
    if (dim[0] == -1) {
        for (uint32_t i = 1; i < dim.size(); i++)
            str += "[" + std::to_string(dim[i]) + " x ";
        str += elementType->toStr();
        str += std::string(dim.size() - 1, ']');
        str += "*";
    } else {
        for (uint32_t i = 0; i < dim.size(); i++)
            str += "[" + std::to_string(dim[i]) + " x ";
        str += elementType->toStr();
        str += std::string(dim.size(), ']');
        //std::cout<<str<<"\n";
    }
    return str;
    //*/
    //return elementType->toStr() + "ARRAY";
}

std::string VoidType::toStr() {
    return "void";
}

std::string FunctionType::toStr() {
    std::string str;
    str = returnType->toStr() + "(";
    if (paramsType.size()) {
        str += paramsType[0]->toStr();
    }
    for (uint32_t i = 1; i < paramsType.size(); i++) {
        str += ", " + paramsType[i]->toStr();
    }
    str += ")";
    return str;
}

std::string PointerType::toStr() {
    return valueType->toStr() + "*";
}
