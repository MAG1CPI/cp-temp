#include "Type.h"

IntType TypeSystem::commonBool = IntType(1);
IntType TypeSystem::commonInt = IntType(32);
ConstIntType TypeSystem::commonConstInt = ConstIntType(32);
FloatType TypeSystem::commonFloat = FloatType();
ConstFloatType TypeSystem::commonConstFloat = ConstFloatType();
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

std::string ConstIntType::toStr() {
    return "i" + std::to_string(size);
}

std::string FloatType::toStr() {
    return "float";
}

std::string ConstFloatType::toStr() {
    return "float";
}

std::string VoidType::toStr() {
    return "void";
}

std::string FunctionType::paramTypeToStr() {
    std::string str = "";
    if (paramsType.size()) {
        str += paramsType[0]->toStr();
    }
    for (uint32_t i = 1; i < paramsType.size(); i++) {
        str += ", " + paramsType[i]->toStr();
    }
    return str;
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
