#include "Type.h"

IntType TypeSystem::commonBool = IntType(1);
IntType TypeSystem::commonInt = IntType(32);
IntType TypeSystem::commonConstInt = IntType(32,true);
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

std::string ArrayType::toStr() {
    // not sure
    std::string str = "";
    bool is_fparam = false;
    Type* part_type = this;
    int part_len;
    // fparam
    if (getLength() == -1) {
        is_fparam = true;
        part_type = getElementType();
    }
    // iterate all dim
    while (part_type && part_type->isArray()) {
        part_len = ((ArrayType*)part_type)->getLength();
        str += "[" + std::to_string(part_len) + "]";
        part_type = ((ArrayType*)part_type)->getElementType();
    }
    // basic type
    if (!(part_type->isInt() || part_type->isFloat()))
        assert(false);
    str += part_type->toStr();
    // fparam
    if (is_fparam)
        str += "*";
    return str + part_type->toStr();
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
