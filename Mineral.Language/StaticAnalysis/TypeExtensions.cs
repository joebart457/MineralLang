
using Mineral.Language.Compiler;
using Mineral.Language.Expressions;
using System;
using Tokenizer.Core.Models;

namespace Mineral.Language.StaticAnalysis;

internal static class TypeExtensions
{
    public static bool IsAssignableFrom(this ConcreteType to, ConcreteType from)
    {
        if (to is StructType structType) return false; // Structs are not directly assignable to

        if (to is CallableType callableType)
        {
            if (from is not CallableType fromCallableType)
                return false;
            if (!callableType.ReturnType.IsEqualTo(fromCallableType.ReturnType))
                return false;
            if (callableType.Parameters.Count != fromCallableType.Parameters.Count)
                return false;
            for (int i = 0; i < callableType.Parameters.Count; i++)
            {
                var toParamType = callableType.Parameters[i].ParameterType;
                var fromParamType = fromCallableType.Parameters[i].ParameterType;
                if (!toParamType.IsEqualTo(fromParamType))
                    return false;
                if (callableType.Parameters[i].ParameterName.Lexeme != fromCallableType.Parameters[i].ParameterName.Lexeme)
                    return false; // TODO: For now, parameter names must match
            }
            return true;
        }

        if (to is ReferenceType toRefType)
        {
            if (from is NullPointerType) return true; // null is always assignable to a reference
            if (from is not ReferenceType fromRefType)
                return false;
            return toRefType.ReferencedType.IsEqualTo(fromRefType.ReferencedType);
        }

        if (to.GetType() != from.GetType())
            return false; // Ensure both are of the same derived type

        if (to.BuiltinType == BuiltinType.Void) return false; // void types are not directly assignable
        return to.BuiltinType == from.BuiltinType;
    }

    public static bool IsEqualTo(this ConcreteType to, ConcreteType from)
    {
        if (to is NullPointerType || from is NullPointerType) return false; // null type is never equal except in special cases
        if (to is StructType structType)
        {
            if (from is not StructType fromStructType)
                return false;
            if (structType.Members.Count != fromStructType.Members.Count) return false;
            for (int i = 0; i < structType.Members.Count; i++)
            {
                var toMember = structType.Members[i];
                var fromMember = fromStructType.Members[i];
                if (!toMember.FieldType.IsEqualTo(fromMember.FieldType))
                    return false;
                if (toMember.Name.Lexeme != fromMember.Name.Lexeme)
                    return false;
            }
            return true;
        }

        if (to is CallableType callableType)
        {
            if (from is not CallableType fromCallableType)
                return false;
            if (!callableType.ReturnType.IsEqualTo(fromCallableType.ReturnType))
                return false;
            if (callableType.Parameters.Count != fromCallableType.Parameters.Count)
                return false;
            for (int i = 0; i < callableType.Parameters.Count; i++)
            {
                var toParamType = callableType.Parameters[i].ParameterType;
                var fromParamType = fromCallableType.Parameters[i].ParameterType;
                if (!toParamType.IsEqualTo(fromParamType))
                    return false;
                if (callableType.Parameters[i].ParameterName.Lexeme != fromCallableType.Parameters[i].ParameterName.Lexeme)
                    return false; // TODO: For now, parameter names must match
            }
            return true;
        }

        if (to is ReferenceType toRefType)
        {
            if (from is not ReferenceType fromRefType)
                return false;
            return toRefType.ReferencedType.IsEqualTo(fromRefType.ReferencedType);
        }

        if (to.GetType() != from.GetType())
            return false; // Ensure both are of the same derived type

        return to.BuiltinType == from.BuiltinType;
    }


    public static bool IsCastableTo(this ConcreteType from, ConcreteType targetType)
    {
        // essentially a reinterpret_cast except for numeric types

        if (from.IsEqualTo(NativeTypes.Int) && targetType.IsEqualTo(NativeTypes.Int32)) return true;
        if (from.IsEqualTo(NativeTypes.Int) && targetType.IsEqualTo(NativeTypes.Int16)) return true;
        if (from.IsEqualTo(NativeTypes.Int) && targetType.IsEqualTo(NativeTypes.Byte)) return true;
        if (from.IsEqualTo(NativeTypes.Int16) && targetType.IsEqualTo(NativeTypes.Int)) return true;
        if (from.IsEqualTo(NativeTypes.Int16) && targetType.IsEqualTo(NativeTypes.Int32)) return true;
        if (from.IsEqualTo(NativeTypes.Int16) && targetType.IsEqualTo(NativeTypes.Byte)) return true;
        if (from.IsEqualTo(NativeTypes.Byte) && targetType.IsEqualTo(NativeTypes.Int)) return true;
        if (from.IsEqualTo(NativeTypes.Byte) && targetType.IsEqualTo(NativeTypes.Int16)) return true;
        if (from.IsEqualTo(NativeTypes.Byte) && targetType.IsEqualTo(NativeTypes.Int32)) return true;

        if (from.IsEqualTo(NativeTypes.Float64) && targetType.IsEqualTo(NativeTypes.Int)) return true;
        if (from.IsEqualTo(NativeTypes.Float64) && targetType.IsEqualTo(NativeTypes.Float32)) return true;

        if (from.IsEqualTo(NativeTypes.Float32) && targetType.IsEqualTo(NativeTypes.Int32)) return true;
        if (from.IsEqualTo(NativeTypes.Float32) && targetType.IsEqualTo(NativeTypes.Float64)) return true;
        return from.GetActualSize() == targetType.GetActualSize();
    }

    public static bool IsConditionalTestable(this ConcreteType type)
    {
        if (type is ReferenceType) return true;
        if (type.IsEqualTo(NativeTypes.Int) || type.IsEqualTo(NativeTypes.Int32) || type.IsEqualTo(NativeTypes.Int16)) return true;
        return false;
    }

    public static bool IsErrorType(this ConcreteType type)
    {
        return type.IsEqualTo(NativeTypes.Error);
    }

    public static bool IsVoidType(this ConcreteType type)
    {
        return type.GetType() == typeof(ConcreteType) && type.BuiltinType == BuiltinType.Void;
    }

    public static bool IsValidReturnType(this ConcreteType type)
    {
        return type is not StructType && type is not NullPointerType;
    }

    public static bool IsValidParameterType(this ConcreteType type)
    {
        return type is not StructType && type is not NullPointerType;
    }

    public static bool IsValidMemberType(this ConcreteType type)
    {
        return type is not StructType && type is not NullPointerType;
    }


    public static bool IsStructReferenceOrStructType(this ConcreteType concreteType)
    {
        if (concreteType is StructType) return true;
        if (concreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType) return true;
        return false;
    }

    public static bool TryFindMember(this ConcreteType concreteType, ModuleErrors errors, Token memberName, out StructTypeField? field)
    {
        field = null;
        StructType? structType = null;
        if (concreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType potentialStructType)
            structType = potentialStructType;
        if (concreteType is StructType potentialStructType2) 
            structType = potentialStructType2;
        if (structType != null)
        {
            var foundMember = structType.Members.Find(m => m.Name.Lexeme == memberName.Lexeme);
            if (foundMember == null)
            {
                errors.Add(memberName, $"Struct type '{structType}' does not contain member '{memberName.Lexeme}'");
                return false;
            }
            field = foundMember;
            return true;
        }
        errors.Add(memberName, $"Cannot access member '{memberName.Lexeme}' of non-struct type '{concreteType}'");
        return false;
    }

    public static bool TryFindMember(this ConcreteType concreteType, Token memberName, out StructTypeField? field)
    {
        field = null;
        StructType? structType = null;
        if (concreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType potentialStructType)
            structType = potentialStructType;
        if (concreteType is StructType potentialStructType2)
            structType = potentialStructType2;
        if (structType != null)
        {
            var foundMember = structType.Members.Find(m => m.Name.Lexeme == memberName.Lexeme);
            if (foundMember == null)
            {
                return false;
            }
            field = foundMember;
            return true;
        }
        return false;
    }


    public static bool TryGetResultingTypeFromOperation(this ConcreteType left, ConcreteType right, OperatorType op, out ConcreteType resultType)
    {
        resultType = NativeTypes.Void;
        switch (op)
        {
            case OperatorType.Addition:
            case OperatorType.Subtraction:
            case OperatorType.Multiplication:
            case OperatorType.Division: 
            {
                resultType = left;
                return left.IsNumericType() && left.IsEqualTo(right);
            }
            case OperatorType.Modulus:
                {
                    resultType = NativeTypes.Int;
                    return left.IsEqualTo(NativeTypes.Int) && left.IsEqualTo(right);
                }
            case OperatorType.Equality:
            case OperatorType.NotEqual:
            {
                resultType = NativeTypes.Int;
                return left.IsAssignableFrom(right) && (left.IsConditionalTestable() || left.IsEqualTo(NativeTypes.Float32) || left.IsEqualTo(NativeTypes.Float64) || left.IsEqualTo(NativeTypes.Byte) || left.IsEqualTo(NativeTypes.Int16) || left.IsEqualTo(NativeTypes.Int32));
            }
            case OperatorType.GreaterThan:
            case OperatorType.LessThan:
            case OperatorType.GreaterThanOrEqual:
            case OperatorType.LessThanOrEqual:
            {
                resultType = NativeTypes.Int;
                return left.IsNumericType() && left.IsEqualTo(right);
            }
            default:
                return false;
        }

    }

    public static bool IsNumericType(this ConcreteType type)
    {
        return type.IsEqualTo(NativeTypes.Int) || type.IsEqualTo(NativeTypes.Float32) || type.IsEqualTo(NativeTypes.Float64);
    }
}