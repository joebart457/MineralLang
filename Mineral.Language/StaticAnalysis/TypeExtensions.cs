
using Mineral.Language.Expressions;
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


    public static bool IsConditionalTestable(this ConcreteType type)
    {
        if (type is ReferenceType) return true;
        if (type.GetType() == typeof(ConcreteType) 
            && (type.BuiltinType == BuiltinType.Int || type.BuiltinType == BuiltinType.String)) return true;
        return false;
    }

    public static bool IsErrorType(this ConcreteType type)
    {
        return type.GetType() == typeof(ConcreteType) && type.BuiltinType == BuiltinType.String;
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
}