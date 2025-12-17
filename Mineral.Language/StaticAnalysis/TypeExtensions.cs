
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
                var toParamType = callableType.Parameters[i].ParmaterType;
                var fromParamType = fromCallableType.Parameters[i].ParmaterType;
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

        if (to.BuiltinType == BuiltinType.Void) return false; // void types are not directly assignable
        return to.BuiltinType == from.BuiltinType;
    }

    public static bool IsEqualTo(this ConcreteType to, ConcreteType from)
    {
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
                var toParamType = callableType.Parameters[i].ParmaterType;
                var fromParamType = fromCallableType.Parameters[i].ParmaterType;
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
}