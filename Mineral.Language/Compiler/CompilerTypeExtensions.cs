using Mineral.Language.Expressions;
using Mineral.Language.StaticAnalysis;
using System.Linq.Expressions;
using Tokenizer.Core.Models;

namespace Mineral.Language.Compiler;

public static class CompilerTypeExtensions
{
    public static int GetStackSize(this ConcreteType concreteType) => concreteType is StructType s? GetSizeOfStruct(s) : 8;
    private static int GetSizeOfStruct(StructType structType)
    {
        int accum = 0;
        foreach (var member in structType.Members)
            accum += GetStackSize(member.FieldType);
        return accum;
    }

    private static int GetActualSize(this ConcreteType concreteType) => concreteType is StructType s ? GetActualSizeOfStruct(s) : GetActualSizeHelper(concreteType);
    private static int GetActualSizeOfStruct(StructType structType)
    {
        int accum = 0;
        foreach (var member in structType.Members)
            accum += GetActualSize(member.FieldType);
        return accum;
    }

    private static int GetActualSizeHelper(this ConcreteType concreteType)
    {
        if (concreteType.IsBuiltin) return (concreteType.BuiltinType) switch
        {
            BuiltinType.Float => 4,
            BuiltinType.Byte => 1,
            _ => 8,
        };
        if (concreteType is StructType s) throw new InvalidOperationException("nested structs are not allowed");
        return 8;
    }


    public static bool TryGetMemberOffset(this StructType structType, Token memberName, out int offset)
    {
        offset = 0;
        int accum = 0;
        foreach(var member in  structType.Members)
        {
            if (member.Name.Lexeme == memberName.Lexeme)
            {
                offset = accum;
                return true;
            }
            accum += member.FieldType.GetStackSize();
        }
        return false;    
    }


    public static bool TryGetMemberOffsetFromStructOrReference(this ConcreteType concreteType, Token memberName, out bool isReferenceType, out int offset)
    {
        isReferenceType = false;
        offset = 0;

        if (concreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType structType)
        {
            isReferenceType = true;
            foreach (var member in structType.Members)
            {
                if (member.Name.Lexeme == memberName.Lexeme)
                    return true;
                offset += member.FieldType.GetActualSize();
            }
        }
        else if (concreteType is StructType stackAllocatedStruct)
        {
            foreach (var member in stackAllocatedStruct.Members)
            {
                if (member.Name.Lexeme == memberName.Lexeme)
                    return true;
                offset += member.FieldType.GetStackSize();
            }
        }
        return false;
    }


    public static bool IsIntegerType(this ExpressionBase expression)
    {
        return expression.ConcreteType.IsEqualTo(NativeTypes.Int);
    }

    public static bool IsFloat32(this ExpressionBase expression)
    {
        return expression.ConcreteType.IsEqualTo(NativeTypes.Float32);
    }

    public static bool IsFloat64(this ExpressionBase expression)
    {
        return expression.ConcreteType.IsEqualTo(NativeTypes.Float64);
    }

}
