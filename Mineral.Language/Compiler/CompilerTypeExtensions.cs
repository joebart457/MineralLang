using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Compiler;

public static class CompilerTypeExtensions
{
    public static int GetStackSize(this ConcreteType concreteType) => concreteType is StructType s? GetSizeOfStruct(s) : 4;
    private static int GetSizeOfStruct(StructType structType)
    {
        int accum = 0;
        foreach (var member in structType.Members)
            accum += GetStackSize(member.FieldType);
        return accum;
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

        StructType? structType = null;
        if (concreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType potentialStructType)
        {
            structType = potentialStructType;
            isReferenceType = true;
        }
        if (concreteType is StructType potentialStructType2)
            structType = potentialStructType2;
        if (structType != null)
        {
            foreach (var member in structType.Members)
            {
                if (member.Name.Lexeme == memberName.Lexeme)
                    return true;
                offset += member.FieldType.GetStackSize();
            }
        }
        return false;
    }
}
