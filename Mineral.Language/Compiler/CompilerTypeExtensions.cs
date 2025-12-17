using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Compiler;

public static class CompilerTypeExtensions
{
    public static int GetStackSize(this ConcreteType concreteType) => 4;

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
}
