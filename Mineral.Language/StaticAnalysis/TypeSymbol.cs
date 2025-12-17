using Tokenizer.Core.Models;

namespace Mineral.Language.StaticAnalysis;

public class TypeSymbol
{
    public TypeSymbol(Token? moduleName, Token typeName, List<TypeSymbol> genericTypeArguments, bool isErrorable)
    {
        ModuleName = moduleName;
        TypeName = typeName;
        GenericTypeArguments = genericTypeArguments;
        IsErrorable = isErrorable;
    }
    public Token? ModuleName { get; set; }
    public Token TypeName { get; set; }
    public List<TypeSymbol> GenericTypeArguments { get; set; }
    public bool IsErrorable { get; set; }

    public bool HasGenericTypeArguments => GenericTypeArguments.Count > 0;

    public Location Start { get; set; }
    public Location End { get; set; }
}
