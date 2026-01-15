using Mineral.Language.StaticAnalysis;

namespace Mineral.Language.Expressions;

public class SizeOfExpression: OperableExpresson
{
    public TypeSymbol TypeSymbol { get; set; }

    public SizeOfExpression(TypeSymbol typeSymbol)
    {
        TypeSymbol = typeSymbol;
    }
}
