using Mineral.Language.StaticAnalysis;

namespace Mineral.Language.Expressions;

public class CastExpression: OperableExpresson
{
    public CastExpression(TypeSymbol targetType, ExpressionBase value)
    {
        TargetType = targetType;
        Value = value;
    }

    public TypeSymbol TargetType { get; set; }
    public ExpressionBase Value { get; set; }
}
