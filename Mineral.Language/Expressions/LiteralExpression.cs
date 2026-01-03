
namespace Mineral.Language.Expressions;

public class LiteralExpression: OperableExpresson
{
    public object? Value { get; set; }

    public LiteralExpression(object? value)
    {
        Value = value;
    }
}
