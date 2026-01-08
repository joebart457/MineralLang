
namespace Mineral.Language.Expressions;

public class DereferenceExpression: OperableExpresson
{
    public ExpressionBase Target { get; set; }

    public DereferenceExpression(ExpressionBase target)
    {
        Target = target;
    }
}
