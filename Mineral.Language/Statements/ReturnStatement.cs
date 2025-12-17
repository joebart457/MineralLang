using Mineral.Language.Expressions;

namespace Mineral.Language.Statements;

public class ReturnStatement: StatementBase
{
    public ExpressionBase ValueToReturn { get; set; }

    public ReturnStatement(ExpressionBase valueToReturn)
    {
        ValueToReturn = valueToReturn;
    }
}
