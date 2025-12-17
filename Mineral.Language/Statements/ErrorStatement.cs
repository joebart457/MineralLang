using Mineral.Language.Expressions;


namespace Mineral.Language.Statements;

public class ErrorStatement: StatementBase
{
    public ExpressionBase ErrorToReturn { get; set; }

    public ErrorStatement(ExpressionBase errorToReturn)
    {
        ErrorToReturn = errorToReturn;
    }
}
