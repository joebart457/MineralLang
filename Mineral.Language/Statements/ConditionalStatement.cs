using Mineral.Language.LValues;

namespace Mineral.Language.Statements;

public class ConditionalStatement: StatementBase
{
    public ConditionalStatement(LValue conditionalTarget, StatementBase thenBlock)
    {
        ConditionalTarget = conditionalTarget;
        ThenBlock = thenBlock;
    }

    public LValue ConditionalTarget { get; set; }
    public StatementBase ThenBlock { get; set; }
}