using Mineral.Language.LValues;

namespace Mineral.Language.Statements;

public class ConditionalStatement: StatementBase
{
    public ConditionalStatement(LValue conditionalTarget, List<StatementBase> thenBlock)
    {
        ConditionalTarget = conditionalTarget;
        ThenBlock = thenBlock;
    }

    public LValue ConditionalTarget { get; set; }
    public List<StatementBase> ThenBlock { get; set; }
}