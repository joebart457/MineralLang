using Mineral.Language.Expressions;
using Mineral.Language.LValues;

namespace Mineral.Language.Statements;

public class AssignmentStatement: StatementBase
{
    public AssignmentStatement(LValue assignmentTarget, LValue? errorTarget, ExpressionBase value)
    {
        AssignmentTarget = assignmentTarget;
        ErrorTarget = errorTarget;
        Value = value;
    }

    public LValue AssignmentTarget { get; set; }
    public LValue? ErrorTarget { get; set; }
    public ExpressionBase Value { get; set; }
}
