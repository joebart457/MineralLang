using Mineral.Language.Expressions;
using Mineral.Language.LValues;

namespace Mineral.Language.Statements;


public class DereferenceAssignmentStatement : StatementBase
{
    public DereferenceAssignmentStatement(LValue assignmentTarget, LValue? errorTarget, ExpressionBase value)
    {
        AssignmentTarget = assignmentTarget;
        ErrorTarget = errorTarget;
        Value = value;
    }

    public LValue AssignmentTarget { get; set; }
    public LValue? ErrorTarget { get; set; }
    public ExpressionBase Value { get; set; }
}