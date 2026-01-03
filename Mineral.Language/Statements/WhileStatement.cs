using Mineral.Language.Expressions;
using System;
using System.Collections.Generic;
using System.Text;

namespace Mineral.Language.Statements;

public class WhileStatement : StatementBase
{
    public WhileStatement(ConditionalExpression conditionalTarget, List<StatementBase> thenBlock)
    {
        ConditionalTarget = conditionalTarget;
        ThenBlock = thenBlock;
    }

    public ConditionalExpression ConditionalTarget { get; set; }
    public List<StatementBase> ThenBlock { get; set; }
}