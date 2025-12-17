using System;
using System.Collections.Generic;
using System.Text;

namespace Mineral.Language.Expressions;

public class LiteralExpression: ExpressionBase
{
    public object? Value { get; set; }

    public LiteralExpression(object? value)
    {
        Value = value;
    }
}
