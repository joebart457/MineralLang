using Mineral.Language.StaticAnalysis;
using System;
using System.Collections.Generic;
using System.Text;

namespace Mineral.Language.Expressions;

public class StackAllocateExpression: ExpressionBase // Not an operable expression
{
    public StackAllocateExpression(TypeSymbol typeToAllocate)
    {
        TypeToAllocate = typeToAllocate;
    }

    public TypeSymbol TypeToAllocate { get; set; }

}
