using System;
using System.Collections.Generic;
using System.Text;

namespace Mineral.Language.Expressions;

public enum OperatorType
{
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Equality,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,

    // Bitwise
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
}

public class BinaryExpression : ConditionalExpression // BinaryExpression is not an operable expression (as a language choice)
{
    public OperableExpresson Left { get; set; }
    public OperableExpresson Right { get; set; }
    public OperatorType Operator { get; set; }
    public BinaryExpression(OperableExpresson left, OperableExpresson right, OperatorType operatorType)
    {
        Left = left;
        Right = right;
        Operator = operatorType;
    }
}