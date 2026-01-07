using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Expressions;

public class MemberAccessExpression: OperableExpresson
{
    public MemberAccessExpression(ExpressionBase instance, Token memberToAccess)
    {
        Instance = instance;
        MemberToAccess = memberToAccess;
    }

    public ExpressionBase Instance { get; set; }
    public Token MemberToAccess { get; set; }

    // A MemberAccessExpression will have a FunctionContext after static analysis
    // when it is a reference to a function (NOT when it is a callable lvalue)
    public FunctionContext? FunctionContext { get; set; }
}
