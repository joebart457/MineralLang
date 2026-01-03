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
}
