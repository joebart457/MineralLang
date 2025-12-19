using System;
using System.Collections.Generic;
using System.Text;
using Tokenizer.Core.Models;

namespace Mineral.Language.Expressions;

public class ReferenceExpression: ExpressionBase
{
    public ReferenceExpression(ExpressionBase? instance, Token memberToAccess)
    {
        Instance = instance;
        MemberToAccess = memberToAccess;
    }

    public ExpressionBase? Instance { get; set; }
    public Token MemberToAccess { get; set; }
}
