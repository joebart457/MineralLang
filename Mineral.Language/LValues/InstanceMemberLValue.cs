using Mineral.Language.Expressions;
using Tokenizer.Core.Models;

namespace Mineral.Language.LValues;

public class InstanceMemberLValue: LValue
{
    public InstanceMemberLValue(LValue instance, Token member)
    {
        Instance = instance;
        Member = member;
    }

    public LValue Instance { get; set; }
    public Token Member { get; set; }
}
