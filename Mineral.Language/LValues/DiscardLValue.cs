using Tokenizer.Core.Models;

namespace Mineral.Language.LValues;

public class DiscardLValue: LValue
{
    public DiscardLValue(Token discardToken)
    {
        DiscardToken = discardToken;
    }
    public Token DiscardToken { get; set; }
}
