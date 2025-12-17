using Tokenizer.Core.Models;

namespace Mineral.Language.LValues;

public class IdentifierLValue: LValue
{
    public Token VariableName { get; set; }

    public IdentifierLValue(Token variableName)
    {
        VariableName = variableName;
    }
}
