using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Expressions;

public class IdentifierExpression: ExpressionBase
{
    public Token Symbol { get; set; }

    public IdentifierExpression(Token symbol)
    {
        Symbol = symbol;
    }


    // An IdentifierExpression will have a FunctionContext after static analysis
    // if it represents a function pointer to a directly declared function with only one overload
    public FunctionContext? FunctionContext { get; set; }
}
