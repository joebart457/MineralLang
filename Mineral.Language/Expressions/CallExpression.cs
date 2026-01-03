using Mineral.Language.StaticAnalysis;

namespace Mineral.Language.Expressions;

public class CallExpression: OperableExpresson
{
    public CallExpression(ExpressionBase callee, List<ExpressionBase> arguments)
    {
        Callee = callee;
        Arguments = arguments;
    }

    public ExpressionBase Callee { get; set; }
    public List<ExpressionBase> Arguments { get; set; }

    // A CallExpression will have a FunctionContext after static analysis
    // when it is a direct function call (and not e.g. a delegate invocation)
    public FunctionContext? FunctionContext { get; set; }
}
