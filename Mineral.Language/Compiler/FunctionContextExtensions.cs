using Mineral.Language.StaticAnalysis;
using System.Text;

namespace Mineral.Language.Compiler;

public static class FunctionContextExtensions
{
    public static string GetFunctionSignature(this FunctionContext functionContext)
    {
        var sb = new StringBuilder();
        sb.Append(functionContext.Module.ModuleName);
        sb.Append(".");
        sb.Append(functionContext.FunctionName.Lexeme);
        sb.Append("@");
        foreach(var kv in functionContext.Parameters)
        {
            sb.Append(kv.Value);
            sb.Append("@");
        }
        sb.Append(functionContext.ReturnType);
        return sb.ToString();
    }
}
