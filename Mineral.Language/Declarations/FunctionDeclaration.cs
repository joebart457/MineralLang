using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Declarations;

public class FunctionDeclaration
{
    public FunctionDeclaration(Token functionName, TypeSymbol returnType, List<FunctionDeclarationParameter> parameters, List<StatementBase> bodyStatements, bool isErrorable)
    {
        FunctionName = functionName;
        ReturnType = returnType;
        Parameters = parameters;
        BodyStatements = bodyStatements;
        IsErrorable = isErrorable;
    }

    public Token FunctionName { get; set; }
    public TypeSymbol ReturnType { get; set; }
    public List<FunctionDeclarationParameter> Parameters { get; set; }
    public List<StatementBase> BodyStatements { get; set; }
    public bool IsErrorable { get; set; }
}

public class FunctionDeclarationParameter
{
    public FunctionDeclarationParameter(Token parameterName, TypeSymbol parameterType)
    {
        ParameterName = parameterName;
        ParameterType = parameterType;
    }
    public Token ParameterName { get; set; }
    public TypeSymbol ParameterType { get; set; }

}