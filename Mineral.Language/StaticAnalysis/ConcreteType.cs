using Tokenizer.Core.Models;

namespace Mineral.Language.StaticAnalysis;

public class ConcreteType
{
    public BuiltinType BuiltinType { get; set; }

    public ConcreteType(BuiltinType builtinType)
    {
        BuiltinType = builtinType;
    }

    public override string ToString()
    {
        return BuiltinType.ToString().ToLower();
    }
}

public class ReferenceType: ConcreteType
{
    public ConcreteType ReferencedType { get; set; }

    public ReferenceType(ConcreteType referencedType): base(BuiltinType.Reference)
    {
        ReferencedType = referencedType;
    }

    public override string ToString()
    {
        return $"ref[{ReferencedType}]";
    }
}

public class StructType: ConcreteType
{
    public Token TypeName { get; set; }
    public List<StructTypeField> Members { get; set; }

    public StructType(Token typeName, List<StructTypeField> members) :base(BuiltinType.Struct)
    {
        TypeName = typeName;
        Members = members;
    }

    public StructType(Token typeName) : base(BuiltinType.Struct)
    {
        TypeName = typeName;
        Members = new();
    }

    public override string ToString()
    {
        return TypeName.Lexeme;
    }
}

public class StructTypeField
{
    public StructTypeField(Token name, ConcreteType fieldType)
    {
        Name = name;
        FieldType = fieldType;
    }

    public Token Name { get; set; }
    public ConcreteType FieldType { get; set; }
}

public class CallableType : ConcreteType
{
    public CallableType(Token? functionName, ConcreteType returnType, List<FunctionParameter> parameters, bool isErrorable) : base(BuiltinType.Callable)
    {
        FunctionName = functionName;
        ReturnType = returnType;
        Parameters = parameters;
        IsErrorable = isErrorable;
    }

    public Token? FunctionName { get; set; }
    public ConcreteType ReturnType { get; set; }
    public List<FunctionParameter> Parameters { get; set; }
    public bool IsErrorable { get; set; }

    public override string ToString()
    {
        var types = Parameters.Select(x => x.ParameterType).ToList();
        types.Add(ReturnType);
        return $"func[{string.Join(",", types.Select(x => x.ToString()))}]";
    }
}

public class FunctionParameter
{
    public FunctionParameter(Token parameterName, ConcreteType parmaterType)
    {
        ParameterName = parameterName;
        ParameterType = parmaterType;
    }

    public Token ParameterName { get; set; }
    public ConcreteType ParameterType { get; set; }
}


public class NullPointerType : ConcreteType
{
    public NullPointerType() : base(BuiltinType.Void)
    {
    }
}