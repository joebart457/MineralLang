
namespace Mineral.Language.StaticAnalysis;

public enum BuiltinType
{
    Int,
    Float,
    String,
    Reference,
    Struct,
    Void,
    Callable,
}

public static class NativeTypes
{


    public static readonly ConcreteType Void = new ConcreteType(BuiltinType.Void);
    public static readonly ConcreteType Error = new ConcreteType(BuiltinType.String);
}