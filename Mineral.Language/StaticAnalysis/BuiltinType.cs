
namespace Mineral.Language.StaticAnalysis;

public enum BuiltinType
{
    Byte,
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

    public static readonly ConcreteType Int = new ConcreteType(BuiltinType.Int);
    public static readonly ConcreteType Float = new ConcreteType(BuiltinType.Float);
    public static readonly ConcreteType Void = new ConcreteType(BuiltinType.Void);
    public static readonly ConcreteType Error = new ConcreteType(BuiltinType.String);
}