
namespace Mineral.Language.StaticAnalysis;

public enum BuiltinType
{
    Byte,
    Int,
    Float32,
    Float64,
    String,
    Reference,
    Struct,
    Void,
    Callable,
}

public static class NativeTypes
{

    public static readonly ConcreteType Int = new ConcreteType(BuiltinType.Int);
    public static readonly ConcreteType String = new ConcreteType(BuiltinType.String);
    public static readonly ConcreteType Float32 = new ConcreteType(BuiltinType.Float32);
    public static readonly ConcreteType Float64 = new ConcreteType(BuiltinType.Float64);
    public static readonly ConcreteType Void = new ConcreteType(BuiltinType.Void);
    public static readonly ConcreteType Error = new ConcreteType(BuiltinType.String);
}