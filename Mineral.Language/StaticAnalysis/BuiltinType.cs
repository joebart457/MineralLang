
using Mineral.Language.Parser;
using Tokenizer.Core.Models;

namespace Mineral.Language.StaticAnalysis;

public enum BuiltinType
{
    Byte,
    Int,
    Float32,
    Float64,
    Reference,
    Struct,
    Void,
    Callable,
}

public static class NativeTypes
{
    public static readonly ConcreteType Byte = new ConcreteType(BuiltinType.Byte);
    public static readonly ConcreteType CString = new ReferenceType(NativeTypes.Byte);


    public static readonly ConcreteType Int = new ConcreteType(BuiltinType.Int);
    public static readonly ConcreteType String = new ReferenceType(
        new StructType(new Token(TokenTypes.Word, "BoxedString", Location.Zero, Location.Zero),
        [
            new(new Token(TokenTypes.Word, "length", Location.Zero, Location.Zero), NativeTypes.Int),
            new(new Token(TokenTypes.Word, "cstr", Location.Zero, Location.Zero), NativeTypes.CString),
        ]));
    public static readonly ConcreteType WString = new ReferenceType(
        new StructType(new Token(TokenTypes.Word, "BoxedWString", Location.Zero, Location.Zero),
        [
            new(new Token(TokenTypes.Word, "length", Location.Zero, Location.Zero), NativeTypes.Int),
            new(new Token(TokenTypes.Word, "cstr", Location.Zero, Location.Zero), NativeTypes.CString),
        ]));

    public static readonly ConcreteType Float32 = new ConcreteType(BuiltinType.Float32);
    public static readonly ConcreteType Float64 = new ConcreteType(BuiltinType.Float64);
    public static readonly ConcreteType Void = new ConcreteType(BuiltinType.Void);
    public static readonly ConcreteType Error = NativeTypes.String;
    

}