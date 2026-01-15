using Mineral.Language.Parser;
using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Expressions;

public class ExpressionBase
{
    protected ConcreteType? _type = null;
    public CompilerMetadata Metadata { get; protected set; } = new();

    public void TagAsType(ConcreteType type)
    {
        _type = type;
    }

    public ConcreteType ConcreteType => _type ?? NativeTypes.Void;


    public Location Start { get; set; } = Location.Zero;
    public Location End { get; set; } = Location.Zero;
    public static MissingExpression Missing => new MissingExpression();
}

public class CompilerMetadata
{
    public int SU { get; set; } // Sethi-Ullman number for minimal register spills
    public bool ContainsCallOrBinary { get; set; }
    public int StackSlotsNeeded { get; set; }
    public string ContinueLabel { get; set; } = ""; // used for loops only
    public string BreakLabel { get; set; } = ""; // used for loops only

}