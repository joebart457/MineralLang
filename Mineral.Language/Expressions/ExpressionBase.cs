using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Expressions;

public class ExpressionBase
{
    protected ConcreteType? _type = null;


    public void TagAsType(ConcreteType type)
    {
        _type = type; 
    }

    public ConcreteType ConcreteType => _type ?? NativeTypes.Void;

    public Location Start { get; set; } = Location.Zero;
    public Location End { get; set; } = Location.Zero;
}