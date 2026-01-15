
using Mineral.Language.Expressions;
using Mineral.Language.Parser;

namespace Mineral.Language.LValues;

public class LValue: ExpressionBase
{
    public static MissingLValue Missing(ExpressionBase invalidLValue) => new MissingLValue(invalidLValue);  
}
