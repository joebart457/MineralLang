using Mineral.Language.Parser;
using Tokenizer.Core.Models;

namespace Mineral.Language.Declarations;

public class DeclarationBase
{
    public Location Start { get; set; } = Location.Zero;
    public Location End { get; set; } = Location.Zero;

    public static MissingDeclaration Missing => new MissingDeclaration();
}
