using Tokenizer.Core.Models;

namespace Mineral.Language.Declarations;

public class ImportDeclaration
{
    public ImportDeclaration(List<Token> imports)
    {
        Imports = imports;
    }
    public List<Token> Imports { get; set; }

}
