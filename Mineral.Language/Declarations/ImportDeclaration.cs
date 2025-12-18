using Tokenizer.Core.Models;

namespace Mineral.Language.Declarations;

public class ImportDeclaration : DeclarationBase
{
    public ImportDeclaration(Token token, List<Token> imports)
    {
        Token = token;
        Imports = imports;
    }
    public Token Token { get; set; } // Token used only for error reporting
    public List<Token> Imports { get; set; }
}
