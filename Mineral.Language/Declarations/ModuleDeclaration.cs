using Tokenizer.Core.Models;

namespace Mineral.Language.Declarations;

public class ModuleDeclaration: DeclarationBase
{
    public ModuleDeclaration(Token moduleName)
    {
        ModuleName = moduleName;
    }
    public Token ModuleName { get; set; }

}
