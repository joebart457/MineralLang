using Mineral.Language.Declarations;
using Tokenizer.Core.Exceptions;
using Tokenizer.Core.Models;

namespace Mineral.Language.Parser;

public class FileParseResult
{
    public FileParseResult()
    {
        Errors = new();
        ModuleName = null;
        Imports = null;
        DeclaredTypes = new();
        DeclaredFunctions = new();
    }

    public List<ParsingException> Errors { get; set; }
    public Token? ModuleName { get; set;  }
    public ImportDeclaration? Imports { get; set; }
    public List<TypeDeclaration> DeclaredTypes { get; set; }
    public List<FunctionDeclaration> DeclaredFunctions { get; set;  }
}
