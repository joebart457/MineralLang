
using Tokenizer.Core;
using Tokenizer.Core.Constants;
using Tokenizer.Core.Models;

namespace Mineral.Language.Parser;

public static class TokenTypes
{
    public const string Equal = "Equal";
    public const string Semicolon = "Semicolon";
    public const string Comma = "Comma";
    public const string QuestionMark = "QuestionMark";
    public const string LParen = "LParen";
    public const string RParen = "RParen";
    public const string LBracket = "LBracket";
    public const string RBracket = "RBracket";
    public const string LCurly = "LCurly";
    public const string RCurly = "RCurly";
    public const string Exclamation = "Exclamation";
    public const string Dot = "Dot";
    public const string Discard = "Discard";

    // Native types
    public const string ReferenceType = "ReferenceType";

    // Keywords
    public const string Module = "Module";
    public const string Import = "Import";
    public const string Type = "Type";
    public const string Function = "Function";
    public const string Public = "Public";
    public const string Error = "Error";
    public const string Return = "Return";
    public const string StackAllocate = "StackAllocate";

    public const string Null = "Null";

    // Convenience
    public const string Word = BuiltinTokenTypes.Word;
    public const string String = BuiltinTokenTypes.String;
    public const string Integer = BuiltinTokenTypes.Integer;
    public const string Float = BuiltinTokenTypes.Float;
    

    // Enclosed
    public const string ImportPath = "ImportPath";
}


public static class TokenizerFactory
{
    public static TextTokenizer Create()
    {
        var settings = TokenizerSettings.Default;
        var rules = new List<TokenizerRule>()
        {
            new(TokenTypes.Equal, "="),
            new(TokenTypes.Semicolon, ";"),
            new(TokenTypes.Comma, ","),
            new(TokenTypes.QuestionMark, "?"),
            new(TokenTypes.LParen, "("),
            new(TokenTypes.RParen, ")"),
            new(TokenTypes.LBracket, "["),
            new(TokenTypes.RBracket, "]"),
            new(TokenTypes.LCurly, "{"),
            new(TokenTypes.RCurly, "}"),
            new(TokenTypes.Exclamation, "!"),
            new(TokenTypes.Dot, "."),
            new(TokenTypes.Discard, "_"),

            new(TokenTypes.ReferenceType, "ref"),

            new(TokenTypes.Module, "module"),
            new(TokenTypes.Import, "import"),
            new(TokenTypes.Type, "type"),
            new(TokenTypes.Function, "func"),
            new(TokenTypes.Public, "pub"),
            new(TokenTypes.Error, "error"),
            new(TokenTypes.Return, "return"),
            new(TokenTypes.StackAllocate, "stackalloc"),
            new(TokenTypes.Null, "null"),

            new(TokenTypes.String, "\"", enclosingLeft: "\"", enclosingRight: "\""),
            new(TokenTypes.ImportPath, "`", enclosingLeft: "`", enclosingRight: "`"),
        };
        settings.AllowNegatives = true;
        return new(rules, settings);
    }
}