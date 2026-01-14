
using Tokenizer.Core;
using Tokenizer.Core.Constants;
using Tokenizer.Core.Models;

namespace Mineral.Language.Parser;

public static class TokenTypes
{
    public const string Assignment = "Assignment";
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
    public const string DerefAssignment = "DerefAssignment";
    public const string Cast = "Cast";

    // Operators
    public const string Addition = "Addition";
    public const string Subtraction = "Subtraction";
    public const string Multiplication = "Multiplication";
    public const string Division = "Division";
    public const string Modulus = "Modulus";
    public const string Equality = "Equality";
    public const string NotEqual = "NotEqual";
    public const string GreaterThan = "GreaterThan";
    public const string LessThan = "LessThan";
    public const string GreaterThanOrEqual = "GreaterThanOrEqual";
    public const string LessThanOrEqual = "LessThanOrEqual";
    public const string BitwiseAnd = "BitwiseAnd";
    public const string BitwiseOr = "BitwiseOr";
    public const string BitwiseXor = "BitwiseXor";

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
    public const string Else = "Else";
    public const string While = "While";
    public const string Deref = "Deref";
    public const string Break = "Break";
    public const string Continue = "Continue";

    public const string Null = "Null";
    public const string Hex = "Hex";

    // Convenience
    public const string Word = BuiltinTokenTypes.Word;
    public const string String = BuiltinTokenTypes.String;
    public const string Integer = BuiltinTokenTypes.Integer;
    public const string Float = BuiltinTokenTypes.Float;
    public const string Double = BuiltinTokenTypes.Double;
    public const string EoLComment = BuiltinTokenTypes.EndOfLineComment;
    public const string MultiLineComment = BuiltinTokenTypes.MultiLineComment;

    // Enclosed
    public const string ImportPath = "ImportPath";
    public const string WString = "WString";
    public const string Char = "Char";
    public const string WChar = "WChar";
}


public static class TokenizerFactory
{
    public static TextTokenizer Create()
    {
        var settings = TokenizerSettings.Default;
        var rules = new List<TokenizerRule>()
        {
            new(TokenTypes.Assignment, "="),
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
            new(TokenTypes.DerefAssignment, ":="),

            // Operators
            new(TokenTypes.Addition, "+"),
            new(TokenTypes.Subtraction, "-"),
            new(TokenTypes.Multiplication, "*"),
            new(TokenTypes.Division, "/"),
            new(TokenTypes.Modulus, "%"),
            new(TokenTypes.Equality, "=="),
            new(TokenTypes.NotEqual, "!="),
            new(TokenTypes.LessThan, "<"),
            new(TokenTypes.LessThanOrEqual, "<="),
            new(TokenTypes.GreaterThan, ">"),
            new(TokenTypes.GreaterThanOrEqual, ">="),
            new(TokenTypes.BitwiseAnd, "&"),
            new(TokenTypes.BitwiseOr, "|"),
            new(TokenTypes.BitwiseXor, "^"),


            new(TokenTypes.ReferenceType, "ref"),

            new(TokenTypes.Module, "module"),
            new(TokenTypes.Import, "import"),
            new(TokenTypes.Type, "type"),
            new(TokenTypes.Function, "func"),
            new(TokenTypes.Public, "pub"),
            new(TokenTypes.Error, "error"),
            new(TokenTypes.Return, "return"),
            new(TokenTypes.StackAllocate, "stackalloc"),
            new(TokenTypes.Else, "else"),
            new(TokenTypes.While, "while"),
            new(TokenTypes.Deref, "deref"),
            new(TokenTypes.Break, "Break"),
            new(TokenTypes.Continue, "Continue"),

            new(TokenTypes.Null, "null"),
            new(TokenTypes.Hex, "0x"),
            new(TokenTypes.EoLComment, "//"),

            new(TokenTypes.String, "\"", enclosingLeft: "\"", enclosingRight: "\""),
            new(TokenTypes.WString, "W\"", enclosingLeft: "\"", enclosingRight: "\""),
            new(TokenTypes.WString, "w\"", enclosingLeft: "\"", enclosingRight: "\""),
            new(TokenTypes.Char, "'", enclosingLeft: "'", enclosingRight: "'"),
            new(TokenTypes.WChar, "W'", enclosingLeft: "'", enclosingRight: "'"),
            new(TokenTypes.WChar, "w'", enclosingLeft: "'", enclosingRight: "'"),
            new(TokenTypes.ImportPath, "`", enclosingLeft: "`", enclosingRight: "`"),
            new(TokenTypes.MultiLineComment, "/*", enclosingLeft: "/*", enclosingRight: "*/"),
        };
        settings.AllowNegatives = true;
        return new(rules, settings);
    }
}