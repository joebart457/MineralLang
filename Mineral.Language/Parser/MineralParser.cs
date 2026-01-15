using Mineral.Language.Declarations;
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Globalization;
using System.Text;
using Tokenizer.Core;
using Tokenizer.Core.Constants;
using Tokenizer.Core.Exceptions;
using Tokenizer.Core.Models;

namespace Mineral.Language.Parser;
public class MissingDeclaration: DeclarationBase
{
    public Token? Token { get; set; }

}

public class MissingLValue: LValue
{
    public ExpressionBase InvalidLValue { get; set; }

    public MissingLValue(ExpressionBase invalidLValue)
    {
        InvalidLValue = invalidLValue;
    }
}

public class MissingExpression: OperableExpresson
{
    public Token? Token { get; set; }
}

public class MineralParser: TokenParser
{
    private List<ParsingException> _errors = new();
    public List<FileParseResult> ParseWorkspace(string directory)
    {
        var results = new List<FileParseResult>();
        foreach(var file in Directory.EnumerateFiles(directory, "*.min", SearchOption.AllDirectories))
        {
            var fileParseResult = ParseFile(file);
            results.Add(fileParseResult);
        }
        return results;
    }

    public FileParseResult ParseFile(string path)
    {
        OverrideCurrentOnNull = true;
        var result = new FileParseResult();
        _errors = result.Errors;
        var text = File.ReadAllText(path);
        var tokenizer = TokenizerFactory.Create();
        var tokens = tokenizer.Tokenize(text, path).Where(x => x.Type != BuiltinTokenTypes.EndOfFile);
        Initialize(tokens.ToList());

        while (!AtEnd())
        {
            try
            {
                var declaration = ParseDeclaration();
                if (declaration is ImportDeclaration importDeclaration)
                {
                    if (result.Imports != null) _errors.Add(new ParsingException(importDeclaration.Token, "duplicate import declaration"));
                    result.Imports = importDeclaration;
                }
                else if (declaration is TypeDeclaration typeDeclaration)
                {
                    result.DeclaredTypes.Add(typeDeclaration);
                }
                else if (declaration is FunctionDeclaration functionDeclaration)
                {
                    result.DeclaredFunctions.Add(functionDeclaration);
                }
                else if (declaration is ModuleDeclaration moduleDeclaration)
                {
                    if (result.ModuleName != null) _errors.Add(new ParsingException(moduleDeclaration.ModuleName, "duplicate module declaration"));
                    result.ModuleName = moduleDeclaration.ModuleName;
                }
            } catch (UnrecoverableTokenException ute)
            {
                break;
            } catch (ParsingException pe)
            {
                _errors.Add(pe);
                break;
            } catch (Exception)
            {
                break;
            }
        }
        if (result.ModuleName == null) _errors.Add(new(new(BuiltinTokenTypes.EndOfFile, "", Location.Zero, Location.Zero), "no module name provided"));
        
        return result;
    }


    private DeclarationBase ParseDeclaration()
    {
        var start = Current().Start;
        DeclarationBase declaration;
        if (AdvanceIfMatch(TokenTypes.Import)) declaration = ParseImport();
        else if (AdvanceIfMatch(TokenTypes.Type)) declaration = ParseType();
        else if (AdvanceIfMatch(TokenTypes.Public)) declaration = ParseFunction(true);
        else if (AdvanceIfMatch(TokenTypes.Function)) declaration = ParseFunction(false);
        else if (AdvanceIfMatch(TokenTypes.Module)) declaration = ParseModule();
        else declaration = AddError(Current(), $"unexpected token {Current()}", DeclarationBase.Missing);
        declaration.Start = start;
         declaration.End = Previous().End;
        return declaration;
    }


    #region Declarations

    private ImportDeclaration ParseImport()
    {
        var token = Previous();
        var imports = new List<Token>();
        ConsumeOrReturn(TokenTypes.LCurly, "expect declaration structure: import { \"mod\", ... }");
        if (!AdvanceIfMatch(TokenTypes.RCurly))
        {
            do
            {

                DoWithSeek(TokenTypes.Comma, () =>
                {
                    var importModule = ConsumeOrReturn(TokenTypes.String, "expect import module name");
                    imports.Add(importModule);
                });
            }
            while (AdvanceIfMatch(TokenTypes.Comma));
            ConsumeOrReturn(TokenTypes.RCurly, "expect enclosing '}' in import declaration");
        }
       
        return new ImportDeclaration(token, imports);
    }

    private TypeDeclaration ParseType()
    {
        var typeName = ConsumeOrReturn(TokenTypes.Word, "expect typename");
        ConsumeOrReturn(TokenTypes.LCurly, "expect declaration structure: type TypeName { int field1; ... }");
        var fields = new List<TypeDeclarationField>();
        if (!AdvanceIfMatch(TokenTypes.RCurly))
        {
            do
            {
                DoWithSeek(TokenTypes.Semicolon, () =>
                {
                    var fieldType = ParseTypeSymbol();
                    var fieldName = Consume(TokenTypes.Word, "expect member name");
                    Consume(TokenTypes.Semicolon, "expect ';' after member name");
                    fields.Add(new(fieldName, fieldType));
                });

            } while (!AtEnd() && !Match(TokenTypes.RCurly));
            ConsumeOrReturn(TokenTypes.RCurly, "expect enclosing '}' in type declaration");
        }

        return new TypeDeclaration(typeName, fields);
    }

    private FunctionDeclaration ParseFunction(bool isPublic)
    {
        if (isPublic) ConsumeOrReturn(TokenTypes.Function, "only functions can have 'pub' modifier");
        Token? importPath = null;
        if (AdvanceIfMatch(TokenTypes.ImportPath)) importPath = Previous();
        var functionName = Consume(TokenTypes.Word, "expect function name");
        Consume(TokenTypes.LParen, "expect parameter list in function declaration: func name(int param1,...) { // Body... } ");
        var paramters = new List<FunctionDeclarationParameter>();
        bool isExtensionMethod = false;
        if (!AdvanceIfMatch(TokenTypes.RParen))
        {
            isExtensionMethod = AdvanceIfMatch(TokenTypes.This);
            do
            {
                DoWithSeek(TokenTypes.Comma, () =>
                {
                    var parameterType = ParseTypeSymbol();
                    var parameterName = ConsumeOrReturn(TokenTypes.Word, "expect parameter name");
                    paramters.Add(new(parameterName, parameterType));
                });
            } while (AdvanceIfMatch(TokenTypes.Comma));
            ConsumeOrReturn(TokenTypes.RParen, "expect enclosing ')' after function parameter list");
        }
        var returnType = ParseTypeSymbol();
        bool isErrorable = AdvanceIfMatch(TokenTypes.Exclamation);

        if (importPath != null)
        {
            ConsumeOrReturn(TokenTypes.Semicolon, "expect ';' after imported function declaration");
            return new FunctionDeclaration(functionName, returnType, paramters, new(), isErrorable, isPublic, isExtensionMethod, importPath);
        }

        var statements = ParseBlock();

        return new FunctionDeclaration(functionName, returnType, paramters, statements, isErrorable, isPublic, isExtensionMethod, importPath);   
    }

    private ModuleDeclaration ParseModule()
    {
        var moduleName = ConsumeOrReturn(TokenTypes.Word, "expect module name");
        return new ModuleDeclaration(moduleName);
    }

    #endregion
    private StatementBase ParseStatement()
    {
        StatementBase statement;
        var start = Current().Start;
        if (AdvanceIfMatch(TokenTypes.Error)) statement = ParseError();
        else if (AdvanceIfMatch(TokenTypes.Break)) statement = new BreakStatement();
        else if (AdvanceIfMatch(TokenTypes.Continue)) statement = new ContinueStatement();
        else if (AdvanceIfMatch(TokenTypes.Return)) statement = ParseReturn();
        else if (AdvanceIfMatch(TokenTypes.While)) statement = ParseWhile();
        else statement = ParseAssignmentOrConditional();
        statement.Start = start;
        statement.End = Previous().End;
        return statement;
    }

    private WhileStatement ParseWhile()
    {
        var condition = CaptureConditionalExpression();
      
        var thenBlock = ParseBlock();
        return new WhileStatement(condition, thenBlock);
    }

    private StatementBase ParseAssignmentOrConditional()
    {
        LValue assignmentTarget;
        if (AdvanceIfMatch(TokenTypes.Discard)) assignmentTarget = new DiscardLValue(Previous());
        else
        {
            var potentialLValue = CaptureConditionalExpression();
            if (AdvanceIfMatch(TokenTypes.QuestionMark))
            {

                var thenBlock = ParseBlock();
                var elseBlock = new List<StatementBase>();
                if (AdvanceIfMatch(TokenTypes.Else))
                {
                    elseBlock = ParseBlock();
                }
                return new ConditionalStatement(potentialLValue, thenBlock, elseBlock);
            }

            if (!TryConvertToLValue(potentialLValue, out assignmentTarget))
                assignmentTarget = AddError(Previous(), $"invalid lvalue (expression type {potentialLValue.GetType()})", LValue.Missing(potentialLValue));
        }

        LValue? errorTarget = null;
        if (AdvanceIfMatch(TokenTypes.Comma))
        {
            errorTarget = ParseLValue();
        }

        var isDerefAssignment = AdvanceIfMatch(TokenTypes.DerefAssignment);
        if (!isDerefAssignment) ConsumeOrReturn(TokenTypes.Assignment, "expect '=' in assignment statement");
        var value = CaptureAssignmentRValue();
        ConsumeOrReturn(TokenTypes.Semicolon, "expect statement to end with ';'");
        if (isDerefAssignment) return new DereferenceAssignmentStatement(assignmentTarget, errorTarget, value);
        return new AssignmentStatement(assignmentTarget, errorTarget, value);
    }

    private bool TryConvertToLValue(ExpressionBase expression, out LValue lvalue)
    {
        lvalue = new DiscardLValue(Previous()); 
        if (expression is MemberAccessExpression memberAccessExpression)
        {
            if (!TryConvertToLValue(memberAccessExpression.Instance, out var instanceAsLValue)) return false;
            lvalue = new InstanceMemberLValue(instanceAsLValue, memberAccessExpression.MemberToAccess);
            lvalue.Start = expression.Start;
            lvalue.End = expression.End;
            return true;
        }
        if (expression is IdentifierExpression identifierExpression)
        {
            lvalue = new IdentifierLValue(identifierExpression.Symbol);
            lvalue.Start = identifierExpression.Start;
            lvalue.End = identifierExpression.End;
            return true;
        }
        return false;
    }

    private List<StatementBase> ParseBlock()
    {
        var block = new List<StatementBase>();
        if (AdvanceIfMatch(TokenTypes.LCurly))
        {
            do
            {
                DoWithSeek(TokenTypes.Semicolon, () =>
                {
                    var statement = ParseStatement();
                    block.Add(statement);
                });
            } while (!AtEnd() && !Match(TokenTypes.RCurly));
            ConsumeOrReturn(TokenTypes.RCurly, "expect enclosing '}' in block");
        }
        else
        {
            var statement = ParseStatement();
            block.Add(statement);
        }
        return block;
    }

    private ErrorStatement ParseError()
    {
        var value = CaptureExpression();
        ConsumeOrReturn(TokenTypes.Semicolon, "expect statement to end with ';'");
        return new ErrorStatement(value);
    }

    private ReturnStatement ParseReturn()
    {
        var value = CaptureExpression();
        ConsumeOrReturn(TokenTypes.Semicolon, "expect statement to end with ';'");
        return new ReturnStatement(value);
    }


    private ExpressionBase CaptureAssignmentRValue()
    {
        if (AdvanceIfMatch(TokenTypes.StackAllocate)) return Capture(() => ParseStackAllocate());
        return CaptureExpression();
    }

    private OperableExpresson CaptureExpression()
    {
        return Capture<OperableExpresson>(() =>
        {
            
            if (AdvanceIfMatch(TokenTypes.ReferenceType))
            {
                var potentialReference = Capture(() => ParseGetOrCallExpression());
                if (potentialReference is MemberAccessExpression memberAccessExpression)
                {
                    return new ReferenceExpression(memberAccessExpression.Instance, memberAccessExpression.MemberToAccess);
                }
                else if (potentialReference is IdentifierExpression identifierExpression)
                   return new ReferenceExpression(null, identifierExpression.Symbol);
                else throw new ParsingException(Previous(), $"unexpected expression type on right hand side of 'ref'");
            }
            else return CaptureConditionalExpression();
        });
        
    }

    private OperableExpresson CaptureConditionalExpression()
    {
        return Capture<OperableExpresson>(() =>
        {
            var lhs = Capture(() => ParseGetOrCallExpression());
            if (AdvanceIfMatchOperator(out var op))
            {
                var rhs = Capture(() => ParseGetOrCallExpression());
                return new BinaryExpression(lhs, rhs, op);
            }
            return lhs;
        }); 
    }

    private CastExpression ParseCastExpression()
    {
        var targetType = ParseTypeSymbol();
        var value = Capture(() => ParseGetOrCallExpression()); // to force user to wrap binary operations occuring on right side of cast
        return new CastExpression(targetType, value);
    }

    private bool AdvanceIfMatchOperator(out OperatorType op) 
    {
        var tokenType = Current().Type;
        if (Enum.TryParse(tokenType, false, out op))
        {
            Advance();
            return true;
        }
        return false;
    }

    private ExpressionBase ParseStackAllocate()
    {
        var typeToAllocate = ParseTypeSymbol();
        return new StackAllocateExpression(typeToAllocate);
    }


    private OperableExpresson ParseGetOrCallExpression()
    {
        var lhs = Capture(() => ParseIdentifier());
        var arguments = new List<ExpressionBase>();
        while (Match(TokenTypes.LParen) || Match(TokenTypes.Dot))
        {
            if (AdvanceIfMatch(TokenTypes.LParen))
            {
                if (!AdvanceIfMatch(TokenTypes.RParen))
                {
                    do
                    {
                        DoWithSeek(TokenTypes.Comma , () =>
                        {
                            var argument = CaptureExpression();
                            arguments.Add(argument);
                        });
                    } while (AdvanceIfMatch(TokenTypes.Comma));
                    ConsumeOrReturn(TokenTypes.RParen, "expect enclosing ')' in argument list");
                }
                lhs =  new CallExpression(lhs, arguments);
            }
            else
            {
                Advance();
                var memberToAccess = ConsumeOrReturn(TokenTypes.Word, "expect member name after '.'");
                lhs = new MemberAccessExpression(lhs, memberToAccess);
            }
        }
        return lhs;
    }

    private OperableExpresson ParseIdentifier()
    {
        if (AdvanceIfMatch(TokenTypes.BitwiseXor)) return ParseCastExpression();
        if (AdvanceIfMatch(TokenTypes.Deref))
        {
            var expression = ParseGetOrCallExpression();
            return new DereferenceExpression(expression);
        }
        if (AdvanceIfMatch(TokenTypes.SizeOf))
        {
            var typeSymbol = ParseTypeSymbol();
            return new SizeOfExpression(typeSymbol);
        }
        if (AdvanceIfMatch(TokenTypes.Word)) return new IdentifierExpression(Previous());

        return Capture(() => ParseLiteralExpression());

    }

    private OperableExpresson ParseLiteralExpression()
    {
        if (AdvanceIfMatch(TokenTypes.LParen))
        {
            // Parse grouped operation
            var expression = CaptureExpression();
            ConsumeOrReturn(TokenTypes.RParen, "expect enclosing ')' in grouping");
            return expression;
        }
        var isNegative = AdvanceIfMatch(TokenTypes.Subtraction)? -1: 1;
        if (AdvanceIfMatch(TokenTypes.Integer))
        {
            if (int.TryParse(Previous().Lexeme, out var i)) return new LiteralExpression(i * isNegative);
            return AddError(Previous(), $"unable to parse value '{Previous().Lexeme}' to integer", ExpressionBase.Missing);
        }
        if (AdvanceIfMatch(TokenTypes.Float))
        {
            if (float.TryParse(Previous().Lexeme, out var flt)) return new LiteralExpression(flt * isNegative);
            return AddError(Previous(), $"unable to parse value '{Previous().Lexeme}' to float32", ExpressionBase.Missing);
        }
        if (AdvanceIfMatch(TokenTypes.Double))
        {
            if (double.TryParse(Previous().Lexeme, out var dbl)) return new LiteralExpression(dbl * isNegative);
            return AddError(Previous(), $"unable to parse value '{Previous().Lexeme}' to float64", ExpressionBase.Missing);
        }
        if (AdvanceIfMatch(TokenTypes.String))
        {
            return new LiteralExpression(Previous().Lexeme);
        }
        if (AdvanceIfMatch(TokenTypes.Null))
        {
            return new LiteralExpression(null);
        }
        if (AdvanceIfMatch(TokenTypes.Char))
        {
            var chr = Encoding.UTF8.GetBytes(Previous().Lexeme);
            if (chr.Length == 1) return new LiteralExpression(chr[0]);
            return AddError(Previous(), $"unable to parse value '{Previous().Lexeme}' to byte", ExpressionBase.Missing);      
        }
        if (AdvanceIfMatch(TokenTypes.Wide))
        {
            if (AdvanceIfMatch(TokenTypes.String)) return new LiteralExpression(new WString(Previous().Lexeme));
            else if (AdvanceIfMatch(TokenTypes.Char))
            {
                var chr = Encoding.Unicode.GetBytes(Previous().Lexeme);
                if (chr.Length == 2) return new LiteralExpression(BitConverter.ToInt16(chr));
                return AddError(Previous(), $"unable to parse value '{Previous().Lexeme}' to int16", ExpressionBase.Missing);
            }
            else return new IdentifierExpression(Previous()); // identifier w
        }
        if (AdvanceIfMatch(TokenTypes.Hex))
        {
            ConsumeOrReturn(TokenTypes.Integer, "expect integer hex value");
            if (Previous().Lexeme.Length == 2 && byte.TryParse(Previous().Lexeme, NumberStyles.HexNumber, null, out var b)) return new LiteralExpression((byte)(b * isNegative));
            else if (Previous().Lexeme.Length == 4 && short.TryParse(Previous().Lexeme, NumberStyles.HexNumber, null, out var s)) return new LiteralExpression((short)(s * isNegative));
            else if (Previous().Lexeme.Length == 8 && int.TryParse(Previous().Lexeme, NumberStyles.HexNumber, null, out var i)) return new LiteralExpression(i * isNegative);
            else if (Previous().Lexeme.Length == 16 && long.TryParse(Previous().Lexeme, NumberStyles.HexNumber, null, out var l)) return new LiteralExpression(l * isNegative);
            return AddError(Previous(), $"unable to parse hex value '{Previous().Lexeme}'", ExpressionBase.Missing);
        }
        var token = Current();
        throw new ParsingException(token, $"unexpected token {token}");
    }

    private LValue ParseLValue()
    {
        // LValues can only be identifiers or member access operations using identifiers
        // IE myid or myid.field1.field2
        // NOT myfunc().field1

        if (AdvanceIfMatch(TokenTypes.Discard)) return new DiscardLValue(Previous());

        LValue instance = ParseIdentifierLValue();
        
        while (AdvanceIfMatch(TokenTypes.Dot))
        {
            var memberToAccess = ConsumeOrReturn(TokenTypes.Word, "expect identifier");
            instance = new InstanceMemberLValue(instance, memberToAccess);
        }
        return instance;
    }

    private LValue ParseIdentifierLValue()
    {
        var variableName = ConsumeOrReturn(TokenTypes.Word, "expect identifier");
        return new IdentifierLValue(variableName);
    }

    private TypeSymbol ParseTypeSymbol()
    {
        Token? module = null;

        if (PeekMatch(1, TokenTypes.Dot))
        {
            module = Consume(TokenTypes.Word, "expect module name");
            Advance();
        }
        if (AdvanceIfMatch(TokenTypes.Function))
        {
            var typename = Previous();
            var typeArguments = new List<TypeSymbol>();
            Consume(TokenTypes.LBracket, "expect type symbol structure: func[int, void]");
            do
            {
                var typeArgument = ParseTypeSymbol();
                typeArguments.Add(typeArgument);
            } while (AdvanceIfMatch(TokenTypes.Comma));
            var isErrorable = AdvanceIfMatch(TokenTypes.Exclamation);
            Consume(TokenTypes.RBracket, "expect enclosing ']' after type arguments");

            return new TypeSymbol(module, typename, typeArguments, isErrorable);
        }
        if (AdvanceIfMatch(TokenTypes.ReferenceType))
        {
            var typename = Previous();
            Consume(TokenTypes.LBracket, "expect type symbol structure: ref[int]");
            var typeArgument = ParseTypeSymbol();
            Consume(TokenTypes.RBracket, "expect enclosing ']' after type argument");
            return new TypeSymbol(module, typename, [typeArgument], false);
        }
        var typeName = Consume(TokenTypes.Word, "expect type symbol");
        return new TypeSymbol(module, typeName, new(), false);
    }

    private bool Seek(string tokenTypeToSeekPast)
    {
        while (!AtEnd())
        {
            if (AdvanceIfMatch(tokenTypeToSeekPast)) return true;
            Advance();
        }
        return false;
    }

    private void DoWithSeek(string tokenTypeToSeekPast, Action action)
    {
        try
        {
            action();
        }catch(ParsingException pe)
        {
            _errors.Add(pe);
            if (!Seek(tokenTypeToSeekPast)) throw new UnrecoverableTokenException(pe);
        }
    }

    private TExpression Capture<TExpression>(Func<TExpression> parseFunction) where TExpression : ExpressionBase
    {
        var start = Current().Start;
        var value = parseFunction();
        var end = Previous().End;
        value.Start = start;
        value.End = end;
        return value;
    }

    private Token ConsumeOrReturn(string tokenType, string errorMessage)
    {
        if (AdvanceIfMatch(tokenType)) return Previous();
        _errors.Add(new ParsingException(Current(), errorMessage));
        return new Token(TokenTypes.Missing, Current().Lexeme, Current().Start, Current().End);
    }


    private MissingDeclaration AddError(Token token, string message, MissingDeclaration tErr)
    {
        _errors.Add(new(token, message));
        tErr.Token = token;
        return tErr;
    }

    private MissingExpression AddError(Token token, string message, MissingExpression tErr)
    {
        _errors.Add(new(token, message));
        tErr.Token = token;
        return tErr;
    }

    private MissingLValue AddError(Token token, string message, MissingLValue tErr)
    {
        _errors.Add(new(token, message));
        return tErr;
    }
}


public class UnrecoverableTokenException: System.Exception
{
    public UnrecoverableTokenException(ParsingException orignatingException)
    {
        OrignatingException = orignatingException;
    }

    public ParsingException OrignatingException { get; set; }


}
public class WString
{
    public WString(string value)
    {
        Value = value;
    }
    public string Value { get; set; }

}