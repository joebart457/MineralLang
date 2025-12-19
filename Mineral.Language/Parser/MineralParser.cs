using Mineral.Language.Declarations;
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Globalization;
using Tokenizer.Core;
using Tokenizer.Core.Constants;
using Tokenizer.Core.Exceptions;
using Tokenizer.Core.Models;

namespace Mineral.Language.Parser;

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
        else throw new ParsingException(Current(), $"unexpected token {Current()}");
        declaration.Start = start;
         declaration.End = Previous().End;
        return declaration;
    }


    #region Declarations

    private ImportDeclaration ParseImport()
    {
        var token = Previous();
        var imports = new List<Token>();
        Consume(TokenTypes.LCurly, "expect declaration structure: import { \"mod\", ... }");
        if (!AdvanceIfMatch(TokenTypes.RCurly))
        {
            do
            {

                DoWithSeek(TokenTypes.Comma, () =>
                {
                    var importModule = Consume(TokenTypes.String, "expect import module name");
                    imports.Add(importModule);
                });
            }
            while (AdvanceIfMatch(TokenTypes.Comma));
            Consume(TokenTypes.RCurly, "expect enclosing '}' in import declaration");
        }
       
        return new ImportDeclaration(token, imports);
    }

    private TypeDeclaration ParseType()
    {
        var typeName = Consume(TokenTypes.Word, "expect typename");
        Consume(TokenTypes.LCurly, "expect declaration structure: type TypeName { int field1; ... }");
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
            Consume(TokenTypes.RCurly, "expect enclosing '}' in type declaration");
        }

        return new TypeDeclaration(typeName, fields);
    }

    private FunctionDeclaration ParseFunction(bool isPublic)
    {
        if (isPublic) Consume(TokenTypes.Function, "only functions can have 'pub' modifier");
        Token? importPath = null;
        if (AdvanceIfMatch(TokenTypes.ImportPath)) importPath = Previous();
        var functionName = Consume(TokenTypes.Word, "expect function name");
        Consume(TokenTypes.LParen, "expect parameter list in function declaration: func name(int param1,...) { // Body... } ");
        var paramters = new List<FunctionDeclarationParameter>();
        if (!AdvanceIfMatch(TokenTypes.RParen))
        {
            do
            {
                DoWithSeek(TokenTypes.Comma, () =>
                {
                    var parameterType = ParseTypeSymbol();
                    var parameterName = Consume(TokenTypes.Word, "expect parameter name");
                    paramters.Add(new(parameterName, parameterType));
                });
            } while (AdvanceIfMatch(TokenTypes.Comma));
            Consume(TokenTypes.RParen, "expect enclosing ')' after function parameter list");
        }

        var returnType = ParseTypeSymbol();
        bool isErrorable = AdvanceIfMatch(TokenTypes.Exclamation);

        if (importPath != null)
        {
            Consume(TokenTypes.Semicolon, "expect ';' after imported function declaration");
            return new FunctionDeclaration(functionName, returnType, paramters, new(), isErrorable, isPublic, importPath);
        }

        var statements = new List<StatementBase>();
        Consume(TokenTypes.LCurly, "expect function body ");
        if (!AdvanceIfMatch(TokenTypes.RCurly))
        {
            do
            {
                DoWithSeek(TokenTypes.Semicolon, () =>
                {
                    var statement = ParseStatement();
                    statements.Add(statement);
                });
            } while (!AtEnd() && !Match(TokenTypes.RCurly));
            Consume(TokenTypes.RCurly, "expect enclosing '}' for function body");
        }

        return new FunctionDeclaration(functionName, returnType, paramters, statements, isErrorable, isPublic, importPath);   
    }

    private ModuleDeclaration ParseModule()
    {
        var moduleName = Consume(TokenTypes.Word, "expect module name");
        return new ModuleDeclaration(moduleName);
    }

    #endregion
    private StatementBase ParseStatement()
    {
        StatementBase statement;
        var start = Current().Start;
        if (AdvanceIfMatch(TokenTypes.Error)) statement = ParseError();
        else if (AdvanceIfMatch(TokenTypes.Return)) statement = ParseReturn();
        else  statement = ParseAssignmentOrConditional();
        statement.Start = start;
        statement.End = Previous().End;
        return statement;
    }

    private StatementBase ParseAssignmentOrConditional()
    {
        var lValue = ParseLValue();
        if (AdvanceIfMatch(TokenTypes.QuestionMark))
        {
       
            var thenBlock = new List<StatementBase>();
            if (AdvanceIfMatch(TokenTypes.LCurly))
            {
                do
                {
                    DoWithSeek(TokenTypes.Semicolon, () =>
                    {
                        var statement = ParseStatement();
                        thenBlock.Add(statement);
                    });
                } while (AdvanceIfMatch(TokenTypes.Semicolon));
                Consume(TokenTypes.RCurly, "expect enclosing '}' in block");
            }
            else
            {
                var statement = ParseStatement();
                thenBlock.Add(statement);
            }
            return new ConditionalStatement(lValue, thenBlock);  
        }

        var assignmentTarget = lValue;
        LValue? errorTarget = null;
        if (AdvanceIfMatch(TokenTypes.Comma))
        {
            errorTarget = ParseLValue();
        }

        Consume(TokenTypes.Equal, "expect '=' in assignment statement");
        var value = ParseExpression();
        Consume(TokenTypes.Semicolon, "expect statement to end with ';'");
        return new AssignmentStatement(assignmentTarget, errorTarget, value);
    }

    private ErrorStatement ParseError()
    {
        var value = ParseExpression();
        Consume(TokenTypes.Semicolon, "expect statement to end with ';'");
        return new ErrorStatement(value);
    }

    private ReturnStatement ParseReturn()
    {
        var value = ParseExpression();
        Consume(TokenTypes.Semicolon, "expect statement to end with ';'");
        return new ReturnStatement(value);
    }

    private ExpressionBase ParseExpression()
    {
        var start = Current().Start;
        ExpressionBase expression;
        if (AdvanceIfMatch(TokenTypes.StackAllocate)) expression = ParseStackAllocate();
        else if (AdvanceIfMatch(TokenTypes.ReferenceType))
        {
            var potentialReference = ParseGetOrCallExpression();
            if (potentialReference is MemberAccessExpression memberAccessExpression)
            {
                expression = new ReferenceExpression(memberAccessExpression.Instance, memberAccessExpression.MemberToAccess);
            }
            else if (potentialReference is IdentifierExpression identifierExpression)
                expression = new ReferenceExpression(null, identifierExpression.Symbol);
            else throw new ParsingException(Previous(), $"unexpected expression type on right hand side of 'ref'");
        }
        else expression = ParseGetOrCallExpression();
        expression.End = Previous().End;
        return expression;
    }

    private ExpressionBase ParseStackAllocate()
    {
        var typeToAllocate = ParseTypeSymbol();
        return new StackAllocateExpression(typeToAllocate);
    }

    private ExpressionBase ParseGetOrCallExpression()
    {
        var lhs = ParseIdentifier();
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
                            var argument = ParseExpression();
                            arguments.Add(argument);
                        });
                    } while (AdvanceIfMatch(TokenTypes.Comma));
                    Consume(TokenTypes.RParen, "expect enclosing ')' in argument list");
                }
                lhs =  new CallExpression(lhs, arguments);
            }
            else
            {
                Advance();
                var memberToAccess = Consume(TokenTypes.Word, "expect member name after '.'");
                lhs = new MemberAccessExpression(lhs, memberToAccess);
            }
        }
        return lhs;
    }

    private ExpressionBase ParseIdentifier()
    {
        if (AdvanceIfMatch(TokenTypes.Word))
        {
            return new IdentifierExpression(Previous());
        }

        return ParseLiteralExpression();
    }

    private ExpressionBase ParseLiteralExpression()
    {
        if (AdvanceIfMatch(TokenTypes.Integer))
        {
            if (int.TryParse(Previous().Lexeme, out var i)) return new LiteralExpression(i);
            throw new ParsingException(Previous(), $"unable to parse value '{Previous().Lexeme}' to integer");
        }
        if (AdvanceIfMatch(TokenTypes.Float))
        {
            if (float.TryParse(Previous().Lexeme, out var flt)) return new LiteralExpression(flt);
            throw new ParsingException(Previous(), $"unable to parse value '{Previous().Lexeme}' to integer");
        }
        if (AdvanceIfMatch(TokenTypes.String))
        {
            return new LiteralExpression(Previous().Lexeme);
        }
        if (AdvanceIfMatch(TokenTypes.Null))
        {
            return new LiteralExpression(null);
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
            var memberToAccess = Consume(TokenTypes.Word, "expect identifier");
            instance = new InstanceMemberLValue(instance, memberToAccess);
        }
        return instance;
    }

    private LValue ParseIdentifierLValue()
    {
        var variableName = Consume(TokenTypes.Word, "expect identifier");
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
            Consume(TokenTypes.RBracket, "expect enclosing ']' after type arguments");
            var isErrorable = AdvanceIfMatch(TokenTypes.Exclamation);
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
}


public class UnrecoverableTokenException: System.Exception
{
    public UnrecoverableTokenException(ParsingException orignatingException)
    {
        OrignatingException = orignatingException;
    }

    public ParsingException OrignatingException { get; set; }


}