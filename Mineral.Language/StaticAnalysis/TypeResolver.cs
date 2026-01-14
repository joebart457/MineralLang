using Mineral.Language.Compiler;
using Mineral.Language.Declarations;
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Parser;
using Mineral.Language.Statements;
using System;
using Tokenizer.Core.Constants;
using Tokenizer.Core.Models;

namespace Mineral.Language.StaticAnalysis;

public class TypeResolver
{
    private static readonly Dictionary<Token, ConcreteType> BuiltinTypes = new Dictionary<Token, ConcreteType>(new TokenEqualityComparer())
    {
        { new(TokenTypes.Word, "byte", Location.Zero, Location.Zero ), NativeTypes.Byte },
        { new(TokenTypes.Word, "int", Location.Zero, Location.Zero ), NativeTypes.Int },
        { new(TokenTypes.Word, "float32", Location.Zero, Location.Zero ), NativeTypes.Float32 },
        { new(TokenTypes.Word, "float64", Location.Zero, Location.Zero ), NativeTypes.Float64 },
        { new(TokenTypes.Word, "string", Location.Zero, Location.Zero ), NativeTypes.String },
        { new(TokenTypes.Word, "wstring", Location.Zero, Location.Zero ), NativeTypes.WString },
        { new(TokenTypes.Word, "cstring", Location.Zero, Location.Zero ), NativeTypes.CString },
        { new(TokenTypes.Word, "void", Location.Zero, Location.Zero ), new ConcreteType(BuiltinType.Void) },

    };

    private FunctionKey CreateFunctionKey(ModuleErrors errors, ModuleContext module, FunctionDeclaration functionDeclaration)
    {
        var parameterTypes = new List<ConcreteType>();
        foreach (var parameter in functionDeclaration.Parameters)
        {
            var parameterType = ResolveDeclaredType(errors, module, parameter.ParameterType);
            parameterTypes.Add(parameterType);
        }
        return new FunctionKey(functionDeclaration.FunctionName, parameterTypes);
    }

    private ConcreteType ResolveDeclaredType(ModuleErrors errors, ModuleContext module, TypeSymbol typeSymbol)
    {
        var moduleToSearch = module;
        if (typeSymbol.ModuleName != null)
        {
            if (!module.TryGetImportedModule(typeSymbol.ModuleName, out var importedModule))
            {
                errors.Add(typeSymbol, $"unable to find module '{typeSymbol.ModuleName.Lexeme}' verify module exists and is imported");
                return NativeTypes.Void;
            }
            moduleToSearch = importedModule;
        } 


        if (!typeSymbol.HasGenericTypeArguments)
        {
            // If there is no module and no type arguments check native types
            if (typeSymbol.ModuleName == null && BuiltinTypes.TryGetValue(typeSymbol.TypeName, out var builtinType))
                return builtinType;
            // if there are no type arguments and no native type is found, check types within the search module
            if (!moduleToSearch.TryGetType(typeSymbol.TypeName, out var type) || type == null)
            {
                errors.Add(typeSymbol.TypeName, $"Unable to resolve typename '{typeSymbol.TypeName.Lexeme}'");
                return NativeTypes.Void;
            }
            return type;
        }

        if (typeSymbol.ModuleName == null && typeSymbol.TypeName.Lexeme == "func")
        {
            var parameterTypes = new List<ConcreteType>();
            ConcreteType returnType = NativeTypes.Void;
            foreach(var genericArgument in typeSymbol.GenericTypeArguments)
            {
                var resolvedTypeArgument = ResolveDeclaredType(errors, module, genericArgument);
                if (genericArgument == typeSymbol.GenericTypeArguments.Last())
                    returnType = resolvedTypeArgument;
                else
                    parameterTypes.Add(resolvedTypeArgument);
            }
            var parameters = parameterTypes.Select(x => new FunctionParameter(new Token(BuiltinTokenTypes.Word, "_", Location.Zero, Location.Zero), x)).ToList();
            return new CallableType(null, returnType, parameters, typeSymbol.IsErrorable);
        }
        else if (typeSymbol.ModuleName == null && typeSymbol.TypeName.Lexeme == "ref")
        {
            if (typeSymbol.GenericTypeArguments.Count != 1)
            {
                errors.Add(typeSymbol, "expect exactly one type argument for reference type");
                return NativeTypes.Void;
            }
            var genericTypeArgument = ResolveDeclaredType(errors, module, typeSymbol.GenericTypeArguments[0]);
            return new ReferenceType(genericTypeArgument);
        }
        errors.Add(typeSymbol, $"Unable to resolve typename '{typeSymbol}'");
        return NativeTypes.Void;
    }

    public List<ModuleContext> ResolveWorkspace(ProgramContext programContext, List<FileParseResult> parsedFiles)
    {
        var modules = new List<ModuleContext>();
        foreach (var partialModule in parsedFiles)
        {
            var module = CreateModuleAndRegisterTypes(programContext, partialModule.ModuleName!, partialModule.DeclaredTypes);
            modules.Add(module);
        }

        foreach (var partialModule in parsedFiles)
        {
            ResolveModuleImportsAndTypes(programContext, partialModule.ModuleName!, partialModule.Imports, partialModule.DeclaredTypes);
        }

        foreach (var partialModule in parsedFiles)
        {
            ForwardDeclareModuleFunctions(programContext, partialModule.ModuleName!, partialModule.DeclaredFunctions);
        }

        foreach (var partialModule in parsedFiles)
        {
            ResolveModuleFunctions(programContext, partialModule.ModuleName!, partialModule.DeclaredFunctions);
        }
        return modules;
    }

    public ModuleContext CreateModuleAndRegisterTypes(ProgramContext programContext, Token moduleName, List<TypeDeclaration> typeDeclarations)
    {

        var module = programContext.GetOrAddModule(moduleName);
        var errors = module.GetOrCreateModuleErrors();


        // First pass, register all type names for forward references
        foreach (var typeDeclaration in typeDeclarations)
        {
            var structType = new StructType(typeDeclaration.TypeName);
            module.TryAddType(errors, typeDeclaration.TypeName, structType);
        }

        return module;

    }

    public ModuleContext ResolveModuleImportsAndTypes(ProgramContext programContext, Token moduleName, ImportDeclaration? importDeclaration, List<TypeDeclaration> typeDeclarations)
    {
        var module = programContext.GetOrAddModule(moduleName);
        var errors = module.GetOrCreateModuleErrors();

        // First process imported modules
        if (importDeclaration != null)
        {
            foreach (var import in importDeclaration.Imports)
            {
                if (importDeclaration.Imports.Count(x => x.Lexeme == import.Lexeme) > 1)
                {
                    errors.Add(import, $"duplicate import of '{import.Lexeme}' will be ignored");
                    continue;
                }
                if (!module.TryAddImportedModule(import))
                {
                    errors.Add(import, $"module {import.Lexeme} is not found in workspace");
                }

            }
        }


        // Resolve member types
        foreach (var typeDeclaration in typeDeclarations)
        {
            if (!module.TryGetType(typeDeclaration.TypeName, out var declaredStructType) || declaredStructType == null)
            {
                errors.Add(typeDeclaration.TypeName, $"Type '{typeDeclaration.TypeName.Lexeme}' is not defined");
                continue;
            }
            foreach (var member in typeDeclaration.Members)
            {
                var memberType = ResolveDeclaredType(errors, module, member.FieldType);
                if (!memberType.IsValidMemberType())
                    errors.Add(member.FieldType, $"type '{memberType}' is not a valid type for a struct member");
                declaredStructType.Members.Add(new StructTypeField(member.FieldName, memberType));
            }
        }


        return module;
    }

    public ModuleContext ForwardDeclareModuleFunctions(ProgramContext programContext, Token moduleName, List<FunctionDeclaration> functionDeclarations)
    {
        var module = programContext.GetOrAddModule(moduleName);
        var errors = module.GetOrCreateModuleErrors();

        // Register all function names for forward references
        foreach (var functionDeclaration in functionDeclarations)
        {
            RegisterFunction(errors, module, functionDeclaration);
        }

        return module;

    }

    public ModuleContext ResolveModuleFunctions(ProgramContext programContext, Token moduleName, List<FunctionDeclaration> functionDeclarations)
    {
        var module = programContext.GetOrAddModule(moduleName);
        var errors = module.GetOrCreateModuleErrors();

        // Second pass, resolve function bodies
        foreach (var functionDeclaration in functionDeclarations)
        {
            ResolveFunction(errors, module, functionDeclaration);
        }

        return module;

    }

    public (ModuleContext, ModuleErrors) ProcessModule(ProgramContext programContext, Token moduleName, ImportDeclaration? importDeclaration, List<TypeDeclaration> typeDeclarations, List<FunctionDeclaration> functionDeclarations)
    {

        var module = programContext.GetOrAddModule(moduleName);
        var errors = new ModuleErrors();

        // First process imported modules
        if (importDeclaration != null)
        {
            foreach (var import in importDeclaration.Imports)
            {
                if (importDeclaration.Imports.Count(x => x.Lexeme == import.Lexeme) > 1)
                {
                    errors.Add(import, $"duplicate import of '{import.Lexeme}' will be ignored");
                    continue;
                }
                if (!module.TryAddImportedModule(import))
                {
                    errors.Add(import, $"module {import.Lexeme} is not found in workspace");
                }

            }
        }
        

        // First pass, register all type names for forward references
        foreach (var typeDeclaration in typeDeclarations)
        {
            var structType = new StructType(typeDeclaration.TypeName);
            module.TryAddType(errors, typeDeclaration.TypeName, structType);
        }

        // Second pass, resolve member types
        foreach (var typeDeclaration in typeDeclarations)
        {
            if (!module.TryGetType(typeDeclaration.TypeName, out var declaredStructType) || declaredStructType == null)
            {
                errors.Add(typeDeclaration.TypeName, $"Type '{typeDeclaration.TypeName.Lexeme}' is not defined");
                continue;
            }
            foreach (var member in typeDeclaration.Members)
            {
                var memberType = ResolveDeclaredType(errors, module, member.FieldType);
                declaredStructType.Members.Add(new StructTypeField(member.FieldName, memberType));
            }
        }

        // First pass, register all function names for forward references
        foreach (var functionDeclaration in functionDeclarations)
        {
            RegisterFunction(errors, module, functionDeclaration);
        }

        // Second pass, resolve function bodies
        foreach (var functionDeclaration in functionDeclarations)
        {
            ResolveFunction(errors, module, functionDeclaration);
        }

       
        return (module, errors);

    }

    public void RegisterFunction(ModuleErrors errors, ModuleContext module, FunctionDeclaration functionDeclaration)
    {
        var context = new FunctionContext(module);
        context.FunctionName = functionDeclaration.FunctionName;
        context.ReturnType = ResolveDeclaredType(errors, module, functionDeclaration.ReturnType);
        if (!context.ReturnType.IsValidReturnType())
            errors.Add(functionDeclaration.ReturnType, $"type '{context.ReturnType}' is not a valid return type");
        context.IsErrorable = functionDeclaration.IsErrorable;
        context.IsPublic = functionDeclaration.IsPublic;
        context.ImportLibraryPath = functionDeclaration.ImportLibraryPath;
        foreach (var parameter in functionDeclaration.Parameters)
        {
            var parameterType = ResolveDeclaredType(errors, module, parameter.ParameterType);
            if (!parameterType.IsValidParameterType())
                errors.Add(parameter.ParameterType, $"type '{parameterType}' is not a valid type for a function parameter");
            if (context.Parameters.ContainsKey(parameter.ParameterName))
            {
                errors.Add(parameter.ParameterName, $"Parameter '{parameter.ParameterName.Lexeme}' is already defined in function '{functionDeclaration.FunctionName.Lexeme}'");
                context.Parameters[new Token(BuiltinTokenTypes.Word, "?", Location.Zero, Location.Zero)] = parameterType; // register as unkown parameter to avoid cascading errors
                continue;
            }

            context.Parameters[parameter.ParameterName] = parameterType;
        }
        var key = CreateFunctionKey(errors, module, functionDeclaration);
        var methodGroup = module.GetOrAddMethodGroup(functionDeclaration.FunctionName);
        methodGroup.TryAddOverload(errors, key, context);
    }

    public void ResolveFunction(ModuleErrors errors, ModuleContext module, FunctionDeclaration functionDeclaration)
    {
        var functionKey = CreateFunctionKey(errors, module, functionDeclaration);
        if (!module.TryGetMethodOverload(functionKey, out var functionContext))
        {
            errors.Add(functionDeclaration.FunctionName, $"Function '{functionKey}' is not registered");
            return;
        }
        if (functionContext.IsImported)
        {
            if (functionDeclaration.BodyStatements.Count != 0)
                errors.Add(functionDeclaration.FunctionName, "imported function cannot declare a function body");
            return;
        }
        foreach (var statement in functionDeclaration.BodyStatements)
        {
            Resolve(errors, module, functionContext, null, statement);
            functionContext.BodyStatements.Add(statement);
        }
        if (!functionContext.EncounteredReturnStatement && !functionContext.ReturnType.IsEqualTo(NativeTypes.Void)) 
        {
            errors.Add(functionDeclaration.FunctionName, $"Function '{functionDeclaration.FunctionName.Lexeme}' is missing a return statement");
        }
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, WhileStatement? parentLoop, StatementBase statement)
    {
        switch (statement)
        {
            case ReturnStatement returnStatement:
                Resolve(errors, module, context, returnStatement);
                break;
            case ErrorStatement errorStatement:
                Resolve(errors, module, context, errorStatement);
                break;
            case ConditionalStatement conditionalStatement:
                Resolve(errors, module, context, conditionalStatement);
                break;
            case WhileStatement whileStatement:
                Resolve(errors, module, context, whileStatement);
                break;
            case AssignmentStatement assignmentStatement:
                Resolve(errors, module, context, assignmentStatement);
                break;
            case DereferenceAssignmentStatement dereferenceAssignmentStatement:
                Resolve(errors, module, context, dereferenceAssignmentStatement);
                break;
            case BreakStatement breakStatement:
                Resolve(errors, module, context, parentLoop, breakStatement);
                break;
            case ContinueStatement continueStatement:
                Resolve(errors, module, context, parentLoop, continueStatement);
                break;
            default:
                errors.Add(statement, $"Unknown statement type '{statement.GetType()}'");
                break;
        }
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, ReturnStatement returnStatement)
    {
        Resolve(errors, module, context, returnStatement.ValueToReturn);
        var returnType = returnStatement.ValueToReturn.ConcreteType;
        if (!context.ReturnType.IsAssignableFrom(returnType))
        {
            errors.Add(returnStatement, $"Function '{context.FunctionName.Lexeme}' expected return type '{context.ReturnType}' but got '{returnType}'");
        }
        context.EncounteredReturnStatement = true;
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, ErrorStatement errorStatement)
    {
        Resolve(errors, module, context, errorStatement.ErrorToReturn);
        if (!errorStatement.ErrorToReturn.ConcreteType.IsErrorType())
        {
            errors.Add(errorStatement, $"Error statement must return an error type, but got '{errorStatement.ErrorToReturn.ConcreteType}'");
        }
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, ConditionalStatement conditionalStatement)
    {
        Resolve(errors, module, context, conditionalStatement.ConditionalTarget);

        ConcreteType conditionType = conditionalStatement.ConditionalTarget.ConcreteType;

        foreach (var statement in conditionalStatement.ThenBlock) Resolve(errors, module, context, null, statement); // TODO Branched return detection
        foreach (var statement in conditionalStatement.ElseBlock) Resolve(errors, module, context, null, statement); 
        if (!conditionType.IsConditionalTestable())
        {
            errors.Add(conditionalStatement.ConditionalTarget, $"Cannot use type '{conditionType}' as a conditional test");
        }
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, WhileStatement whileStatement)
    {
        Resolve(errors, module, context, whileStatement.ConditionalTarget);

        ConcreteType conditionType = whileStatement.ConditionalTarget.ConcreteType;

        foreach (var statement in whileStatement.ThenBlock) Resolve(errors, module, context, whileStatement, statement); // TODO Branched return detection
        if (!conditionType.IsConditionalTestable())
        {
            errors.Add(whileStatement.ConditionalTarget, $"Cannot use type '{conditionType}' as a conditional test in loop");
        }
    }

    private void ResolveLValue(ModuleErrors errors, ModuleContext module, FunctionContext context, LValue lValue)
    {
        switch (lValue)
        {
            case IdentifierLValue identifierLValue:
                ResolveLValue(errors, module, context, identifierLValue);
                break;
            case InstanceMemberLValue instanceMemberLValue:
                ResolveLValue(errors, module, context, instanceMemberLValue);
                break;
            // We do not include DiscardLValue because that must be handled at the source instead of resolved here
            default:
                errors.Add(lValue, $"Unkown lValue type '{lValue.GetType()}'");
                break;
        }
    }

    private void ResolveLValue(ModuleErrors errors, ModuleContext module, FunctionContext context, IdentifierLValue identifierLValue)
    {
        ConcreteType? variableType;
        if (context.TryGetVariableType(identifierLValue.VariableName, out variableType))
        {
            identifierLValue.TagAsType(variableType);

            return;
        }
        if (context.Parameters.TryGetValue(identifierLValue.VariableName, out variableType))
        {
            identifierLValue.TagAsType(variableType);
            return;
        }

        errors.Add(identifierLValue, $"Undefined variable '{identifierLValue.VariableName.Lexeme}'");
    }

    private void ResolveLValue(ModuleErrors errors, ModuleContext module, FunctionContext context, InstanceMemberLValue instanceMemberLValue)
    {
        ResolveLValue(errors, module, context, instanceMemberLValue.Instance);
        var instanceType = instanceMemberLValue.Instance.ConcreteType;
        if (instanceType is not StructType structType)
        {
            errors.Add(instanceMemberLValue.Member, $"Cannot access member '{instanceMemberLValue.Member.Lexeme}' of non-struct type '{instanceType}'");
            return;
        }
        var foundMember = structType.Members.Find(m => m.Name.Lexeme == instanceMemberLValue.Member.Lexeme);
        if (foundMember == null)
        {
            errors.Add(instanceMemberLValue.Member, $"Struct type '{structType}' does not contain member '{instanceMemberLValue.Member.Lexeme}'");
            return;
        }
        instanceMemberLValue.TagAsType(foundMember.FieldType);
    }


    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, AssignmentStatement assignmentStatement)
    {
        Resolve(errors, module, context, assignmentStatement.Value);
        var valueType = assignmentStatement.Value.ConcreteType;

        
        if (assignmentStatement.AssignmentTarget is IdentifierLValue identifierExpression)
        {
            assignmentStatement.AssignmentTarget.TagAsType(ResolveOrCreateVariable(errors, context, identifierExpression.VariableName, valueType));
        }
        else if (assignmentStatement.AssignmentTarget is DiscardLValue)
        {
            // Pass
        }
        else ResolveLValue(errors, module, context, assignmentStatement.AssignmentTarget);
        if (assignmentStatement.AssignmentTarget is not DiscardLValue)
        {
            if (assignmentStatement.Value is StackAllocateExpression sae)
            {
                if (assignmentStatement.AssignmentTarget.ConcreteType.IsValidParameterType())
                    errors.Add(assignmentStatement.AssignmentTarget, $"type '{assignmentStatement.AssignmentTarget.ConcreteType}' is not stack allocatable");
                if (!assignmentStatement.AssignmentTarget.ConcreteType.IsEqualTo(valueType))
                    errors.Add(assignmentStatement.AssignmentTarget, $"Cannot assign value of type '{valueType}' to lvalue of type '{assignmentStatement.AssignmentTarget.ConcreteType}'");
            }
            else if (!assignmentStatement.AssignmentTarget.ConcreteType.IsAssignableFrom(valueType))
                errors.Add(assignmentStatement.AssignmentTarget, $"Cannot assign value of type '{valueType}' to lvalue of type '{assignmentStatement.AssignmentTarget.ConcreteType}'");
        }
        

        var errorTarget = assignmentStatement.ErrorTarget;
        if (errorTarget != null)
        {
            if (!(valueType is CallableType callableType1 && callableType1.IsErrorable))
            {
                errors.Add(assignmentStatement.Value, $"expect errorable expression on right hand side of error assignment");
            }
            if (errorTarget is IdentifierLValue errorIdentifierExpression)
            {
                errorTarget.TagAsType(ResolveOrCreateVariable(errors, context, errorIdentifierExpression.VariableName, NativeTypes.Error));
            }
            else if (errorTarget is DiscardLValue)
            {
                // Pass
            }
            else ResolveLValue(errors, module, context, errorTarget);
            if (!errorTarget.ConcreteType.IsErrorType())
            {
                errors.Add(errorTarget, $"Error target must be of error type, but got '{errorTarget.ConcreteType}'");
            }
        }
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, DereferenceAssignmentStatement dereferenceAssignmentStatement)
    {
        Resolve(errors, module, context, dereferenceAssignmentStatement.Value);
        var valueType = dereferenceAssignmentStatement.Value.ConcreteType;

  

        if (dereferenceAssignmentStatement.AssignmentTarget is IdentifierLValue identifierExpression)
        {
            dereferenceAssignmentStatement.AssignmentTarget.TagAsType(ResolveOrCreateVariable(errors, context, identifierExpression.VariableName, valueType));
        }
        else if (dereferenceAssignmentStatement.AssignmentTarget is DiscardLValue)
        {
            errors.Add(dereferenceAssignmentStatement.AssignmentTarget, $"unexpected discard lvalue in dereference assignment");
        }
        else ResolveLValue(errors, module, context, dereferenceAssignmentStatement.AssignmentTarget);

        ConcreteType referencedType;
        if (dereferenceAssignmentStatement.AssignmentTarget.ConcreteType is not ReferenceType referenceType)
        {
            errors.Add(dereferenceAssignmentStatement.AssignmentTarget, $"left hand side of dereference assignment must be reference type");
            referencedType = valueType;
        }
        else referencedType = referenceType.ReferencedType;

        if (dereferenceAssignmentStatement.AssignmentTarget is not DiscardLValue)
        {
            if (dereferenceAssignmentStatement.Value is StackAllocateExpression sae)
            {
                errors.Add(sae, $"unable to stack allocate dereferenced value");
            }
            else if (!referencedType.IsAssignableFrom(valueType))
                errors.Add(dereferenceAssignmentStatement.AssignmentTarget, $"Cannot assign value of type '{valueType}' to lvalue of type '{referencedType}'");
        }


        var errorTarget = dereferenceAssignmentStatement.ErrorTarget;
        if (errorTarget != null)
        {
            if (!(valueType is CallableType callableType1 && callableType1.IsErrorable))
            {
                errors.Add(dereferenceAssignmentStatement.Value, $"expect errorable expression on right hand side of error assignment");
            }
            if (errorTarget is IdentifierLValue errorIdentifierExpression)
            {
                errorTarget.TagAsType(ResolveOrCreateVariable(errors, context, errorIdentifierExpression.VariableName, NativeTypes.Error));
            }
            else if (errorTarget is DiscardLValue)
            {
                // Pass
            }
            else ResolveLValue(errors, module, context, errorTarget);
            if (!errorTarget.ConcreteType.IsErrorType())
            {
                errors.Add(errorTarget, $"Error target must be of error type, but got '{errorTarget.ConcreteType}'");
            }
        }
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, WhileStatement? parentLoop, BreakStatement breakStatement)
    {
        if (parentLoop == null) errors.Add(breakStatement, $"'break' must occur inside loop body");
        else breakStatement.ParentLoop = parentLoop;
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, WhileStatement? parentLoop, ContinueStatement continueStatement)
    {
        if (parentLoop == null) errors.Add(continueStatement, $"'continue' must occur inside loop body");
        else continueStatement.ParentLoop = parentLoop;
    }

    private ConcreteType ResolveOrCreateVariable(ModuleErrors errors, FunctionContext context, Token variableName, ConcreteType attemptedAssignmentType)
    {
        if (context.LocalVariables.TryGetValue(variableName, out var existingType))
        {
            return existingType;
        }
        else if (context.Parameters.TryGetValue(variableName, out var parameterType))
        {
            return parameterType;
        }
        else
        {
            // If it doesn't exist, we assume it's being declared here
            if (attemptedAssignmentType is NullPointerType)
            {
                // if the attempted assignment is null and the variable does not exist,
                // we are unable to determine the actual type of the variable, so error
                errors.Add(variableName, $"unable to determine assigned type of variable {variableName.Lexeme}");
            }
            context.LocalVariables[variableName] = attemptedAssignmentType;
            return attemptedAssignmentType;
        }
    }

    private bool CanResolveToVariable(FunctionContext context, Token variableName)
    {
        if (context.LocalVariables.TryGetValue(variableName, out var existingType))
            return true;
        else if (context.Parameters.TryGetValue(variableName, out var parameterType))
            return true;
        else return false;
    }


    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, ExpressionBase expression)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                Resolve(errors, module, context, callExpression);
                break;
            case IdentifierExpression identifierExpression:
                Resolve(errors, module, context, identifierExpression);
                break;
            case MemberAccessExpression memberAccessExpression:
                Resolve(errors, module, context, memberAccessExpression);
                break;
            case LiteralExpression literalExpression:
                Resolve(errors, module, context, literalExpression);
                break;
            case StackAllocateExpression stackAllocateExpression:
                Resolve(errors, module, context, stackAllocateExpression);
                break;
            case ReferenceExpression referenceExpression:
                Resolve(errors, module, context, referenceExpression);
                break;
            case DereferenceExpression dereferenceExpression:
                Resolve(errors, module, context, dereferenceExpression);
                break;
            case CastExpression castExpression:
                Resolve(errors, module, context, castExpression);
                break;
            case BinaryExpression binaryExpression:
                Resolve(errors, module, context, binaryExpression);
                break;
            default:
                errors.Add(expression, $"Unknown expression type '{expression.GetType()}'");
                break;
        }

    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, BinaryExpression binaryExpression)
    {
        Resolve(errors, module, context, binaryExpression.Left);
        Resolve(errors, module, context, binaryExpression.Right);
        if (!binaryExpression.Left.ConcreteType.TryGetResultingTypeFromOperation(binaryExpression.Right.ConcreteType, binaryExpression.Operator, out var resultingType))
        {
            errors.Add(binaryExpression, $"cannot perform operation {binaryExpression.Operator} on types '{binaryExpression.Left.ConcreteType}' and '{binaryExpression.Right.ConcreteType}'");
            return;
        }
        binaryExpression.TagAsType(resultingType);


    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, CallExpression callExpression)
    {
        foreach (var argument in callExpression.Arguments) Resolve(errors, module, context, argument);
        if (callExpression.Callee is IdentifierExpression identifierExpression)
        {
            var argumentTypes = callExpression.Arguments.Select(arg => arg.ConcreteType).ToList();
            var functionKey = new FunctionKey(identifierExpression.Symbol, argumentTypes);
            if (module.TryGetMethodOverload(functionKey, out var functionContext))
            {
                callExpression.TagAsType(functionContext.ReturnType);
                callExpression.FunctionContext = functionContext;
                return; // we can return early since the functionKey check guarantees the correct types (or at least assignable types)
            }
        }
        else if (callExpression.Callee is MemberAccessExpression memberAccessExpression 
            && TryResolveToMethodGroup(errors, module, context, memberAccessExpression, out var methodGroup) 
            && methodGroup != null)
        {
            var argumentTypes = callExpression.Arguments.Select(arg => arg.ConcreteType).ToList();
            var functionKey = new FunctionKey(memberAccessExpression.MemberToAccess, argumentTypes);
            if (!methodGroup.TryGetPublicOverload(functionKey, out var functionContext))
            {
                errors.Add(callExpression.Callee, $"unable to find overload of function {functionKey}");
                return;
            }
            callExpression.TagAsType(functionContext.ReturnType);
            callExpression.FunctionContext = functionContext;
            return;
        }

        Resolve(errors, module, context, callExpression.Callee);
        if (callExpression.Callee.ConcreteType is not CallableType callableType)
        {
            errors.Add(callExpression.Callee, $"Cannot call non-callable type '{callExpression.Callee.ConcreteType}'");
            return;
        }

        callExpression.TagAsType(callableType.ReturnType);

        
        if (callExpression.Arguments.Count != callableType.Parameters.Count)
        {
            errors.Add(callExpression.Callee, $"Function expected {callableType.Parameters.Count} arguments but got {callExpression.Arguments.Count}");
            return;
        }
        for(int i = 0; i < callableType.Parameters.Count; i++)
        {
            var argumentType = callExpression.Arguments[i].ConcreteType;
            var parameterType = callableType.Parameters[i].ParameterType;
            if (!parameterType.IsAssignableFrom(argumentType))
            {
                errors.Add(callExpression.Arguments[i], $"Cannot assign argument of type '{argumentType}' to parameter of type '{parameterType}'");
            }
        }
    }

    private bool TryResolveToMethodGroup(ModuleErrors errors, ModuleContext module, FunctionContext context, MemberAccessExpression memberAccessExpression, out MethodGroup? methodGroup)
    {
        methodGroup = null;

        if (memberAccessExpression.Instance is IdentifierExpression identifierExpression)
        {
            if (CanResolveToVariable(context, identifierExpression.Symbol)) return false; // inner-scoped variable trumps outer-scoped imported module
            if (module.TryGetImportedModule(identifierExpression.Symbol, out var importedModule) 
                && importedModule.TryGetMethodGroup(memberAccessExpression.MemberToAccess, out methodGroup))
            {
                return true;
            }

        }
        return false;
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, IdentifierExpression identifierExpression)
    {
        ConcreteType? variableType;
        if (context.TryGetVariableType(identifierExpression.Symbol, out variableType))
        {
            identifierExpression.TagAsType(variableType);
            
            return;
        }
        if (context.Parameters.TryGetValue(identifierExpression.Symbol, out variableType))
        {
            identifierExpression.TagAsType(variableType);
            return;
        }

        if (module.TryGetMethodOverload(identifierExpression.Symbol, out var functionContext))
        {
            // This is only possible when function reference is unambiguous (i.e., only one overload exists)
            identifierExpression.TagAsType(functionContext.AsConcreteType());
            identifierExpression.FunctionContext = functionContext;
            return;
        }

        errors.Add(identifierExpression, $"Undefined variable '{identifierExpression.Symbol.Lexeme}'");
    }


    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, MemberAccessExpression memberAccessExpression)
    {
        if( TryResolveToMethodGroup(errors, module, context, memberAccessExpression, out var methodGroup) && methodGroup != null)
        {
            if (methodGroup.TryGetSingularOverload(out var functionOverload))
            {
                // This is only possible when function reference is unambiguous (i.e., only one overload exists)
                memberAccessExpression.TagAsType(functionOverload.AsConcreteType());
                memberAccessExpression.FunctionContext = functionOverload;
                return;
            }
        }
        Resolve(errors, module, context, memberAccessExpression.Instance);
        var instanceType = memberAccessExpression.Instance.ConcreteType;
        if (instanceType.TryFindMember(errors, memberAccessExpression.MemberToAccess, out var member) && member != null)
            memberAccessExpression.TagAsType(member.FieldType);
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, LiteralExpression literalExpression)
    {
        if (literalExpression.Value is int)
            literalExpression.TagAsType(NativeTypes.Int);
        else if (literalExpression.Value is float)
            literalExpression.TagAsType(NativeTypes.Float32);
        else if (literalExpression.Value is double)
            literalExpression.TagAsType(NativeTypes.Float64);
        else if (literalExpression.Value is string)
            literalExpression.TagAsType(NativeTypes.String);
        else if (literalExpression.Value is WString)
            literalExpression.TagAsType(NativeTypes.WString);
        else if (literalExpression.Value is null)
            literalExpression.TagAsType(new NullPointerType());
        else if (literalExpression.Value is byte)
            literalExpression.TagAsType(NativeTypes.Byte);
        else
            errors.Add(literalExpression, $"Unknown literal type for value '{literalExpression.Value}'");
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, StackAllocateExpression stackAllocateExpression)
    {
        var typeToAllocate = ResolveDeclaredType(errors, module, stackAllocateExpression.TypeToAllocate);
        stackAllocateExpression.TagAsType(typeToAllocate);
    }
    private void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, ReferenceExpression referenceExpression)
    {
        if (referenceExpression.Instance != null)
        {
            Resolve(errors, module, context, referenceExpression.Instance);
            if (referenceExpression.Instance.ConcreteType is not StructType structType)
            {
                errors.Add(referenceExpression.MemberToAccess, $"Cannot access member '{referenceExpression.MemberToAccess.Lexeme}' of non-struct type '{referenceExpression.Instance.ConcreteType}'");
                return;
            }
            var foundMember = structType.Members.Find(m => m.Name.Lexeme == referenceExpression.MemberToAccess.Lexeme);
            if (foundMember == null)
            {
                errors.Add(referenceExpression.MemberToAccess, $"Struct type '{structType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
                return;
            }
            referenceExpression.TagAsType(new ReferenceType(foundMember.FieldType));

            return;
        }

        if (!context.TryGetVariableType(referenceExpression.MemberToAccess, out var variableType))
        {
            errors.Add(referenceExpression.MemberToAccess, $"Undefined variable '{referenceExpression.MemberToAccess.Lexeme}'");
            return;
        }
        referenceExpression.TagAsType(new ReferenceType(variableType));
    }

    private void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, DereferenceExpression dereferenceExpression)
    {
        Resolve(errors, module, context, dereferenceExpression.Target);
        var targetType = dereferenceExpression.Target.ConcreteType;
        if (targetType is not ReferenceType referenceType)
        {
            errors.Add(dereferenceExpression, $"invalid dereference of non-reference type '{targetType}'");
            return;
        }
        if (referenceType.ReferencedType.IsVoidType()) errors.Add(dereferenceExpression, $"unable to dereference to void type");
        else if (referenceType.ReferencedType is StructType) errors.Add(dereferenceExpression, $"unable to dereference to type '{referenceType.ReferencedType}'");
        dereferenceExpression.TagAsType(referenceType.ReferencedType);
    }

    public void Resolve(ModuleErrors errors, ModuleContext module, FunctionContext context, CastExpression castExpression)
    {
        var targetType = ResolveDeclaredType(errors, module, castExpression.TargetType);
        Resolve(errors, module, context, castExpression.Value);
        var valueType = castExpression.Value.ConcreteType;
        if (!valueType.IsCastableTo(targetType))
            errors.Add(castExpression, $"unable to cast '{valueType}' to type '{targetType}'");
        else if (valueType.IsEqualTo(targetType))
            errors.Add(castExpression, $"redundant cast of '{valueType}' to type '{targetType}'");
        castExpression.TagAsType(targetType);
    }

}

public class ModuleErrors
{
    public ModuleErrors()
    {
        Errors = new();
    }
    public List<ModuleError> Errors { get; set; }
    public void Add(Token token, string error)
    {
        Errors.Add(new(token.Start, token.End, error));
    }

    public void Add(TypeSymbol typeSymbol, string error)
    {
        Errors.Add(new(typeSymbol.Start, typeSymbol.End, error));
    }

    public void Add(Location start, Location end, string error)
    {
        Errors.Add(new(start, end, error));
    }

    public void Add(StatementBase statement, string error)
    {
        Errors.Add(new(statement.Start, statement.End, error));
    }

    public void Add(ExpressionBase expression, string error)
    {
        Errors.Add(new(expression.Start, expression.End, error));
    }

}

public class ModuleError
{
    public ModuleError(Location start, Location end, string message)
    {
        Start = start;
        End = end;
        Message = message;
    }

    public Location Start { get; set; }
    public Location End { get; set; }
    public string Message { get; set; }

    public override string ToString()
    {
        return $"{Start}, {End}: {Message}";
    }
}

public class ProgramContext
{
    public ProgramContext()
    {
        Modules = new(new TokenEqualityComparer());
    }
    public Dictionary<Token, ModuleContext> Modules { get; private set; }

    public ModuleContext GetOrAddModule(Token moduleName)
    {
        if (TryGetModule(moduleName, out var module)) return module;
        module = new ModuleContext(this, moduleName.Lexeme);
        Modules[moduleName] = module;
        return module;
    }

    public bool TryAddModule(ModuleErrors errors, Token moduleName, ModuleContext module)
    {
        if (Modules.ContainsKey(moduleName))
        {
            errors.Add(moduleName, $"module '{moduleName.Lexeme}' is already defined in program");
            return false;
        }
        Modules[moduleName] = module;
        return true;
    }

    public bool TryGetModule(Token moduleName, out ModuleContext module)
    {
        module = new(this, "");
        if (Modules.TryGetValue(moduleName, out var potentialModule) && potentialModule != null)
        {
            module = potentialModule;
            return true;
        }
        return false;
    }
}


public class ModuleContext
{
    public ModuleContext(ProgramContext programContext, string moduleName)
    {
        ProgramContext = programContext;
        ModuleName = moduleName;
        Types = new(new TokenEqualityComparer());
        Methods = new(new TokenEqualityComparer());
        ImportedModules = new(new TokenEqualityComparer());
    }
    public ProgramContext ProgramContext { get; private set; }
    public string ModuleName { get; set; }
    public Dictionary<Token, StructType> Types { get; set; }
    public Dictionary<Token, MethodGroup> Methods { get; set; }
    public Dictionary<Token, ModuleContext> ImportedModules { get; set; }
    private ModuleErrors? _moduleErrors;
    public ModuleErrors GetOrCreateModuleErrors()
    {
        if (_moduleErrors == null) _moduleErrors = new(); 
        return _moduleErrors;  
    }

    public bool TryAddImportedModule(Token moduleName)
    {
        if (!ProgramContext.TryGetModule(moduleName, out var module)) return false;
        if (ImportedModules.ContainsKey(moduleName)) return true; // Ignore second import of the same module
        ImportedModules.Add(moduleName, module);
        return true;
    }

    public bool TryGetImportedModule(Token moduleName, out ModuleContext module)
    {
        if (ImportedModules.TryGetValue(moduleName, out var moduleContext))
        {
            module = moduleContext;
            return true;
        }
        module = new ModuleContext(ProgramContext, "");
        return false;
    }

    public bool TryAddType(ModuleErrors errors, Token typeName, StructType type)
    {
        if (Types.ContainsKey(typeName))
        {
            errors.Add(typeName, $"type {type} has already been defined in module '{ModuleName}'");
            return false;
        }
        Types[typeName] = type;
        return true;
    }

    public bool TryGetType(Token typeName, out StructType? type)
    {
        return Types.TryGetValue(typeName, out type);
    }

    public bool TryGetMethodGroup(Token methodName, out MethodGroup? methodGroup)
    {
        return Methods.TryGetValue(methodName, out methodGroup);
    }

    public bool TryGetMethodOverload(Token methodName, out FunctionContext functionContext)
    {
        functionContext = new(this);
        if (!Methods.TryGetValue(methodName, out var methodGroup)) return false;
        return methodGroup.TryGetSingularOverload(out functionContext);
    }

    public bool TryGetMethodOverload(FunctionKey functionKey, out FunctionContext functionContext)
    {
        functionContext = new(this);
        if (!Methods.TryGetValue(functionKey.FunctionName, out var methodGroup)) return false;
        return methodGroup.TryGetOverload(functionKey, out functionContext);
    }

    public MethodGroup GetOrAddMethodGroup(Token methodName)
    {
        if (Methods.TryGetValue(methodName, out var methodGroup)) return methodGroup;
        methodGroup = new MethodGroup(this, methodName);
        Methods.Add(methodName, methodGroup);
        return methodGroup;
    }

}

public class MethodGroup
{
    public MethodGroup(ModuleContext module, Token functionName)
    {
        Module = module;
        FunctionName = functionName;
        Overloads = new(new FunctionKeyEqualityComparer());
    }
    public ModuleContext Module { get; set; }
    public Token FunctionName { get; set; }
    public Dictionary<FunctionKey, FunctionContext> Overloads { get; private set; }

    public bool TryAddOverload(ModuleErrors errors, FunctionKey key, FunctionContext function)
    {
        if (Overloads.ContainsKey(key))
        {
            errors.Add(key.FunctionName, $"function '{function}' is already defined in module '{Module.ModuleName}'");
            return false;
        }
        Overloads.Add(key, function);
        return true;
    }

    public bool TryGetOverload(FunctionKey functionKey, out FunctionContext functionOverload)
    {
        functionOverload = new(Module);
        if (Overloads.TryGetValue(functionKey, out var potentialOverload) && potentialOverload != null)
        {
            functionOverload = potentialOverload;
            return true;
        }
        else return false;
    }

    public bool TryGetPublicOverload(FunctionKey functionKey, out FunctionContext functionOverload)
    {
        functionOverload = new(Module); 
        if (Overloads.TryGetValue(functionKey, out var potentialOverload) && potentialOverload != null && potentialOverload.IsPublic)
        {
            functionOverload = potentialOverload;
            return true;
        }
        else return false;
    }

    public bool TryGetSingularOverload(out FunctionContext functionOverload)
    {
        functionOverload = new(Module);
        if (Overloads.Count == 1)
        {
            functionOverload = Overloads.First().Value;
            return true;
        }
        return false;
    }

}

public class FunctionContext
{
    public FunctionContext(ModuleContext module)
    {
        Module = module;
        FunctionName = new Token(BuiltinTokenTypes.Word, "anonymous", Location.Zero, Location.Zero);
        LocalVariables = new Dictionary<Token, ConcreteType>(new TokenEqualityComparer());
        Parameters = new Dictionary<Token, ConcreteType>(new TokenEqualityComparer());
        ReturnType = new ConcreteType(BuiltinType.Void); // TODO Static void type?
        BodyStatements = new();

    }
    public ModuleContext Module { get; private set; }
    public Token FunctionName { get; set; }
    public Dictionary<Token, ConcreteType> LocalVariables { get; set; }
    public Dictionary<Token, ConcreteType> Parameters { get; set; }
    public ConcreteType ReturnType { get; set; }
    public bool EncounteredReturnStatement { get; set; } = false;
    public bool IsErrorable { get; set; } = false;
    public bool IsPublic { get; set; } = false;
    public bool IsImported => ImportLibraryPath != null;
    public Token? ImportLibraryPath { get; set; }
    public List<StatementBase> BodyStatements { get; set; }

    public CallableType AsConcreteType() => new CallableType(FunctionName, ReturnType, Parameters.Select(kv => new FunctionParameter(kv.Key, kv.Value)).ToList(), IsErrorable);

    public bool TryGetVariableType(Token variableName, out ConcreteType variableType)
    {

        if (LocalVariables.TryGetValue(variableName, out var potentialType))
        {
            variableType = potentialType;
            return true;
        }

        if (Parameters.TryGetValue(variableName, out potentialType))
        {
            variableType = potentialType;
            return true;
        }
        variableType = new ConcreteType(BuiltinType.Void);
        return false;
    }

    public CompilerMetadata Metadata { get; private set; } = new();
}


public class FunctionKey
{
    public FunctionKey(Token functionName, List<ConcreteType> parameterTypes)
    {
        FunctionName = functionName;
        ParameterTypes = parameterTypes;
    }
    public Token FunctionName { get; set; }
    public List<ConcreteType> ParameterTypes { get; set; }

}

public class FunctionKeyEqualityComparer : IEqualityComparer<FunctionKey>
{
    public bool Equals(FunctionKey? x, FunctionKey? y)
    {
        if (x == null && y == null) return true;
        if (x == null || y == null) return false;
        if (x.FunctionName.Lexeme != y.FunctionName.Lexeme) return false;
        if (x.ParameterTypes.Count != y.ParameterTypes.Count) return false;
        for (int i = 0; i < x.ParameterTypes.Count; i++)
        {
            if (!x.ParameterTypes[i].IsEqualTo(y.ParameterTypes[i]))
                return false;
        }
        return true;
    }
    public int GetHashCode(FunctionKey obj)
    {
        int hash = obj.FunctionName.Lexeme.GetHashCode();
        return hash;
    }
}

public class TokenEqualityComparer : IEqualityComparer<Token>
{
    public bool Equals(Token? x, Token? y)
    {
        if (x == null && y == null) return true;
        if (x == null || y == null) return false;
        return x.Lexeme == y.Lexeme;
    }
    public int GetHashCode(Token obj)
    {
        return HashCode.Combine(obj.Lexeme);
    }
}