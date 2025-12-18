using Assembler.Core;
using Assembler.Core.Constants;
using Assembler.Core.Models;
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Runtime.InteropServices;


namespace Mineral.Language.Compiler;

public class MineralCompiler
{

    private X86AssemblyContext _asm = new();

    public (bool success, string error) CompileProgram(string outputPath, ProgramContext program)
    {
        try
        {
            // Compilation logic goes here
            _asm = new();

            foreach (var module in program.Modules.Values)
            {
                CompileModule(module);
            }

            _asm.SetEntryPoint("main");
            X86AssemblyGenerator.OutputAsText(_asm, out var text);
        } catch(Exception ex)
        {
            return (false, ex.Message);
        }
        return (true, "");
    }

    private void CompileModule(ModuleContext module)
    {
        foreach (var kv in module.Methods)
        {
            foreach (var function in kv.Value.Overloads.Values)
                RegisterFunction(function);
        }
        foreach (var kv in module.Methods)
        {
            foreach(var function in kv.Value.Overloads.Values)
                CompileFunction(function);
        }
    }

    private void RegisterFunction(FunctionContext functionContext)
    {
        if (functionContext.IsImported)
        {
            var fullLibraryPath = Path.GetFullPath(functionContext.ImportLibraryPath!.Lexeme);
            var importLibrary = _asm.ImportLibraries.Find(x => x.LibraryPath == fullLibraryPath);
            string libraryAlias;
            if (importLibrary != null) libraryAlias = importLibrary.LibraryAlias;
            else
            {
                libraryAlias = _asm.CreateUniqueLabel(); // Randomly generate library alias
                importLibrary = new X86AssemblyContext.ImportLibrary(fullLibraryPath, libraryAlias); 
                _asm.AddImportLibrary(importLibrary);
            }
            var functionSignature = functionContext.GetFunctionSignature();
            var parameters = functionContext.Parameters.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
            var function = new X86AssemblyContext.ImportedFunction(CallingConvention.StdCall, libraryAlias, functionContext.FunctionName.Lexeme, functionSignature, parameters);
            importLibrary.AddImportedFunction(function);
        }
    }

    private void CompileFunction(FunctionContext functionContext)
    {
        var parameters = functionContext.Parameters.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
        var localVariables = functionContext.LocalVariables.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
        var function = new X86Function(CallingConvention.StdCall, functionContext.GetFunctionSignature(), parameters, localVariables, false, ""); // TODO: Exports
        _asm.EnterFunction(function);
        _asm.SetupStackFrame();
        foreach(var statement in functionContext.BodyStatements)
        {
            // Compile each statement
            Compile(statement);
        }
        if (functionContext.BodyStatements.LastOrDefault() is not ReturnStatement && functionContext.BodyStatements.LastOrDefault() is not ErrorStatement)
        {
            _asm.Xor(X86Register.edx, X86Register.edx); // zero out error
            _asm.TeardownStackFrame();
            _asm.Return();
        }
        _asm.ExitFunction();
    }

    private void Compile(StatementBase statement)
    {
        switch (statement)
        {
            case ReturnStatement returnStatement:
                Compile(returnStatement);
                break;
            case ErrorStatement errorStatement:
                Compile(errorStatement);
                break;
            case ConditionalStatement conditionalStatement:
                Compile(conditionalStatement);
                break;
            case AssignmentStatement assignmentStatement:
                Compile(assignmentStatement);
                break;
            default:
                throw new InvalidOperationException($"Unknown statement type '{statement.GetType()}'");
        }
    }

    private void Compile(ReturnStatement returnStatement)
    {
        CompileToRegister(returnStatement.ValueToReturn, X86Register.eax);
        _asm.Xor(X86Register.edx, X86Register.edx); // Zero out error before returning value
        _asm.TeardownStackFrame();
        _asm.Return();
    }

    private void Compile(ErrorStatement errorStatement)
    {
        CompileToRegister(errorStatement.ErrorToReturn, X86Register.edx);
        _asm.TeardownStackFrame();
        _asm.Return();
    }

    private void Compile(ConditionalStatement conditionalStatement)
    {
        CompileToRegister(conditionalStatement.ConditionalTarget, X86Register.edx); // Sort of hacky, but asm optimizer will optimize this away for calls with immediate error checks 
        _asm.Test(X86Register.edx, X86Register.edx);
        var endIfLabel = _asm.CreateUniqueLabel();
        _asm.Jnz(endIfLabel);
        Compile(conditionalStatement.ThenBlock);
        _asm.Label(endIfLabel);
    }

    private void Compile(AssignmentStatement assignmentStatement)
    {
        var errorTarget = assignmentStatement.ErrorTarget;
        if (errorTarget == null && assignmentStatement.Value is CallExpression callExpression && callExpression.Callee.ConcreteType is CallableType callableType && callableType.ReturnType.IsVoidType() && callableType.IsErrorable)
        {
            errorTarget = assignmentStatement.AssignmentTarget;
            CompileForError(callExpression);
        } else
        {
            CompileToRegister(assignmentStatement.Value, X86Register.eax); // Push value to assign onto the stack
            RegisterOffset assignmentTarget;
            if (assignmentStatement.AssignmentTarget is IdentifierLValue identifierLValue)
            {
                assignmentTarget = _asm.GetIdentifierOffset(identifierLValue.VariableName.Lexeme, out _);
            }
            else if (assignmentStatement.AssignmentTarget is InstanceMemberLValue instanceMemberLValue)
            {
                CompileToRegister(instanceMemberLValue.Instance, X86Register.ecx); // Since expressions by default only use eax and edx, we use ecx so we have no need to store the assignment value temporarily
                if (instanceMemberLValue.Instance.ConcreteType is StructType instanceType && instanceType.TryGetMemberOffset(instanceMemberLValue.Member, out var offset))
                {
                    assignmentTarget = Offset.Create(X86Register.ecx, offset);
                }
                else throw new InvalidOperationException($"unable to find member '{instanceMemberLValue.Member.Lexeme}' in type '{instanceMemberLValue.Instance.ConcreteType}' or '{instanceMemberLValue.Instance.ConcreteType}' is not a struct type");

            }
            else throw new NotSupportedException($"lvalue of type '{assignmentStatement.AssignmentTarget.GetType()}' is not supported");

            CompileAsAssignmentValue(assignmentTarget, assignmentStatement.Value);
        }
            


    
        // Errors returned in edx
        if (errorTarget != null)
        {
            if (errorTarget is IdentifierLValue errIdentifierLValue)
            {
                var offset = _asm.GetIdentifierOffset(errIdentifierLValue.VariableName.Lexeme, out _);
                _asm.Mov(offset, X86Register.edx);
            }
            else if (errorTarget is InstanceMemberLValue instanceMemberLValue)
            {
                CompileToRegister(instanceMemberLValue.Instance, X86Register.ecx); // Since expressions by default only use eax and edx, we use ecx so we have no need to store the assignment value temporarily
                if (instanceMemberLValue.Instance.ConcreteType is StructType instanceType && instanceType.TryGetMemberOffset(instanceMemberLValue.Member, out var offset))
                {
                    _asm.Mov(Offset.Create(X86Register.ecx, offset), X86Register.edx);
                }
                else throw new InvalidOperationException($"unable to find member '{instanceMemberLValue.Member.Lexeme}' in type '{instanceMemberLValue.Instance.ConcreteType}' or '{instanceMemberLValue.Instance.ConcreteType}' is not a struct type");

            }
            else throw new NotSupportedException($"lvalue of type '{assignmentStatement.AssignmentTarget.GetType()}' is not supported");
        }

    }

    private void Compile(ExpressionBase expression)
    {
        // all expressions end up with their returned value in eax
        CompileToRegister(expression, X86Register.eax);
        
    }

    private void CompileForError(CallExpression callExpression)
    {
        if (!(callExpression.Callee.ConcreteType is CallableType callableType && callableType.IsErrorable)) throw new InvalidOperationException("can only optimize error call for errorable calls");
        for (int i = callExpression.Arguments.Count - 1; i >= 0; i--)
        {
            var argument = callExpression.Arguments[i];
            CompileAsArgument(argument);
        }
        if (callExpression.FunctionContext != null)
        {
            // it is a direct call
            _asm.Call(callExpression.FunctionContext.GetFunctionSignature(), false);
        }
        else
        {
            // Indirect call
            CompileToRegister(callExpression.Callee, X86Register.eax);
            _asm.Call(X86Register.eax);
        }

    }


    private void CompileToRegister(ExpressionBase expression, X86Register register)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                CompileToRegister(callExpression, register);
                break;
            case IdentifierExpression identifierExpression:
                CompileToRegister(identifierExpression, register);
                break;
            case MemberAccessExpression memberAccessExpression:
                CompileToRegister(memberAccessExpression, register);
                break;
            case LiteralExpression literalExpression:
                CompileToRegister(literalExpression, register);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }


    private void CompileToRegister(CallExpression callExpression, X86Register register)
    {
        if (register == X86Register.edx) throw new InvalidOperationException("cannot override edx in call due to error return");
        for(int i = callExpression.Arguments.Count - 1; i >= 0; i--)
        {
            var argument = callExpression.Arguments[i];
            CompileAsArgument(argument);
        }
        if (callExpression.FunctionContext != null)
        {
            // it is a direct call
            _asm.Call(callExpression.FunctionContext.GetFunctionSignature(), callExpression.FunctionContext.IsImported);
        }
        else
        {
            // Indirect call
            CompileToRegister(callExpression.Callee, X86Register.eax);
            _asm.Call(X86Register.eax);
        }
        if (register != X86Register.eax)
        {
            if (callExpression.Callee.ConcreteType is CallableType callableType && callableType.ReturnType.IsVoidType()) return;
            _asm.Mov(register, X86Register.eax);
        }
    }

    private void CompileToRegister(MemberAccessExpression memberAccessExpression, X86Register register)
    {
        CompileToRegister(memberAccessExpression.Instance, register);
        if (memberAccessExpression.Instance.ConcreteType is not StructType instanceType)
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}'");
        if (!instanceType.TryGetMemberOffset(memberAccessExpression.MemberToAccess, out var offset))
            throw new InvalidOperationException($"type '{instanceType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        _asm.Mov(register, Offset.Create(register, offset));
        
    }
    private void CompileToRegister(IdentifierExpression identifierExpression, X86Register register)
    {
        if (identifierExpression.FunctionContext != null)
        {
            _asm.Mov(register, identifierExpression.FunctionContext.GetFunctionSignature());
        }

        else _asm.Mov(register, _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _));
    }

    private void CompileToRegister(LiteralExpression literalExpression, X86Register register)
    {
        if (literalExpression.Value is int i) _asm.Mov(register, i);
        else if (literalExpression.Value is float flt)
        {
            var label = _asm.AddSinglePrecisionFloatingPointData(flt);
            _asm.Mov(register, label);
        }
        else if (literalExpression.Value is string str)
        {
            var label = _asm.AddStringData(str);
            _asm.Mov(register, label);
        }
        else if (literalExpression.Value is null) _asm.Mov(register, 0);
        else throw new InvalidOperationException($"literal of type '{literalExpression.ConcreteType}' is not supported");
    }



    #region AsAssignment

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, ExpressionBase expression)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                CompileAsAssignmentValue(assignmentTarget, callExpression);
                break;
            case IdentifierExpression identifierExpression:
                CompileAsAssignmentValue(assignmentTarget, identifierExpression);
                break;
            case MemberAccessExpression memberAccessExpression:
                CompileAsAssignmentValue(assignmentTarget, memberAccessExpression);
                break;
            case LiteralExpression literalExpression:
                CompileAsAssignmentValue(assignmentTarget, literalExpression);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, IdentifierExpression identifierExpression)
    {
        CompileToRegister(identifierExpression, X86Register.eax);
        _asm.Mov(assignmentTarget, X86Register.eax);
    }

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, MemberAccessExpression memberAccessExpression)
    {
        CompileToRegister(memberAccessExpression, X86Register.eax);
        _asm.Mov(assignmentTarget, X86Register.eax);
    }

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, CallExpression callExpression)
    {
        CompileToRegister(callExpression, X86Register.eax);
        _asm.Mov(assignmentTarget, X86Register.eax);
    }

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, LiteralExpression literalExpression)
    {
        if (literalExpression.Value is int i) _asm.Mov(assignmentTarget, i);
        else if (literalExpression.Value is float flt)
        {
            var label = _asm.AddSinglePrecisionFloatingPointData(flt);
            _asm.Mov(assignmentTarget, label);
        }
        else if (literalExpression.Value is string str)
        {
            var label = _asm.AddStringData(str);
            _asm.Mov(assignmentTarget, label);
        }
        else if (literalExpression.Value is null) _asm.Mov(assignmentTarget, 0);
        else throw new InvalidOperationException($"literal of type '{literalExpression.ConcreteType}' is not supported");
    }

    #endregion

    #region AsArgument

    private void CompileAsArgument(ExpressionBase expression)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                CompileAsArgument(callExpression);
                break;
            case IdentifierExpression identifierExpression:
                CompileAsArgument(identifierExpression);
                break;
            case MemberAccessExpression memberAccessExpression:
                CompileAsArgument(memberAccessExpression);
                break;
            case LiteralExpression literalExpression:
                CompileAsArgument(literalExpression);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }

    private void CompileAsArgument(IdentifierExpression identifierExpression)
    {
        CompileToRegister(identifierExpression, X86Register.eax);
        _asm.Push(X86Register.eax);
    }

    private void CompileAsArgument(MemberAccessExpression memberAccessExpression)
    {
        CompileToRegister(memberAccessExpression, X86Register.eax);
        _asm.Push(X86Register.eax);
    }

    private void CompileAsArgument(CallExpression callExpression)
    {
        CompileToRegister(callExpression, X86Register.eax);
        // TODO: check for error to short circuit call? if so, which direction?
        _asm.Push(X86Register.eax);
    }

    private void CompileAsArgument(LiteralExpression literalExpression)
    {
        if (literalExpression.Value is int i) _asm.Push(i);
        else if (literalExpression.Value is float flt)
        {
            var label = _asm.AddSinglePrecisionFloatingPointData(flt);
            _asm.Push(label);
        }
        else if (literalExpression.Value is string str)
        {
            var label = _asm.AddStringData(str);
            _asm.Push(label);
        }
        else if (literalExpression.Value is null) _asm.Push(0);
        else throw new InvalidOperationException($"literal of type '{literalExpression.ConcreteType}' is not supported");
    }


    #endregion
}



public abstract class MemoryLocation
{
    public abstract bool IsRegister { get; }
    public abstract bool IsMemory { get; }
    public abstract bool IsLabel { get; }

    public static RegisterMemoryLocation Register(X86Register register) => new RegisterMemoryLocation(register);
    public static StackMemoryLocation Memory(RegisterOffset registerOffset) => new StackMemoryLocation(registerOffset);
    public static DirectMemoryLocation Direct(string label) => new DirectMemoryLocation(label); 
}

public class RegisterMemoryLocation : MemoryLocation
{
    public RegisterMemoryLocation(X86Register register)
    {
        Register = register;
    }
    public X86Register Register { get; set; }


    public override bool IsRegister => true;
    public override bool IsMemory => false;
    public override bool IsLabel => false;
}

public class StackMemoryLocation : MemoryLocation
{
    public StackMemoryLocation(RegisterOffset registerOffset)
    {
        RegisterOffset = registerOffset;
    }
    public RegisterOffset RegisterOffset { get; set; }


    public override bool IsRegister => false;
    public override bool IsMemory => true;
    public override bool IsLabel => false;
}

public class DirectMemoryLocation : MemoryLocation
{
    public DirectMemoryLocation(string label)
    {
        Label = label;
    }
    public string Label { get; set; }


    public override bool IsRegister => false;
    public override bool IsMemory => false;
    public override bool IsLabel => true;
}



