
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Runtime.InteropServices;
using X86_64Assembler;


namespace Mineral.Language.Compiler;

public class MineralCompiler
{

    private X86AssemblyContext _asm = new();

    public (bool success, string error) CompileProgram(string outputPath, ProgramContext program, string entryPoint = "main.main@int", OutputTarget outputTarget = OutputTarget.Exe)
    {
        try
        {
            // Compilation logic goes here
            _asm = new();

            foreach (var module in program.Modules.Values)
            {
                CompileModule(module);
            }

            var error = _asm.OutputX86Assembly64Bit(outputTarget, entryPoint, Path.GetFileName(outputPath), out var peFile);
            var bytes = peFile.AssembleProgram(entryPoint);
            File.WriteAllBytes(outputPath, bytes);
            var text = peFile.EmitDecodedAssembly(entryPoint);
            File.WriteAllText(outputPath, text);
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
            var libraryPath = functionContext.ImportLibraryPath!.Lexeme;
            var parameters = functionContext.Parameters.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();

            _asm.AddImport(libraryPath, CallingConvention.Cdecl, functionContext.GetDecoratedFunctionLabel(), functionContext.FunctionName.Lexeme, parameters);
        }
    }

    private void CompileFunction(FunctionContext functionContext)
    {
        if (functionContext.IsImported) return; 
        var parameters = functionContext.Parameters.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
        var localVariables = functionContext.LocalVariables.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
        var function = new X86Function(CallingConvention.StdCall, functionContext.GetFunctionSignature(), parameters, localVariables, false, ""); // TODO: Exports
        _asm.EnterFunction(function);

        foreach(var statement in functionContext.BodyStatements)
        {
            // Compile each statement
            Compile(statement);
        }
        if (functionContext.BodyStatements.LastOrDefault() is not ReturnStatement && functionContext.BodyStatements.LastOrDefault() is not ErrorStatement)
        {
            _asm.Xor(Reg64.RDX, (RM64)Reg64.RDX); // zero out error
            _asm.TearDownStackFrame();
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
        CompileToRegister(returnStatement.ValueToReturn, Reg64.RAX);
        _asm.Xor(Reg64.RDX, (RM64)Reg64.RDX); // Zero out error before returning value
        _asm.TearDownStackFrame();
        _asm.Return();
    }

    private void Compile(ErrorStatement errorStatement)
    {
        CompileToRegister(errorStatement.ErrorToReturn, Reg64.RDX);
        _asm.TearDownStackFrame();
        _asm.Return();
    }

    private void Compile(ConditionalStatement conditionalStatement)
    {
        var memoryLocation = CompileAndReturnAssignmentTargetFromLValue(conditionalStatement.ConditionalTarget);
        if (memoryLocation != null) _asm.Mov(Reg64.RDX, memoryLocation);
        _asm.Test(Reg64.RDX, Reg64.RDX);
        var endIfLabel = _asm.CreateUniqueLabel("C");
        _asm.Jz(Rel32.Create(endIfLabel));
        foreach(var statment in conditionalStatement.ThenBlock)
            Compile(statment);
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
            var assignmentTarget = CompileAndReturnAssignmentTargetFromLValue(assignmentStatement.AssignmentTarget);
            
            if (assignmentTarget != null) CompileAsAssignmentValue(assignmentTarget, assignmentStatement.Value);
            else CompileToRegister(assignmentStatement.Value, Reg64.RAX);
        }
            


    
        // Errors returned in edx
        if (errorTarget != null)
        {
            var errorTargetMemoryLocation = CompileAndReturnAssignmentTargetFromLValue(errorTarget);
            if (errorTargetMemoryLocation != null) _asm.Mov(errorTargetMemoryLocation, Reg64.RDX);
            else
            {
                // Pass for discard
            }
        }

    }

    private Mem64? CompileAndReturnAssignmentTargetFromLValue(LValue lValue)
    {
        if (lValue is IdentifierLValue identifierLValue)
        {
            return _asm.GetIdentifierOffset(identifierLValue.VariableName.Lexeme, out _);        
        }
        else if (lValue is InstanceMemberLValue instanceMemberLValue)
        {
            var instanceMemoryLocation = CompileAndReturnAssignmentTargetFromLValue(instanceMemberLValue.Instance);
            if (instanceMemoryLocation == null) throw new InvalidOperationException("Invalid discard as left hand side of member access");
            if (instanceMemberLValue.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(instanceMemberLValue.Member, out var isReferenceType, out var offset))
            {
                if (isReferenceType)
                {
                    _asm.Mov(X86Register.ecx, instanceMemoryLocation); // Since expressions by default only use eax and edx, we use ecx so we have no need to store the assignment value temporarily
                    return Offset.Create(X86Register.ecx, offset);
                }

                // TODO
                _asm.Lea(X86Register.ecx, instanceMemoryLocation); // Since expressions by default only use eax and edx, we use ecx so we have no need to store the assignment value temporarily
                return Offset.Create(X86Register.ecx, -offset);
            }
            else throw new InvalidOperationException($"unable to find member '{instanceMemberLValue.Member.Lexeme}' in type '{instanceMemberLValue.Instance.ConcreteType}' or '{instanceMemberLValue.Instance.ConcreteType}' is not a struct type");
        }
        else if (lValue is DiscardLValue)
        {
            // Pass
            return null;
        }
        else throw new NotSupportedException($"lvalue of type '{lValue.GetType()}' is not a supported assignment target");
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
            _asm.Call(callExpression.FunctionContext.GetDecoratedFunctionLabel(), false);
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
            case ReferenceExpression referenceExpression:
                CompileToRegister(referenceExpression, register);
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
            _asm.Call(callExpression.FunctionContext.GetDecoratedFunctionLabel(), callExpression.FunctionContext.IsImported);
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
        var instanceType = memberAccessExpression.Instance.ConcreteType;
        if (!instanceType.TryGetMemberOffsetFromStructOrReference(memberAccessExpression.MemberToAccess, out var isReferenceType, out var offset))
        {
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}' or type '{instanceType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        }
        if (isReferenceType)
        {
            CompileToRegister(memberAccessExpression.Instance, register);
            _asm.Mov(register, Offset.Create(register, offset));
            return;
        }
        // Since you cannot assign struct types to anything other than local variables
        if (memberAccessExpression.Instance is not IdentifierExpression identifierExpression)
            throw new InvalidOperationException($"unexpected expression type evaluated to struct '{memberAccessExpression.Instance.GetType()}'");
        var lhsOffset = _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
        lhsOffset.Offset -= offset;
        _asm.Mov(register, lhsOffset);
    }

    private void CompileToRegister(IdentifierExpression identifierExpression, X86Register register)
    {
        if (identifierExpression.FunctionContext != null)
        {
            _asm.Mov(register, identifierExpression.FunctionContext.GetDecoratedFunctionLabel());
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

    private void CompileToRegister(ReferenceExpression referenceExpression, X86Register register)
    {
        if (referenceExpression.Instance != null)
        {
            CompileToRegister(referenceExpression.Instance, register);
            if (referenceExpression.Instance.ConcreteType is not StructType instanceType)
                throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{referenceExpression.Instance.ConcreteType}'");
            if (!instanceType.TryGetMemberOffset(referenceExpression.MemberToAccess, out var offset))
                throw new InvalidOperationException($"type '{instanceType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
            _asm.Lea(register, Offset.Create(register, -offset));
            return;
        }

        var memoryLocation = _asm.GetIdentifierOffset(referenceExpression.MemberToAccess.Lexeme, out _);
        _asm.Lea(register, memoryLocation);
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
            case StackAllocateExpression stackAllocateExpression:
                CompileAsAssignmentValue(assignmentTarget, stackAllocateExpression);
                break;
            case ReferenceExpression referenceExpression:
                CompileAsAssignmentValue(assignmentTarget, referenceExpression);
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
        var instanceType = memberAccessExpression.Instance.ConcreteType;
        if (!instanceType.TryGetMemberOffsetFromStructOrReference(memberAccessExpression.MemberToAccess, out var isReferenceType, out var offset))
        {
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}' or type '{instanceType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        }
        if (isReferenceType)
        {
            CompileToRegister(memberAccessExpression.Instance, X86Register.eax);
            _asm.Mov(X86Register.eax, Offset.Create(X86Register.eax, offset));
            _asm.Mov(assignmentTarget, X86Register.eax);
            return;
        }
        // Since you cannot assign struct types to anything other than local variables
        if (memberAccessExpression.Instance is not IdentifierExpression identifierExpression)
            throw new InvalidOperationException($"unexpected expression type evaluated to struct '{memberAccessExpression.Instance.GetType()}'");
        var lhsOffset = _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
        lhsOffset.Offset -= offset;
        _asm.Mov(X86Register.eax, lhsOffset);
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

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, StackAllocateExpression stackAllocateExpression)
    {
        // Nothing needs to be done here since the function declaration will handle reserving space for local variables 
    }

    private void CompileAsAssignmentValue(RegisterOffset assignmentTarget, ReferenceExpression referenceExpression)
    {
        CompileToRegister(referenceExpression, X86Register.eax);
        _asm.Mov(assignmentTarget, X86Register.eax);
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
            case ReferenceExpression referenceExpression:
                CompileAsArgument(referenceExpression);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }

    private void CompileAsArgument(IdentifierExpression identifierExpression)
    {
        var memoryLocation = _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
        _asm.Push(memoryLocation);
    }

    private void CompileAsArgument(MemberAccessExpression memberAccessExpression)
    {
        var instanceType = memberAccessExpression.Instance.ConcreteType;
        if (!instanceType.TryGetMemberOffsetFromStructOrReference(memberAccessExpression.MemberToAccess, out var isReferenceType, out var offset))
        {
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}' or type '{instanceType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        }
        if (isReferenceType)
        {
            CompileToRegister(memberAccessExpression.Instance, X86Register.eax);
            _asm.Push(Offset.Create(X86Register.eax, offset));
            return;
        }
        // Since you cannot assign struct types to anything other than local variables
        if (memberAccessExpression.Instance is not IdentifierExpression identifierExpression)
            throw new InvalidOperationException($"unexpected expression type evaluated to struct '{memberAccessExpression.Instance.GetType()}'");
        var lhsOffset = _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
        lhsOffset.Offset -= offset;
        _asm.Push(lhsOffset);
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

    private void CompileAsArgument(ReferenceExpression referenceExpression)
    {
        CompileToRegister(referenceExpression, X86Register.eax);
        _asm.Push(X86Register.eax);
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



