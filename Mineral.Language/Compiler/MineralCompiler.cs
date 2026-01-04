
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Runtime.InteropServices;
using Tokenizer.Core.Models;
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

    private int _stackSlotIndex = 0;
    private Mem64 NextAvailableLocalStackSlot()
    {
        var mem = _asm.GetIdentifierOffset($"%__iterm{_stackSlotIndex}", out _);
        _stackSlotIndex++;
        return mem;
    }

    private void FreeLastStackSlot()
    {
        // Frees the most recently used stack slot
        _stackSlotIndex--;
        if (_stackSlotIndex < 0) throw new InvalidOperationException("stack slot index went below zero");
    }

    private void CompileFunction(FunctionContext functionContext)
    {
        _stackSlotIndex = 0;
        if (functionContext.IsImported) return; 
        var parameters = functionContext.Parameters.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
        var localVariables = functionContext.LocalVariables.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();

        var additonalStackSlotsNeeded = GetAdditonalStackSlotsNeeded(functionContext);
        for (int i = 0; i < additonalStackSlotsNeeded; i++)
        {
            localVariables.Add(new X86FunctionLocalData($"%__iterm{i}", 8));
        }

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


    #region Happy path expression compilation

    private RM Compile(ExpressionBase expression)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                return Compile(callExpression);
            case IdentifierExpression identifierExpression:
                return Compile(identifierExpression);
            case MemberAccessExpression memberAccessExpression:
                return Compile(memberAccessExpression);
            case LiteralExpression literalExpression:
                return Compile(literalExpression);
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }
    private Mem64 Compile(IdentifierExpression identifierExpression)
    {
        return _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
    }

    private Mem64 Compile(MemberAccessExpression memberAccessExpression)
    {
        var instanceLocation = Compile(memberAccessExpression.Instance);
        if (!memberAccessExpression.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(memberAccessExpression.MemberToAccess, out var isReferenceType, out var offset))
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}' or type '{memberAccessExpression.Instance.ConcreteType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        if (!isReferenceType)
        {
            return HandleRM(instanceLocation,
                mem =>
                {
                    return Mem64.Create((Reg64)mem.Register, mem.Displacement.Offset + offset);
                },
                reg =>
                {
                    throw new Exception("cannot return struct in register");
                });
        }
        return HandleRM(instanceLocation,
                mem =>
                {
                    _asm.Mov(Reg64.RAX, mem); 
                    return Mem64.Create(Reg64.RAX, offset);
                },
                reg =>
                {
                    return Mem64.Create(reg, offset);
                });
    }

    private Reg64 Compile(ReferenceExpression referenceExpression)
    {
        if (referenceExpression.Instance == null)
        {
            var location = _asm.GetIdentifierOffset(referenceExpression.MemberToAccess.Lexeme, out _);
            _asm.Lea(Reg64.RAX, location);
            return Reg64.RAX;
        }
        var rm = Compile(referenceExpression.Instance);
        if (!referenceExpression.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(referenceExpression.MemberToAccess, out var isReferenceType, out var offset))
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{referenceExpression.Instance.ConcreteType}' or type '{referenceExpression.Instance.ConcreteType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
        if (!isReferenceType)
        {
            return HandleRM(rm,
                mem =>
                {
                    _asm.Lea(Reg64.RAX, Mem64.Create((Reg64)mem.Register, mem.Displacement.Offset + offset));
                    return Reg64.RAX;
                },
                reg =>
                {
                    throw new Exception("cannot return struct in register");
                });
        }
        return HandleRM(rm,
                mem =>
                {
                    _asm.Mov(Reg64.RAX, mem); 
                    _asm.Lea(Reg64.RAX, Mem64.Create(Reg64.RAX, offset));
                    return Reg64.RAX;
                },
                reg =>
                {
                    _asm.Lea(Reg64.RAX, Mem64.Create(reg, offset));
                    return Reg64.RAX;
                });
    }

    private RM Compile(BinaryExpression binaryExpression)
    {
        var rmLeft = Compile(binaryExpression.Left);
        if (binaryExpression.Right.Metadata.ContainsCall)
        {
            var intermStackSlot = NextAvailableLocalStackSlot();
            HandleRM(rmLeft,
                mem =>
                {
                    _asm.Mov(Reg64.RAX, mem);
                    _asm.Mov(intermStackSlot, Reg64.RAX);
                },
                reg =>
                {
                    _asm.Mov(intermStackSlot, reg);
                },
                xmm =>
                {
                    _asm.Movss(intermStackSlot, xmm);
                });

            var rmRight = Compile(binaryExpression.Right);


        }
    }

    private void HandleOperation(RM left, RM right, OperatorType op)
    {
        if (left is Reg64 regLeft && right is Reg64 regRight)
        {

        }
    }

    private void HandleOperation(Reg64 left, Reg64 right, OperatorType op)
    {

    }

    private Reg64 HandleOperation(Reg64 left, Mem64 right, OperatorType op)
    {
        if (op == OperatorType.Addition)
        {
            _asm.Add(left, right);
        }
        else if (op == OperatorType.Subtraction)
        {
            _asm.Sub(left, right);
        }
        else if (op == OperatorType.Multiplication)
        {
            _asm.Imul(left, right);
        }
        else if (op == OperatorType.Division || op == OperatorType.Modulus)
        {
            if (left != Reg64.RAX) _asm.Mov(Reg64.RAX, (RM64)left);
            _asm.Cqo();
            _asm.Div((RM64)right);
            return op == OperatorType.Division ? Reg64.RAX : Reg64.RDX;
        }


    }
    private void HandleOperation(Reg64 left, Xmm128 right, OperatorType op)
    {

    }


    private void HandleOperation(Mem64 left, Reg64 right, OperatorType op)
    {

    }

    private void HandleOperation(Mem64 left, Mem64 right, OperatorType op)
    {

    }

    private void HandleOperation(Mem64 left, Xmm128 right, OperatorType op)
    {

    }

    private void HandleOperation(Xmm128 left, Reg64 right, OperatorType op)
    {

    }

    private void HandleOperation(Xmm128 left, Mem64 right, OperatorType op)
    {

    }

    private void HandleOperation(Xmm128 left, Xmm128 right, OperatorType op)
    {

    }


    private RM Compile(CallExpression callExpression)
    {
        // Will always return in RAX or Xmm0

        // align stack

        // if argument count is odd and greather than 4, we need to subtract an additional 8 bytes from RSP to keep RSP 16 byte aligned
        var additionalStackToSubtract = 0;
        if (callExpression.Arguments.Count < 4) additionalStackToSubtract += (4 - callExpression.Arguments.Count) * 8;
        else if (callExpression.Arguments.Count > 4 && ((callExpression.Arguments.Count % 2) == 1)) additionalStackToSubtract = 8;
        if (additionalStackToSubtract > 0)
        {
            _asm.Sub(Reg64.RSP, additionalStackToSubtract); // align stack to 16 byte boundary
        }

        for (int i = callExpression.Arguments.Count - 1; i >= 0; i--)
            {
                var argument = callExpression.Arguments[i];
                CompileAsArgument(argument);
            }
        if (callExpression.FunctionContext != null)
        {
            if (callExpression.FunctionContext.IsImported)
            {
                // imported function call
                for (int i = 0; i < callExpression.Arguments.Count; i++)
                {
                    var argument = callExpression.Arguments[i];
                    var mem = Mem64.Create(Reg64.RSP, i * 8); // RSP points directly at last pushed value, so first arg should be at offset 0 from rsp
                    if (argument.IsFloatingPointType())
                    {
                        _asm.Movss(SelectXmmRegister(i), mem);
                    }
                    // TODO add double precision support
                    else
                    {
                        _asm.Mov(SelectGeneralRegister(i), mem);
                    }
                }
                _asm.Call((RM64)Mem64.Create(callExpression.FunctionContext.GetDecoratedFunctionLabel()));
            }
            // it is a direct call
            else _asm.Call(Rel32.Create(callExpression.FunctionContext.GetDecoratedFunctionLabel()));
        }
        else
        {
            // Indirect call
            var indirect = Compile(callExpression.Callee);
            if (indirect is RM64 rm)
            {
                _asm.Call(rm);
            }
            else throw new InvalidOperationException($"unexpected rm type in call. rm type was '{indirect.GetType()}'");
        }
        _asm.Add(Reg64.RSP, (callExpression.Arguments.Count * 8) + additionalStackToSubtract); // clear stack of arguments
        if (callExpression.IsFloatingPointType())
        {
            return Xmm128.XMM0;
        }
        return Reg64.RAX;
    }

    private Xmm128 SelectXmmRegister(int index)
    {
        switch (index)
        {
            case 0:
                return Xmm128.XMM0;
            case 1: return Xmm128.XMM1;
            case 2: return Xmm128.XMM2;
            case 3: return Xmm128.XMM3;
            default:
                throw new NotSupportedException($"xmm arguments not supported past parameter 4");
        }
    }

    private Reg64 SelectGeneralRegister(int index)
    {
        // select register based on argument index according to windows x64 abi
        switch (index)
        {
            case 0:
                return Reg64.RCX;
            case 1: return Reg64.RDX;
            case 2: return Reg64.R8;
            case 4: return Reg64.R9;
            default:
                throw new NotSupportedException("general register arguments not supported past parameter 4");
        }
    }

    #endregion

    private Mem64 HandleRM(RM rm, Func<Mem64, Mem64> handleMemory, Func<Reg64, Mem64> handleRegister, Func<Xmm128, Mem64>? handleXmm)
    {
        if (rm is Reg64 reg)
            return handleRegister(reg);
        else if (rm is Mem64 mem)
            return handleMemory(mem);
        else if (handleXmm != null && rm is Xmm128 xmm)
            return handleXmm(xmm);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }

    private Reg64 HandleRM(RM rm, Func<Mem64, Reg64> handleMemory, Func<Reg64, Reg64> handleRegister, Func<Xmm128, Reg64>? handleXmm)
    {
        if (rm is Reg64 reg)
            return handleRegister(reg);
        else if (rm is Mem64 mem)
            return handleMemory(mem);
        else if (handleXmm != null && rm is Xmm128 xmm)
            return handleXmm(xmm);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }

    private void HandleRM(RM rm, Action<Mem64> handleMemory, Action<Reg64> handleRegister, Action<Xmm128>? handleXmm)
    {
        if (rm is Reg64 reg)
            handleRegister(reg);
        else if (rm is Mem64 mem)
            handleMemory(mem);
        else if (handleXmm != null && rm is Xmm128 xmm)
            handleXmm(xmm);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
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


    #region Expressions

    private RM CompileToPreferred(ExpressionBase expression, RM preferred)
    {
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                CompileToPreferred(binaryExpression, preferred);
                break;
            case CallExpression callExpression:
                CompileToPreferred(callExpression, preferred);
                break;
            case IdentifierExpression identifierExpression:
                CompileToPreferred(identifierExpression, preferred);
                break;
            case MemberAccessExpression memberAccessExpression:
                CompileToPreferred(memberAccessExpression, preferred);
                break;
            case LiteralExpression literalExpression:
                CompileToPreferred(literalExpression, preferred);
                break;
            case ReferenceExpression referenceExpression:
                CompileToPreferred(referenceExpression, preferred);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }

    private RM CompileToPreferred(BinaryExpression binaryExpression, RM preferred)
    {
        RM first;
        RM second;
        if (binaryExpression.Operator == OperatorType.Division && binaryExpression.IsIntegerType())
        {
            preferred = Reg64.RAX;
        }
        if (binaryExpression.Left.Metadata.SU >= binaryExpression.Right.Metadata.SU)
        {
            first = CompileToPreferred(binaryExpression.Left, preferred);
            second = CompileToPreferred(binaryExpression.Right, );

        }
    }

    private RM CompileToPreferred(CallExpression callExpression, RM preferred)
    {
        // TODO
    }

    private RM CompileToPreferred(IdentifierExpression identifierExpression, RM preferred)
    {
        // TODO
    }

    private RM CompileToPreferred(MemberAccessExpression memberAccessExpression, RM preferred)
    {
        // TODO
    }

    private RM CompileToPreferred(LiteralExpression literalExpression, RM preferred)
    {
        // TODO
    }

    private RM CompileToPreferred(ReferenceExpression referenceExpression, RM preferred)
    {
        // TODO
    }


    private RM CompileToMemory(ExpressionBase expression)
    {
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                CompileToMemory(binaryExpression);
                break;
            case CallExpression callExpression:
                CompileToMemory(callExpression);
                break;
            case IdentifierExpression identifierExpression:
                CompileToMemory(identifierExpression);
                break;
            case MemberAccessExpression memberAccessExpression:
                CompileToMemory(memberAccessExpression);
                break;
            case LiteralExpression literalExpression:
                CompileToMemory(literalExpression);
                break;
            case ReferenceExpression referenceExpression:
                CompileToMemory(referenceExpression);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }

    private RM CompileToMemory(BinaryExpression binaryExpression)
    {
        RM first;
        RM second;
        if (binaryExpression.Operator == OperatorType.Division && binaryExpression.IsIntegerType())
        {
            preferred = Reg64.RAX;
        }
        if (binaryExpression.Left.Metadata.SU >= binaryExpression.Right.Metadata.SU)
        {
            first = CompileToMemory(binaryExpression.Left);
            second = CompileToMemory(binaryExpression.Right, );

        }
    }

    private RM CompileToMemory(CallExpression callExpression)
    {
        // TODO
    }

    private RM CompileToMemory(IdentifierExpression identifierExpression)
    {
        // TODO
        return _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
    }

    private Mem64 CompileToMemory(MemberAccessExpression memberAccessExpression)
    {
        // TODO
        var instanceLocation = CompileToMemory(memberAccessExpression.Instance);

        if (!memberAccessExpression.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(memberAccessExpression.MemberToAccess, out var isReferenceType, out var offset))
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}' or type '{memberAccessExpression.Instance.ConcreteType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        if (instanceLocation is Mem64 mem)
        {
            mem.Displacement.Offset += offset;
            return mem;
        }
        if (instanceLocation is Reg64 reg)
        {
            _asm.Add(reg, offset);
            return reg;
        }
        else
            throw new InvalidOperationException("unexpected RM type for member access");
    }

    private RM CompileToMemory(LiteralExpression literalExpression)
    {
        // TODO
    }


    private Xmm128? Compile(CallExpression callExpression)
    {


        // Will always return in RAX or Xmm0
    }

    private Mem64 Compile(IdentifierExpression identifierExpression)
    {
        return _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
    }

    private Immediate Compile(LiteralExpression literalExpression)
    {

    }

    private Mem64 Compile(MemberAccessExpression memberAccessExpression)
    {

    }

    private RM Compile(BinaryExpression binaryExpression)
    {


        // result always in Reg64 or Xmm128
    }


    private Reg64 HandleReferenceExpression(CallExpression instance, ReferenceExpression referenceExpression)
    {
        Compile(instance);
        if (instance.ConcreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType structType)
        {
            if (!structType.TryGetMemberOffset(referenceExpression.MemberToAccess, out var offset))
                throw new InvalidOperationException($"type '{structType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
            var freeRegister = GetFreeRegister();
            _asm.Lea(freeRegister, Mem64.Create(Reg64.RAX, offset));
            return freeRegister;
        }
        else throw new InvalidOperationException("expected reference type as left hand side of member");   
    }

    private Reg64 HandleReferenceExpression(MemberAccessExpression instance, ReferenceExpression referenceExpression)
    {
        var mem = CompileToMemory(instance);
        
        if (instance.ConcreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType structType)
        {
            if (!structType.TryGetMemberOffset(referenceExpression.MemberToAccess, out var offset))
                throw new InvalidOperationException($"type '{structType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
            var freeRegister = GetFreeRegister();
            _asm.Lea(freeRegister, mem);
            _asm.Add(freeRegister, offset);
            return freeRegister;
        }
        else throw new InvalidOperationException("expected reference type as left hand side of member");
    }

    private Reg64 HandleReferenceExpression(IdentifierExpression instance, ReferenceExpression referenceExpression)
    {
        var mem = _asm.GetIdentifierOffset(instance.Symbol.Lexeme, out _);

        if (instance.ConcreteType is ReferenceType referenceType && referenceType.ReferencedType is StructType structType)
        {
            if (!structType.TryGetMemberOffset(referenceExpression.MemberToAccess, out var offset))
                throw new InvalidOperationException($"type '{structType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
            var freeRegister = GetFreeRegister();
            _asm.Lea(freeRegister, mem);
            _asm.Add(freeRegister, offset);
            return freeRegister;
        }
        else throw new InvalidOperationException("expected reference type as left hand side of member");
    }



    private RM HandleBinary(CallExpression left, CallExpression right, OperatorType operatorType)
    {
        // TODO

        if (operatorType == OperatorType.Division && left.IsIntegerType())
        {
            // division requires rax and rdx

            Compile(right);
            var preservedRegister = GetCalleePreservedRegister();
            _asm.Mov((RM64)preservedRegister, Reg64.RAX);
            Compile(left);
            _asm.Cqo(); // sign extend rax into rdx:rax
            _asm.Idiv(preservedRegister);
            return Reg64.RAX;
        }
        if (operatorType == OperatorType.Modulus && left.IsIntegerType())
        {
            // division requires rax and rdx

            Compile(right);
            var preservedRegister = GetCalleePreservedRegister();
            _asm.Mov((RM64)preservedRegister, Reg64.RAX);
            Compile(left);
            _asm.Cqo(); // sign extend rax into rdx:rax
            _asm.Idiv(preservedRegister);
            return Reg64.RDX; // Remainder is in rdx
        }

        Compile(left);
        var callPreservedRegister = GetCalleePreservedRegister();
        _asm.Mov((RM64)callPreservedRegister, Reg64.RAX);
        Compile(right);
        



    }



    // TODO handle constant folding in type resolver instead
    //private Immediate HandleBinary(LiteralExpression left, LiteralExpression right, OperatorType operatorType)
    //{
    //    if (left.Value is int leftInt && right.Value is int rightInt)
    //    {
    //        switch (operatorType)
    //        {
    //            case OperatorType.Addition:
    //                return new Imm32(leftInt + rightInt);
    //            case OperatorType.Subtraction:
    //                return new Imm32(leftInt - rightInt);
    //            case OperatorType.Multiplication:
    //                return new Imm32(leftInt * rightInt);
    //            case OperatorType.Division:
    //                return new Imm32(leftInt / rightInt);
    //            default:
    //                throw new InvalidOperationException($"unsupported operator type '{operatorType}' for integer literals");
    //        }
    //    }
    //    else if (left.Value is float leftFloat && right.Value is float rightFloat)
    //    {
    //        switch (operatorType)
    //        {
    //            case OperatorType.Addition:
    //                return new Imm32(leftFloat + rightFloat);
    //            case OperatorType.Subtraction:
    //                return new Imm32(leftFloat - rightFloat);
    //            case OperatorType.Multiplication:
    //                return new Imm32(leftFloat * rightFloat);
    //            case OperatorType.Division:
    //                return new Imm32(leftFloat / rightFloat);
    //            default:
    //                throw new InvalidOperationException($"unsupported operator type '{operatorType}' for float literals");
    //        }
    //    }
    //    else
    //        throw new InvalidOperationException("only integer literals are supported for constant folding");
    //}

    private Reg64 GetFreeRegister()
    {

    }

    private Reg64 GetCalleePreservedRegister()
    {

    }

    private List<Register> calleeSavedRegisters = new()
    {
        Reg64.RBX,
        Reg64.RBP,
        Reg64.R12,
        Reg64.R13,
        Reg64.R14,
        Reg64.R15
    };

    private List<Register> callerSavedRegisters = new()
    {
        Reg64.RAX,
        Reg64.RCX,
        Reg64.RDX,
        Reg64.RSI,
        Reg64.RDI,
        Reg64.R8,
        Reg64.R9,
        Reg64.R10,
        Reg64.R11
    };

    private Register HandleAddition(RM dest, RM src)
    {
        if (dest is Reg64 destReg)
        {
            _asm.Add(destReg, ToRM64OrThrow(src));
            return destReg;
        }
        else if (src is Reg64 srcReg)
        {
            _asm.Add(srcReg, ToRM64OrThrow(dest));
            return srcReg;
        }
        else if (dest is Xmm128 destXmm)
        {
            _asm.Addsd(destXmm, ToRM64OrThrow(src));
            return destXmm;
        }
        else if (src is Xmm128 srcXmm)
        {
            _asm.Addsd(srcXmm, ToRM64OrThrow(dest));
            return srcXmm;
        }
        else
            throw new InvalidOperationException("at least one operand must be a register for addition");
    }

    private RM64 ToRM64OrThrow(RM rm)
    {
        if (rm is RM64 rm64) return rm64;
        throw new InvalidOperationException($"expected RM64 but got '{rm.GetType()}'");
    }

    #endregion

    private int GetAdditonalStackSlotsNeeded(FunctionContext functionContext)
    {
        foreach (var statement in functionContext.BodyStatements)
        {
            GatherMetadata(statement);
        }
        return functionContext.BodyStatements.Max(s => s.Metadata.StackSlotsNeeded);
    }

    private void GatherMetadata(StatementBase statement)
    {
        switch (statement)
        {
            case WhileStatement whileStatement:
                GatherMetadata(whileStatement);
                break;
            case AssignmentStatement assignmentStatement:
                GatherMetadata(assignmentStatement);
                break;
            case ErrorStatement errorStatement:
                GatherMetadata(errorStatement);
                break;
            case ReturnStatement returnStatement:
                GatherMetadata(returnStatement);
                break;
            case ConditionalStatement ifStatement:
                GatherMetadata(ifStatement);
                break;
            default:
                throw new InvalidOperationException($"Unknown statement type '{statement.GetType()}'");
        }
    }

    private void GatherMetadata(WhileStatement whileStatement)
    {
        GatherMetadata(whileStatement.ConditionalTarget);
        whileStatement.Metadata.StackSlotsNeeded = whileStatement.ConditionalTarget.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(AssignmentStatement assignmentStatement)
    {
        GatherMetadata(assignmentStatement.Value);
        assignmentStatement.Metadata.StackSlotsNeeded = assignmentStatement.Value.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(ErrorStatement errorStatement)
    {
        GatherMetadata(errorStatement.ErrorToReturn);
        errorStatement.Metadata.StackSlotsNeeded = errorStatement.ErrorToReturn.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(ReturnStatement returnStatement)
    {
        if (returnStatement.ValueToReturn == null)
        {
            returnStatement.Metadata.StackSlotsNeeded = 0;
            return;
        }
        GatherMetadata(returnStatement.ValueToReturn);
        returnStatement.Metadata.StackSlotsNeeded = returnStatement.ValueToReturn.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(ConditionalStatement ifStatement)
    {
        GatherMetadata(ifStatement.ConditionalTarget);
        int maxStackSlotsNeeded = ifStatement.ConditionalTarget.Metadata.StackSlotsNeeded;
        foreach (var statment in ifStatement.ThenBlock)
        {
            GatherMetadata(statment);
            maxStackSlotsNeeded = Math.Max(maxStackSlotsNeeded, statment.Metadata.StackSlotsNeeded);
        }
        if (ifStatement.ElseBlock != null)
        {
            foreach (var statment in ifStatement.ElseBlock)
            {
                GatherMetadata(statment);
                maxStackSlotsNeeded = Math.Max(maxStackSlotsNeeded, statment.Metadata.StackSlotsNeeded);
            }
        }
        ifStatement.Metadata.StackSlotsNeeded = maxStackSlotsNeeded;
    }

    private void GatherMetadata(ExpressionBase expression)
    {
        switch (expression)
        {
            case BinaryExpression binaryExpression:
                GatherMetadata(binaryExpression);
                break;
            case CallExpression callExpression:
                GatherMetadata(callExpression);
                break;
            case IdentifierExpression identifierExpression:
                GatherMetadata(identifierExpression);
                break;
            case LiteralExpression literalExpression:
                GatherMetadata(literalExpression);
                break;
            case MemberAccessExpression memberAccessExpression:
                GatherMetadata(memberAccessExpression);
                break;
            case ReferenceExpression referenceExpression:
                GatherMetadata(referenceExpression);
                break;
            case StackAllocateExpression stackAllocateExpression:
                GatherMetadata(stackAllocateExpression);
                break;
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }

    private void GatherMetadata(BinaryExpression binaryExpression)
    {
        
        GatherMetadata(binaryExpression.Left);
        GatherMetadata(binaryExpression.Right);
        binaryExpression.Metadata.StackSlotsNeeded = Math.Max(binaryExpression.Left.Metadata.StackSlotsNeeded, binaryExpression.Right.Metadata.StackSlotsNeeded);
        binaryExpression.Metadata.ContainsCall = binaryExpression.Left.Metadata.ContainsCall || binaryExpression.Right.Metadata.ContainsCall;

        if (binaryExpression.Left.Metadata.ContainsCall && binaryExpression.Right.Metadata.ContainsCall)
        {
            binaryExpression.Metadata.StackSlotsNeeded += 1;
        }
    }

    private void GatherMetadata(CallExpression callExpression)
    {
        foreach (var argument in callExpression.Arguments)
        {
            GatherMetadata(argument);
        }
        callExpression.Metadata.SU = 4;
        callExpression.Metadata.ContainsCall = true;
        callExpression.Metadata.StackSlotsNeeded = callExpression.Arguments.Max(a => a.Metadata.StackSlotsNeeded);
    }

    private void GatherMetadata(IdentifierExpression identifierExpression)
    {
        identifierExpression.Metadata.SU = 0;
        identifierExpression.Metadata.ContainsCall = false;
        identifierExpression.Metadata.StackSlotsNeeded = 0;
    }

    private void GatherMetadata(LiteralExpression literalExpression)
    {
        literalExpression.Metadata.SU = 1;
        literalExpression.Metadata.ContainsCall = false;
        literalExpression.Metadata.StackSlotsNeeded = 0;
    }

    private void GatherMetadata(MemberAccessExpression memberAccessExpression)
    {
        GatherMetadata(memberAccessExpression.Instance);

        memberAccessExpression.Metadata.SU = memberAccessExpression.Instance.Metadata.SU;
        memberAccessExpression.Metadata.ContainsCall = memberAccessExpression.Instance.Metadata.ContainsCall;
        memberAccessExpression.Metadata.StackSlotsNeeded = memberAccessExpression.Instance.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(ReferenceExpression referenceExpression)
    {
        if (referenceExpression.Instance != null)
        {
            GatherMetadata(referenceExpression.Instance);
            referenceExpression.Metadata.SU = referenceExpression.Instance.Metadata.SU;
            referenceExpression.Metadata.ContainsCall = referenceExpression.Instance.Metadata.ContainsCall;
            referenceExpression.Metadata.StackSlotsNeeded = referenceExpression.Instance.Metadata.StackSlotsNeeded;
        }
        else
        {
            referenceExpression.Metadata.SU = 1;
            referenceExpression.Metadata.ContainsCall = false;
            referenceExpression.Metadata.StackSlotsNeeded = 0;
        }
    }

    private void GatherMetadata(StackAllocateExpression stackAllocateExpression)
    {
        // Pass 
        // Stack allocation should not occur naturally in expression trees
    }
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



