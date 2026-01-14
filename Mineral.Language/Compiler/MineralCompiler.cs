
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Optimizer;
using Mineral.Language.Parser;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Runtime.InteropServices;
using X86_64Assembler;

namespace Mineral.Language.Compiler;

public class MineralCompiler
{

    private X86AssemblyContext _asm = new();
    private FunctionContext _ctx;
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

            var optimizer = new X86Optimizer();
            optimizer.Optimize(_asm);

            var error = _asm.OutputX86Assembly64Bit(outputTarget, entryPoint, Path.GetFileName(outputPath), out var peFile);
            var bytes = peFile.AssembleProgram(entryPoint);
            File.WriteAllBytes(outputPath, bytes);
            var text = peFile.EmitDecodedAssembly(entryPoint);
            File.WriteAllText(Path.ChangeExtension(outputPath, ".asm"), text);
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

    private void FreeLastUsedStackSlot()
    {
        // Frees the most recently used stack slot
        _stackSlotIndex--;
        if (_stackSlotIndex < 0) throw new InvalidOperationException("stack slot index went below zero");
    }

    private void CompileFunction(FunctionContext functionContext)
    {
        _stackSlotIndex = 0;
        if (functionContext.IsImported) return;
        _ctx = functionContext;
        var parameters = functionContext.Parameters.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();
        var localVariables = functionContext.LocalVariables.Select(kv => new X86FunctionLocalData(kv.Key.Lexeme, kv.Value.GetStackSize())).ToList();

        var additonalStackSlotsNeeded = GetAdditonalStackSlotsNeeded(functionContext);
        for (int i = 0; i < additonalStackSlotsNeeded; i++)
        {
            localVariables.Add(new X86FunctionLocalData($"%__iterm{i}", 8));
        }

        _asm.EnterFunction(CallingConvention.StdCall, functionContext.GetDecoratedFunctionLabel(), parameters, localVariables, false, ""); // TODO: Exports

        foreach(var statement in functionContext.BodyStatements)
        {
            // Compile each statement except for stack allocate statments
            if (statement is AssignmentStatement assignmentStatement && assignmentStatement.Value is StackAllocateExpression stackAllocateExpression) // TODO move this check to sanitizer
                continue;
            Compile(statement);
        }
        if (functionContext.BodyStatements.LastOrDefault() is not ReturnStatement && functionContext.BodyStatements.LastOrDefault() is not ErrorStatement)
        {
            _asm.Xor(Reg64.RDX, (RM64)Reg64.RDX); // zero out error
            _asm.TearDownStackFrame();
            _asm.Ret();
        }
        _asm.ExitFunction();
    }

    #region Statements

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
            case WhileStatement whileStatement:
                Compile(whileStatement);
                break;
            case AssignmentStatement assignmentStatement:
                Compile(assignmentStatement);
                break;
            case DereferenceAssignmentStatement dereferenceAssignmentStatement:
                Compile(dereferenceAssignmentStatement);
                break;
            case BreakStatement breakStatement:
                Compile(breakStatement);
                break;
            case ContinueStatement continueStatement:
                Compile(continueStatement);
                break;
            default:
                throw new InvalidOperationException($"Unknown statement type '{statement.GetType()}'");
        }
    }

    private void Compile(ReturnStatement returnStatement)
    {
        var rm = CompileToRMOrImmediate(returnStatement.ValueToReturn, Reg64.RAX, Xmm128.XMM0);
        if (returnStatement.ValueToReturn.IsFloat32())
        {
            HandleRMOrImmediate(rm,
                (mem) => _asm.Movss(Xmm128.XMM0, mem),
                (xmm) => _asm.MovIfNeeded(Xmm128.XMM0, xmm));
        }
        else if (returnStatement.ValueToReturn.IsFloat64())
        {
            HandleRMOrImmediate(rm,
                (mem) => _asm.Movsd(Xmm128.XMM0, mem),
                (xmm) => _asm.MovIfNeeded(Xmm128.XMM0, xmm));
        }
        else if (returnStatement.ValueToReturn.IsIntegerType() || returnStatement.ValueToReturn.IsReferenceType() || returnStatement.ValueToReturn.IsStringType())
        {
            HandleRMOrImmediate(rm,
                (mem) => _asm.Mov(Reg64.RAX, mem),
                (Reg64 reg) => _asm.MovIfNeeded(Reg64.RAX, reg),
                (imm) => _asm.Mov(Reg64.RAX, imm));
        }
        else if (returnStatement.ValueToReturn.IsByteType())
        {
            HandleRMOrImmediate(rm,
                (mem) => _asm.Mov(Reg8.AL, mem),
                (Reg8 reg) => _asm.MovIfNeeded(Reg8.AL, reg),
                (Imm8 imm) => _asm.Mov(Reg8.AL, imm.Value));
        }
        else throw new InvalidOperationException($"unexpected return type during code generation '{returnStatement.ValueToReturn.ConcreteType}'");

        if (_ctx.IsErrorable) _asm.Xor(Reg64.RDX, (RM64)Reg64.RDX); // Zero out error before returning value
        _asm.TearDownStackFrame();
        _asm.Ret();
    }

    private void Compile(ErrorStatement errorStatement)
    {
        var rm = CompileToRMOrImmediate(errorStatement.ErrorToReturn, Reg64.RDX, Xmm128.XMM0);
        HandleRMOrImmediate(rm,
            (mem) => _asm.Mov(Reg64.RDX, mem),
            (Reg64 reg) => _asm.MovIfNeeded(Reg64.RDX, reg),
            (imm) => _asm.Mov(Reg64.RDX, imm)); // if we ever make integer types errorable

        _asm.TearDownStackFrame();
        _asm.Ret();
    }

    private void Compile(ConditionalStatement conditionalStatement)
    {
        var thenBlock = () =>
        {
            foreach (var stmt in conditionalStatement.ThenBlock)
                Compile(stmt);
        };

        var elseBlock = () =>
        {
            foreach (var stmt in conditionalStatement.ElseBlock)
                Compile(stmt);
        };

        if (conditionalStatement.ConditionalTarget is BinaryExpression binaryExpression)
        {
            CompileAsConditionalBlock(binaryExpression, thenBlock, elseBlock);
            return;
        }

        var rmCondition = Compile(conditionalStatement.ConditionalTarget, Reg64.RAX, Xmm128.XMM0);

        // Ensure value in RAX
        // Conditional will always return in Reg64 since it is integer or reference type truthiness
        var regToTest = Reg64.RAX;
        HandleRM(rmCondition,
            (mem) => _asm.Mov(regToTest, mem),
            (reg) => _asm.MovIfNeeded(regToTest, reg));

        var branchIdentifier = _asm.CreateUniqueLabel("");
        var falseLabel = $"CTST_{branchIdentifier}";
        var endLabel = $"CTST_END_{branchIdentifier}";
        _asm.Test(regToTest, regToTest);
        _asm.Jz(Rel32.Create(falseLabel));
        thenBlock();
        _asm.Jmp(Rel32.Create(endLabel));
        _asm.Label(falseLabel);
        elseBlock();
        _asm.Label(endLabel);
    }


    private void Compile(WhileStatement whileStatement)
    {


        var loopIdentifier = _asm.CreateUniqueLabel("");
        var loopTop = $"L{loopIdentifier}";
        var endLabel = $"LCTST_END{loopIdentifier}";
        whileStatement.Metadata.ContinueLabel = loopTop;
        whileStatement.Metadata.BreakLabel = endLabel;

        var thenBlock = () =>
        {
            foreach (var stmt in whileStatement.ThenBlock)
                Compile(stmt);
        };

        _asm.Label(loopTop);
        var rmCondition = Compile(whileStatement.ConditionalTarget, Reg64.RAX, Xmm128.XMM0);

        // Ensure value in RAX
        // Conditional will always return in Reg64 since it is integer or reference type truthiness
        var regToTest = Reg64.RAX;
        HandleRM(rmCondition,
            (mem) => _asm.Mov(regToTest, mem),
            (reg) => _asm.MovIfNeeded(regToTest, reg));

        _asm.Test(regToTest, regToTest);
        _asm.Jz(Rel32.Create(endLabel));
        thenBlock();
        _asm.Jmp(Rel32.Create(loopTop));
        _asm.Label(endLabel);
    }

    private void Compile(AssignmentStatement assignmentStatement)
    {
        var rmValue = CompileToRMOrImmediate(assignmentStatement.Value, Reg64.RAX, Xmm128.XMM0);
        var assignmentTarget = CompileAndReturnAssignmentTargetFromLValue(assignmentStatement.AssignmentTarget, Reg64.RCX);
        if (assignmentTarget == null) { }
        else if (assignmentStatement.AssignmentTarget.IsFloat32())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg64.RAX, mem);
                    _asm.Mov(assignmentTarget, Reg64.RAX);
                },
                (reg) => _asm.Movss(assignmentTarget, reg));
        }
        else if (assignmentStatement.AssignmentTarget.IsFloat64())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg64.RAX, mem);
                    _asm.Mov(assignmentTarget, Reg64.RAX);
                },
                (reg) => _asm.Movsd(assignmentTarget, reg));
        }
        else if (assignmentStatement.AssignmentTarget.IsIntegerType() || assignmentStatement.AssignmentTarget.IsReferenceType() || assignmentStatement.AssignmentTarget.IsStringType())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg64.RAX, mem);
                    _asm.Mov(assignmentTarget, Reg64.RAX);
                },
                (reg) => 
                    _asm.Mov(assignmentTarget, reg),
                (imm) => _asm.Mov(assignmentTarget, imm));
        }
        else if (assignmentStatement.AssignmentTarget.IsByteType())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg8.AL, mem);
                    _asm.Mov(assignmentTarget, Reg8.AL);
                },
                (Reg8 reg) =>
                    _asm.Mov(assignmentTarget, reg),
                (Imm8 imm) => _asm.Mov(assignmentTarget, imm.Value));
        }
        else throw new InvalidOperationException($"unexpected assignment of type '{assignmentStatement.Value.ConcreteType}' to '{assignmentStatement.AssignmentTarget.ConcreteType}'");



        // Errors returned in RDX
        var errorTarget = assignmentStatement.ErrorTarget;
        if (errorTarget != null)
        {
            var errorTargetMemoryLocation = CompileAndReturnAssignmentTargetFromLValue(errorTarget, Reg64.RDX);

            if (errorTargetMemoryLocation != null)
            {
                _asm.Mov(errorTargetMemoryLocation, Reg64.RDX);
            } // else pass for discard
        }
    }

    private void Compile(DereferenceAssignmentStatement dereferenceAssignmentStatement)
    {
        var rmValue = CompileToRMOrImmediate(dereferenceAssignmentStatement.Value, Reg64.RAX, Xmm128.XMM0);
        var assignmentTarget = CompileAndReturnAssignmentTargetFromLValue(dereferenceAssignmentStatement.AssignmentTarget, Reg64.RCX);
        var desiredXmm = Xmm128.XMM1;
        if (assignmentTarget != null)
        {
            _asm.Mov(Reg64.RCX, assignmentTarget);
            assignmentTarget = Mem64.Create(Reg64.RCX);
        }
        if (assignmentTarget == null) { }
        else if (dereferenceAssignmentStatement.AssignmentTarget.IsFloat32())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Movss(desiredXmm, mem);
                    _asm.Movss(assignmentTarget, desiredXmm);
                },
                (reg) => _asm.Movss(assignmentTarget, reg));
        }
        else if (dereferenceAssignmentStatement.AssignmentTarget.IsFloat64())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg64.RAX, mem);
                    _asm.Mov(assignmentTarget, Reg64.RAX);
                },
                (reg) => _asm.Movsd(assignmentTarget, reg));
        }
        else if (dereferenceAssignmentStatement.AssignmentTarget.IsIntegerType() || dereferenceAssignmentStatement.AssignmentTarget.IsReferenceType() || dereferenceAssignmentStatement.AssignmentTarget.IsStringType())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg64.RAX, mem);
                    _asm.Mov(assignmentTarget, Reg64.RAX);
                },
                (reg) =>
                    _asm.Mov(assignmentTarget, reg),
                (imm) => _asm.Mov(assignmentTarget, imm));
        }
        else if (dereferenceAssignmentStatement.AssignmentTarget.IsByteType())
        {
            HandleRMOrImmediate(rmValue,
                (mem) =>
                {
                    _asm.Mov(Reg8.AL, mem);
                    _asm.Mov(assignmentTarget, Reg8.AL);
                },
                (Reg8 reg) => _asm.Mov(assignmentTarget, reg),
                (Imm8 imm) => _asm.Mov(assignmentTarget, imm.Value));
        }
        else throw new InvalidOperationException($"unexpected assignment of type '{dereferenceAssignmentStatement.Value.ConcreteType}' to '{dereferenceAssignmentStatement.AssignmentTarget.ConcreteType}'");



        // Errors returned in RDX
        var errorTarget = dereferenceAssignmentStatement.ErrorTarget;
        if (errorTarget != null)
        {
            var errorTargetMemoryLocation = CompileAndReturnAssignmentTargetFromLValue(errorTarget, Reg64.RDX);

            if (errorTargetMemoryLocation != null)
            {
                _asm.Mov(errorTargetMemoryLocation, Reg64.RDX);
            } // else pass for discard
        }
    }


    private Mem64? CompileAndReturnAssignmentTargetFromLValue(LValue lValue, Reg64 desiredReg)
    {
        if (lValue is IdentifierLValue identifierLValue)
        {
            return _asm.GetIdentifierOffset(identifierLValue.VariableName.Lexeme, out _);
        }
        else if (lValue is InstanceMemberLValue instanceMemberLValue)
        {
            var instanceMemoryLocation = CompileAndReturnAssignmentTargetFromLValue(instanceMemberLValue.Instance, desiredReg);
            if (instanceMemoryLocation == null) throw new InvalidOperationException("Invalid discard as left hand side of member access");
            if (instanceMemberLValue.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(instanceMemberLValue.Member, out var isReferenceType, out var offset))
            {
                if (isReferenceType)
                {
                    _asm.Mov(desiredReg, instanceMemoryLocation);
                    return Mem64.Create(desiredReg, offset);
                }

                // If it is a struct type it is guaranteed to be on the stack
                if (instanceMemoryLocation.Register != Reg64.RBP || instanceMemoryLocation.Displacement is not Disp32 disp32) 
                    throw new InvalidOperationException("structs should only be stack allocated");
                return Mem64.Create(Reg64.RBP, disp32.Offset - offset);
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

    private void Compile(BreakStatement breakStatement)
    {
        var breakLabel = breakStatement.ParentLoop?.Metadata.BreakLabel;
        if (string.IsNullOrEmpty(breakLabel))
            throw new InvalidOperationException($"cannot generate code for break statement occurring outside of loop");
        _asm.Jmp(Rel32.Create(breakLabel));
    }

    private void Compile(ContinueStatement continueStatement)
    {
        var continueLabel = continueStatement.ParentLoop?.Metadata.ContinueLabel;
        if (string.IsNullOrEmpty(continueLabel))
            throw new InvalidOperationException($"cannot generate code for continue statement occurring outside of loop");
        _asm.Jmp(Rel32.Create(continueLabel));
    }

    #endregion


    #region Happy path expression compilation

    private RMOrImmediate CompileToRMOrImmediate(ExpressionBase expression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        switch (expression)
        {
            case LiteralExpression literalExpression:
                return Compile(literalExpression, desiredReg, desiredXmm);
            
            default:
                return new RMOrImmediate(Compile(expression, desiredReg, desiredXmm));
        }
    }

    private RMOrImmediate Compile(LiteralExpression literalExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        if (literalExpression.Value is int i)
            return new RMOrImmediate(Imm32.Create(i));
        else if (literalExpression.Value is null)
            return new RMOrImmediate(Imm32.Create(0));
        else if (literalExpression.Value is float flt)
            return new RMOrImmediate(_asm.AddFloatingPoint(flt));
        else if (literalExpression.Value is string str)
        {
            var mem = _asm.AddString(str);
            var boxedMem = _asm.AddOtherData(new BoxedString(str.Length, mem.Symbol));
            _asm.Lea(desiredReg, boxedMem);
            //_asm.Mov(desiredReg, Imm64.Create(mem.Symbol)); // absolute addressing
            return new RMOrImmediate(desiredReg);
        }
        else if (literalExpression.Value is WString wstr)
        {
            var mem = _asm.AddUTF16String(wstr.Value);
            var boxedMem = _asm.AddOtherData(new BoxedString(wstr.Value.Length, mem.Symbol));
            _asm.Lea(desiredReg, boxedMem);
            //_asm.Mov(desiredReg, Imm64.Create(mem.Symbol)); // absolute addressing
            return new RMOrImmediate(desiredReg);
        }
        else if (literalExpression.Value is byte b)
            return new RMOrImmediate(new Imm8(b));
        else throw new InvalidOperationException($"Unknown literal type for value '{literalExpression.Value}'");
    }

    private RM Compile(ExpressionBase expression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                return Compile(callExpression, desiredReg, desiredXmm);
            case BinaryExpression binaryExpression:
                return Compile(binaryExpression);
            case IdentifierExpression identifierExpression:
                return Compile(identifierExpression);
            case MemberAccessExpression memberAccessExpression:
                return Compile(memberAccessExpression, desiredReg, desiredXmm);
            case ReferenceExpression referenceExpression:
                return Compile(referenceExpression, desiredReg, desiredXmm);
            case DereferenceExpression dereferenceExpression:
                return Compile(dereferenceExpression, desiredReg, desiredXmm);
            case CastExpression castExpression:
                return Compile(castExpression, desiredReg, desiredXmm);
            default:
                throw new InvalidOperationException($"Unknown expression type '{expression.GetType()}'");
        }
    }
    private Mem Compile(IdentifierExpression identifierExpression)
    {
        if (identifierExpression.FunctionContext != null)
            return Mem64.Create(identifierExpression.FunctionContext.GetDecoratedFunctionLabel());
        return _asm.GetIdentifierOffset(identifierExpression.Symbol.Lexeme, out _);
    }


    private Mem Compile(MemberAccessExpression memberAccessExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        if (memberAccessExpression.FunctionContext != null) 
            return Mem64.Create(memberAccessExpression.FunctionContext.GetDecoratedFunctionLabel());

        var instanceLocation = Compile(memberAccessExpression.Instance, desiredReg, desiredXmm);
        if (!memberAccessExpression.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(memberAccessExpression.MemberToAccess, out var isReferenceType, out var offset))
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{memberAccessExpression.Instance.ConcreteType}' or type '{memberAccessExpression.Instance.ConcreteType}' does not contain member '{memberAccessExpression.MemberToAccess.Lexeme}'");
        if (!isReferenceType)
        {
            return HandleRM(instanceLocation,
                mem =>
                {
                    return Mem64.Create((Reg64)mem.Register, mem.Displacement.Offset - offset);
                },
                reg =>
                {
                    throw new Exception("cannot return struct in register");
                });
        }
        return HandleRM(instanceLocation,
                mem =>
                {
                    _asm.Mov(desiredReg, mem); 
                    return Mem64.Create(desiredReg, offset);
                },
                reg =>
                {
                    _asm.MovIfNeeded(desiredReg, reg); // TODO verify needed?
                    return Mem64.Create(desiredReg, offset);
                });
    }

    private Reg64 Compile(ReferenceExpression referenceExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        if (referenceExpression.Instance == null)
        {
            var location = _asm.GetIdentifierOffset(referenceExpression.MemberToAccess.Lexeme, out _);
            _asm.Lea(Reg64.RAX, location);
            return Reg64.RAX;
        }
        var rm = Compile(referenceExpression.Instance, desiredReg, desiredXmm);
        if (!referenceExpression.Instance.ConcreteType.TryGetMemberOffsetFromStructOrReference(referenceExpression.MemberToAccess, out var isReferenceType, out var offset))
            throw new InvalidOperationException($"expect left hand side of member access to be struct type but got '{referenceExpression.Instance.ConcreteType}' or type '{referenceExpression.Instance.ConcreteType}' does not contain member '{referenceExpression.MemberToAccess.Lexeme}'");
        if (!isReferenceType)
        {
            return HandleRM(rm,
                mem =>
                {
                    _asm.Lea(desiredReg, Mem64.Create((Reg64)mem.Register, mem.Displacement.Offset - offset));
                    return desiredReg;
                },
                reg =>
                {
                    throw new Exception("cannot return struct in register");
                });
        }
        return HandleRM(rm,
                mem =>
                {
                    _asm.Mov(desiredReg, mem); 
                    _asm.Lea(desiredReg, Mem64.Create(desiredReg, offset));
                    return desiredReg;
                },
                reg =>
                {
                    _asm.Lea(desiredReg, Mem64.Create(reg, offset));
                    return desiredReg;
                });
    }

    private Mem64 Compile(DereferenceExpression dereferenceExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        var rmTarget = Compile(dereferenceExpression.Target, desiredReg, desiredXmm);
        if (rmTarget is Mem mem)
        {
            _asm.Mov(desiredReg, mem);
            return Mem64.Create(desiredReg);
        }
        else if (rmTarget is Reg64 reg)
        {
            return Mem64.Create(desiredReg);
        }
        else throw new InvalidOperationException($"invalid rm type during dereference '{rmTarget.GetType()}'");
    }

    private RM Compile(CastExpression castExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        var rmValue = Compile(castExpression.Value, desiredReg, desiredXmm);
        if (castExpression.IsFloat32() && castExpression.Value.IsFloat64())
        {
            HandleRM(rmValue,
                (mem) => _asm.Cvtsd2ss(desiredXmm, mem),
                (Xmm128 xmm) => _asm.Cvtsd2ss(desiredXmm, xmm));
            return desiredXmm;
        }
        if (castExpression.IsFloat32() && castExpression.Value.IsIntegerType())
        {
            HandleRM(rmValue,
                (Mem mem) => _asm.Cvtsi2ss(desiredXmm, (RM64)mem),
                (Reg64 reg) => _asm.Cvtsi2ss(desiredXmm, reg));
            return desiredXmm;
        }

        if (castExpression.IsFloat64() && castExpression.Value.IsFloat32())
        {
            HandleRM(rmValue,
                (Mem mem) => _asm.Cvtss2sd(desiredXmm, mem),
                (Xmm128 xmm) => _asm.Cvtss2sd(desiredXmm, xmm));
            return desiredXmm;
        }
        if (castExpression.IsFloat64() && castExpression.Value.IsIntegerType())
        {
            HandleRM(rmValue,
                (Mem mem) => _asm.Cvtsi2sd(desiredXmm, (RM64)mem),
                (Reg64 reg) => _asm.Cvtsi2sd(desiredXmm, reg));
            return desiredXmm;
        }

        if (castExpression.IsIntegerType() && castExpression.Value.IsFloat32())
        {
            HandleRM(rmValue,
                (mem) => _asm.Cvtss2si(desiredReg, mem),
                (xmm) => _asm.Cvtss2si(desiredReg, xmm));
            return desiredReg;
        }
        if (castExpression.IsIntegerType() && castExpression.Value.IsFloat64())
        {
            HandleRM(rmValue,
                (Mem mem) => _asm.Cvtsd2si(desiredReg, mem),
                (Reg64 reg) => _asm.Cvtsd2si(desiredReg, reg));
            return desiredReg;
        }

        // otherwise no action is required, return value directly
        return rmValue;
    }

    #region Binary

    private RM Compile(BinaryExpression binaryExpression)
    {
        // Here it is assumed binary expressions can only have operands with equal types
        if (!binaryExpression.Left.ConcreteType.IsEqualTo(binaryExpression.Right.ConcreteType))
            throw new InvalidOperationException($"binary {binaryExpression.Operator} is not supported between types '{binaryExpression.Left.ConcreteType}' and '{binaryExpression.Right.ConcreteType}'");
        if (binaryExpression.Left.IsIntegerType() || binaryExpression.Left.IsReferenceType() || binaryExpression.Left.IsStringType()) return CompileIntegerResult(binaryExpression);
        else if (binaryExpression.Left.IsFloat32()) return CompileFloat32Result(binaryExpression);
        else if (binaryExpression.Left.IsFloat64()) return CompileFloat64Result(binaryExpression);
        else if (binaryExpression.Left.IsByteType()) return CompileByteResult(binaryExpression);
        else throw new InvalidOperationException($"unexpected lhs type '{binaryExpression.Left.ConcreteType}'");
    }

    private void CompileAsConditionalBlock(BinaryExpression binaryExpression, Action ifBlock, Action elseBlock)
    {
        // Here it is assumed binary expressions can only have operands with equal types
        if (!binaryExpression.Left.ConcreteType.IsEqualTo(binaryExpression.Right.ConcreteType))
            throw new InvalidOperationException($"binary {binaryExpression.Operator} is not supported between types '{binaryExpression.Left.ConcreteType}' and '{binaryExpression.Right.ConcreteType}'");
        if (binaryExpression.Left.IsIntegerType() || binaryExpression.Left.IsReferenceType() || binaryExpression.Left.IsStringType()) CompileIntegerResultAsConditionalBlock(binaryExpression, ifBlock, elseBlock);
        else if (binaryExpression.Left.IsFloat32()) CompileFloat32ResultAsConditionalBlock(binaryExpression, ifBlock, elseBlock);
        else if (binaryExpression.Left.IsFloat64()) CompileFloat64ResultAsConditionalBlock(binaryExpression, ifBlock, elseBlock);
        else if (binaryExpression.Left.IsByteType()) CompileByteResultAsConditionalBlock(binaryExpression, ifBlock, elseBlock);
        else throw new InvalidOperationException($"unexpected lhs type '{binaryExpression.Left.ConcreteType}'");
    }


    #region Binary Helpers

    #region Integer

    private Reg64 CompileIntegerResult(BinaryExpression binaryExpression)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, true);

            return HandleOperator(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true);

        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, false);

            return HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, true);

            return HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true);

        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, false);

            return HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        }

        throw new InvalidOperationException("unexpected binary case");
    }

    private void CompileIntegerResultAsConditionalBlock(BinaryExpression binaryExpression, Action thenBlock, Action elseBlock)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, true, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true, thenBlock, elseBlock);
            return;
        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, false, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, true, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, false, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;

        }
        throw new InvalidOperationException("unexpected binary case");
    }


    private Reg64 HandleDivision(BinaryExpression binaryExpression, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        var leftReg = Reg64.RAX;
        var rightReg = Reg64.RCX;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, Xmm128.XMM1);
        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                        return;
                    // If register does not persist accross calls
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);

                },
                (reg) => { _asm.Mov(interm, reg); rmRight = new RMOrImmediate(interm); },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }
        var rmLeft = CompileToRMOrImmediate(binaryExpression.Right, leftReg, Xmm128.XMM0);

        // Ensure lhs is in RAX
        HandleRMOrImmediate(rmLeft,
            (mem) => _asm.Mov(leftReg, mem),
            (reg) => _asm.MovIfNeeded(leftReg, reg),
            (Imm32 imm) => _asm.Mov(leftReg, imm));

        OperatorFactory.Handle(
            leftReg, rmRight,
            (reg, mem) =>
            {
                _asm.Cqo();
                _asm.Idiv((RM64)mem);
            },
            (regDest, regSrc) =>
            {
                _asm.Cqo();
                _asm.Idiv(regSrc);
            },
            (Reg64 reg, Imm32 imm) =>
            {
                _asm.Cqo();
                _asm.Mov(rightReg, imm);
                _asm.Idiv(rightReg);
            });

        if (spillRightResult) FreeLastUsedStackSlot();
        return op == OperatorType.Division ? Reg64.RAX : Reg64.RDX;
    }

    private void HandleDivision(BinaryExpression binaryExpression, bool spillRightResult, Action thenBlock, Action elseBlock)
    {
        var regToTest = HandleDivision(binaryExpression, spillRightResult);
        var falseLabel = _asm.CreateUniqueLabel("CTST_");
        var endLabel = _asm.CreateUniqueLabel("CTST_END_");
        _asm.Test(regToTest, regToTest);
        _asm.Jz(Rel32.Create(falseLabel));
        thenBlock();
        _asm.Jmp(Rel32.Create(endLabel));
        _asm.Label(falseLabel);
        elseBlock();
        _asm.Label(endLabel);

    }

    private Reg64 HandleOperator(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        Reg64 rmReturn = leftReg;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                        return;
                    // If register does not persist accross calls
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);
                },
                (reg) => { _asm.Mov(interm, reg); rmRight = new RMOrImmediate(interm); },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Mov(leftReg, mem),
                (reg) => _asm.MovIfNeeded(leftReg, reg),
                (Imm32 imm) => _asm.Mov(leftReg, imm)); 

            OperatorFactory.Handle(
                leftReg, rmRight,
                (reg, mem) => _asm.Sub(reg, mem),
                (dest, src) => _asm.Sub(dest, (RM64)src),
                (reg, imm) => _asm.Sub(reg, imm.Value));


        }
        else if (op == OperatorType.Addition)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },

                (reg, mem) => _asm.Add(reg, mem),
                (dest, src) => _asm.Add(dest, (RM64)src),
                (reg, imm) => _asm.Add(reg, imm.Value));

        }
        else if (op == OperatorType.Multiplication)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },
                (reg, mem) => _asm.Imul(reg, mem),
                (dest, src) => _asm.Imul(dest, src),
                (reg, imm) => _asm.Imul(reg, reg, imm.Value));
        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.GE, "CL", "CL_END");
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.G, "CLE", "CLE_END");
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.LE, "CG", "CG_END");
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.L, "CGE", "CGE_END");
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.NE, "CE", "CE_END");
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.E, "CNE", "CNE_END");
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        return rmReturn;
    }

    private void HandleOperator(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult, Action thenBlock, Action elseBlock)
    {
        var op = binaryExpression.Operator;
        Reg64? rmReturn = null;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                        return;
                    // If register does not persist accross calls
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);
                },
                (reg) => { _asm.Mov(interm, reg); rmRight = new RMOrImmediate(interm); },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Mov(leftReg, mem),
                (reg) => _asm.MovIfNeeded(leftReg, reg),
                (Imm32 imm) => _asm.Mov(leftReg, imm));

            OperatorFactory.Handle(
                leftReg, rmRight,
                (reg, mem) => _asm.Sub(reg, mem),
                (dest, src) => _asm.Sub(dest, (RM64)src),
                (reg, imm) => _asm.Sub(reg, imm.Value));


        }
        else if (op == OperatorType.Addition)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },

                (reg, mem) => _asm.Add(reg, mem),
                (dest, src) => _asm.Add(dest, (RM64)src),
                (reg, imm) => _asm.Add(reg, imm.Value));

        }
        else if (op == OperatorType.Multiplication)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },
                (reg, mem) => _asm.Imul(reg, mem),
                (dest, src) => _asm.Imul(dest, src),
                (reg, imm) => _asm.Imul(reg, reg, imm.Value));
        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.GE, "CL", "CL_END", thenBlock, elseBlock);
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.G, "CLE", "CLE_END", thenBlock, elseBlock);
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.LE, "CG", "CG_END", thenBlock, elseBlock);
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.L, "CGE", "CGE_END", thenBlock, elseBlock);
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.NE, "CE", "CE_END", thenBlock, elseBlock);
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.E, "CNE", "CNE_END", thenBlock, elseBlock);
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        if (rmReturn != null)
        {
            // then we still need to test for the branch
            var falseLabel = _asm.CreateUniqueLabel("CTST_");
            var endLabel = _asm.CreateUniqueLabel("CTST_END_");
            _asm.Test(rmReturn, rmReturn);
            _asm.Jz(Rel32.Create(falseLabel));
            thenBlock();
            _asm.Jmp(Rel32.Create(endLabel));
            _asm.Label(falseLabel);
            elseBlock();
            _asm.Label(endLabel);
        }
    }

    private Reg64 HandleConditional(Reg64 leftReg, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle(rmLeft, rmRight,
                    (mem) =>
                    {
                        _asm.Mov(leftReg, mem);
                        return leftReg;
                    },
                    (reg) =>
                    {
                        _asm.MovIfNeeded(leftReg, reg);
                        return leftReg;
                    },
                    (imm) =>
                    {
                        _asm.Mov(leftReg, imm);
                        return leftReg;
                    },
                    (reg, mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(reg, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(reg, (RM64)reg);
                        _asm.Label(endLabel);
                    },
                    (regDest, regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(regDest, (RM64)regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(regDest, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(regDest, (RM64)regDest);
                        _asm.Label(endLabel);
                    },
                    (reg, imm) =>
                    {
                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, imm.Value);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(reg, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(reg, (RM64)reg);
                        _asm.Label(endLabel);
                    });
        return leftReg;
    }

    private Reg64? HandleConditionalBranch(Reg64 leftReg, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix, Action ifBlock, Action elseBlock)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle(rmLeft, rmRight,
                    (mem) =>
                    {
                        _asm.Mov(leftReg, mem);
                        return leftReg;
                    },
                    (reg) =>
                    {
                        _asm.MovIfNeeded(leftReg, reg);
                        return leftReg;
                    },
                    (imm) =>
                    {
                        _asm.Mov(leftReg, imm);
                        return leftReg;
                    },
                    (reg, mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    },
                    (regDest, regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(regDest, (RM64)regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    },
                    (reg, imm) =>
                    {
                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, imm.Value);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    });
        return null;
    }
    #endregion Integer

    #region Float32

    private RM CompileFloat32Result(BinaryExpression binaryExpression)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat32(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true);

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat32(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat32(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true);

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat32(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        throw new InvalidOperationException("unexpected binary case");
    }

    private void CompileFloat32ResultAsConditionalBlock(BinaryExpression binaryExpression, Action thenBlock, Action elseBlock)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat32(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true, thenBlock, elseBlock);
            return;
        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat32(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat32(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat32(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;

        }
        throw new InvalidOperationException("unexpected binary case");
    }

    private RM HandleOperatorFloat32(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        RM rmReturn = leftXmm;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                    {
                        return;
                    }
                    _asm.Mov(rightReg , mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);

                },
                (Xmm128 reg) => 
                { 
                    _asm.Movss(interm, reg); 
                    rmRight = new RMOrImmediate(interm);
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Division)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movss(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Divss(reg, mem),
                (dest, src) => _asm.Divss(dest, src));
        }
        else if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movss(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg,Mem mem) => _asm.Subss(reg, mem),
                (dest, src) => _asm.Subss(dest, src));
        }
        else if (op == OperatorType.Addition)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movss(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Addss(reg, mem),
                (dest, src) => _asm.Addss(dest, src));

        }
        else if (op == OperatorType.Multiplication)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movss(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Mulss(reg, mem),
                (dest, src) => _asm.Mulss(dest, src));
        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.AE, "CL", "CL_END");
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.A, "CLE", "CLE_END");
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.BE, "CG", "CG_END");
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.B, "CGE", "CGE_END");
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.E, "CE", "CE_END");
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.NE, "CNE", "CNE_END");
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        return rmReturn;
    }

    private void HandleOperatorFloat32(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult, Action ifBlock, Action elseBlock)
    {
        var op = binaryExpression.Operator;
        RM? rmReturn = leftXmm;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                    {
                        return;
                    }
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);

                },
                (Xmm128 reg) =>
                {
                    _asm.Movss(interm, reg);
                    rmRight = new RMOrImmediate(interm);
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Division)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movss(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Divss(reg, mem),
                (dest, src) => _asm.Divss(dest, src));
        }
        else if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movss(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Subss(reg, mem),
                (dest, src) => _asm.Subss(dest, src));
        }
        else if (op == OperatorType.Addition)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movss(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Addss(reg, mem),
                (dest, src) => _asm.Addss(dest, src));

        }
        else if (op == OperatorType.Multiplication)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movss(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Mulss(reg, mem),
                (dest, src) => _asm.Mulss(dest, src));
        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalBranchFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.AE, "CL", "CL_END", ifBlock, elseBlock);
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalBranchFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.A, "CLE", "CLE_END", ifBlock, elseBlock);
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalBranchFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.BE, "CG", "CG_END", ifBlock, elseBlock);
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalBranchFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.B, "CGE", "CGE_END", ifBlock, elseBlock);
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalBranchFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.E, "CE", "CE_END", ifBlock, elseBlock);
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalBranchFloat32(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.NE, "CNE", "CNE_END", ifBlock, elseBlock);
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        if (rmReturn != null)
        {
            throw new InvalidOperationException("unable to generate code for floating point truthiness");
        }
    }

    private Reg64 HandleConditionalFloat32(Reg64 leftReg, Xmm128 leftXmm, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle(rmLeft, rmRight,
                    (Mem mem) =>
                    {
                        _asm.Movss(leftXmm, mem);
                        return leftXmm;
                    },
                    (Xmm128 reg) =>
                    {
                        _asm.MovIfNeeded(leftXmm, reg);
                        return leftXmm;
                    },
                    (Xmm128 reg, Mem mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comiss(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(leftReg, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(leftReg, (RM64)leftReg);
                        _asm.Label(endLabel);
                    },
                    (Xmm128 regDest, Xmm128 regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comiss(regDest, regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(leftReg, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(leftReg, (RM64)leftReg);
                        _asm.Label(endLabel);
                    });
        return leftReg;
    }

    private Reg64? HandleConditionalBranchFloat32(Reg64 leftReg, Xmm128 leftXmm, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix, Action ifBlock, Action elseBlock)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle(rmLeft, rmRight,
                    (Mem mem) =>
                    {
                        _asm.Movss(leftXmm, mem);
                        return leftXmm;
                    },
                    (Xmm128 reg) =>
                    {
                        _asm.MovIfNeeded(leftXmm, reg);
                        return leftXmm;
                    },
                    (Xmm128 reg, Mem mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comiss(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    },
                    (Xmm128 regDest, Xmm128 regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comiss(regDest, regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    });
        return null;
    }

    #endregion Float32

    #region Float64

    private RM CompileFloat64Result(BinaryExpression binaryExpression)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat64(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true);

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat64(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat64(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true);

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
            return HandleOperatorFloat64(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        throw new InvalidOperationException("unexpected binary case");
    }

    private void CompileFloat64ResultAsConditionalBlock(BinaryExpression binaryExpression, Action thenBlock, Action elseBlock)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat64(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true, thenBlock, elseBlock);
            return;
        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat64(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat64(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorFloat64(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;

        }
        throw new InvalidOperationException("unexpected binary case");
    }

    private RM HandleOperatorFloat64(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        RM rmReturn = leftXmm;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                    {
                        return;
                    }
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);

                },
                (Xmm128 reg) =>
                {
                    _asm.Movsd(interm, reg);
                    rmRight = new RMOrImmediate(interm);
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Division)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movsd(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Divsd(reg, mem),
                (dest, src) => _asm.Divsd(dest, src));
        }
        else if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movsd(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Subsd(reg, mem),
                (dest, src) => _asm.Subsd(dest, src));
        }
        else if (op == OperatorType.Addition)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movsd(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Addsd(reg, mem),
                (dest, src) => _asm.Addsd(dest, src));

        }
        else if (op == OperatorType.Multiplication)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movsd(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Mulsd(reg, mem),
                (dest, src) => _asm.Mulsd(dest, src));
        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.AE, "CL", "CL_END");
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.A, "CLE", "CLE_END");
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.BE, "CG", "CG_END");
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.B, "CGE", "CGE_END");
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.E, "CE", "CE_END");
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.NE, "CNE", "CNE_END");
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        return rmReturn;
    }

    private void HandleOperatorFloat64(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult, Action ifBlock, Action elseBlock)
    {
        var op = binaryExpression.Operator;
        RM? rmReturn = leftXmm;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                    {
                        return;
                    }
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);

                },
                (Xmm128 reg) =>
                {
                    _asm.Movsd(interm, reg);
                    rmRight = new RMOrImmediate(interm);
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Division)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movsd(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Divsd(reg, mem),
                (dest, src) => _asm.Divsd(dest, src));
        }
        else if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a xmm register
            HandleRMOrImmediate(rmLeft,
                (mem) => _asm.Movsd(leftXmm, mem),
                (Xmm128 reg) => _asm.MovIfNeeded(leftXmm, reg));

            OperatorFactory.Handle(
                leftXmm, rmRight,
                (Xmm128 reg, Mem mem) => _asm.Subsd(reg, mem),
                (dest, src) => _asm.Subsd(dest, src));
        }
        else if (op == OperatorType.Addition)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movsd(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Addsd(reg, mem),
                (dest, src) => _asm.Addsd(dest, src));

        }
        else if (op == OperatorType.Multiplication)
        {
            rmReturn = OperatorFactory.HandleCommutative(
                rmLeft, rmRight,
                (mem) => { _asm.Movsd(leftXmm, mem); return leftXmm; },

                (Xmm128 reg, Mem mem) => _asm.Mulsd(reg, mem),
                (dest, src) => _asm.Mulsd(dest, src));
        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalBranchFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.AE, "CL", "CL_END", ifBlock, elseBlock);
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalBranchFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.A, "CLE", "CLE_END", ifBlock, elseBlock);
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalBranchFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.BE, "CG", "CG_END", ifBlock, elseBlock);
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalBranchFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.B, "CGE", "CGE_END", ifBlock, elseBlock);
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalBranchFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.E, "CE", "CE_END", ifBlock, elseBlock);
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalBranchFloat64(leftReg, leftXmm, rmLeft, rmRight, ConditionCodes.NE, "CNE", "CNE_END", ifBlock, elseBlock);
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        if (rmReturn != null)
        {
            throw new InvalidOperationException("unable to generate code for floating point truthiness");
        }
    }

    private Reg64 HandleConditionalFloat64(Reg64 leftReg, Xmm128 leftXmm, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle(rmLeft, rmRight,
                    (Mem mem) =>
                    {
                        _asm.Movsd(leftXmm, mem);
                        return leftXmm;
                    },
                    (Xmm128 reg) =>
                    {
                        _asm.MovIfNeeded(leftXmm, reg);
                        return leftXmm;
                    },
                    (Xmm128 reg, Mem mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comisd(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(leftReg, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(leftReg, (RM64)leftReg);
                        _asm.Label(endLabel);
                    },
                    (Xmm128 regDest, Xmm128 regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comisd(regDest, regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(leftReg, Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(leftReg, (RM64)leftReg);
                        _asm.Label(endLabel);
                    });
        return leftReg;
    }

    private Reg64? HandleConditionalBranchFloat64(Reg64 leftReg, Xmm128 leftXmm, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix, Action ifBlock, Action elseBlock)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle(rmLeft, rmRight,
                    (Mem mem) =>
                    {
                        _asm.Movss(leftXmm, mem);
                        return leftXmm;
                    },
                    (Xmm128 reg) =>
                    {
                        _asm.MovIfNeeded(leftXmm, reg);
                        return leftXmm;
                    },
                    (Xmm128 reg, Mem mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comisd(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    },
                    (Xmm128 regDest, Xmm128 regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Comisd(regDest, regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    });
        return null;
    }

    #endregion Float64

    #region Byte

    private Reg64 CompileByteResult(BinaryExpression binaryExpression)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            return HandleOperatorByte(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true);

        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            return HandleOperatorByte(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            return HandleOperatorByte(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true);

        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            return HandleOperatorByte(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        }

        throw new InvalidOperationException("unexpected binary case");
    }

    private void CompileByteResultAsConditionalBlock(BinaryExpression binaryExpression, Action thenBlock, Action elseBlock)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorByte(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true, thenBlock, elseBlock);
            return;
        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorByte(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorByte(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true, thenBlock, elseBlock);
            return;
        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCallOrBinary && !binaryExpression.Right.Metadata.ContainsCallOrBinary)
        {
            HandleOperatorByte(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);
            return;

        }
        throw new InvalidOperationException("unexpected binary case");
    }


    private Reg64 HandleOperatorByte(BinaryExpression binaryExpression, Reg64 leftReg64, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        var leftReg = leftReg64.ToReg8();
        var rmReturn = leftReg64;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                        return;
                    // If register does not persist accross calls
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);
                },
                (reg) => { _asm.Mov(interm, reg); rmRight = new RMOrImmediate(interm); },
                (Imm8 imm) => {
                    // Pass since immediates persist always
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg64, leftXmm);

        if (op == OperatorType.BitwiseAnd)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (RM8 mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (Imm8 imm) => { _asm.Mov(leftReg, imm.Value); return leftReg; },

                (Reg8 reg, RM8 mem) => _asm.And(reg, mem),
                (Reg8 dest, Reg8 src) => _asm.And(dest, (RM8)src),
                (Reg8 reg, Imm8 imm) => _asm.And(reg, imm.Value)).ToReg64();

        }
        else if (op == OperatorType.BitwiseOr)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (RM8 mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (Imm8 imm) => { _asm.Mov(leftReg, imm.Value); return leftReg; },

                (Reg8 reg, RM8 mem) => _asm.Or(reg, mem),
                (Reg8 dest, Reg8 src) => _asm.Or(dest, (RM8)src),
                (Reg8 reg, Imm8 imm) => _asm.Or(reg, imm.Value)).ToReg64();

        }
        else if (op == OperatorType.BitwiseXor)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (RM8 mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (Imm8 imm) => { _asm.Mov(leftReg, imm.Value); return leftReg; },

                (Reg8 reg, RM8 mem) => _asm.Xor(reg, mem),
                (Reg8 dest, Reg8 src) => _asm.Xor(dest, (RM8)src),
                (Reg8 reg, Imm8 imm) => _asm.Xor(reg, imm.Value)).ToReg64();

        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalByte(leftReg64, rmLeft, rmRight, ConditionCodes.GE, "CL", "CL_END");
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalByte(leftReg64, rmLeft, rmRight, ConditionCodes.G, "CLE", "CLE_END");
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalByte(leftReg64, rmLeft, rmRight, ConditionCodes.LE, "CG", "CG_END");
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalByte(leftReg64, rmLeft, rmRight, ConditionCodes.L, "CGE", "CGE_END");
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalByte(leftReg64, rmLeft, rmRight, ConditionCodes.NE, "CE", "CE_END");
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalByte(leftReg64, rmLeft, rmRight, ConditionCodes.E, "CNE", "CNE_END");
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        return rmReturn;
    }

    private void HandleOperatorByte(BinaryExpression binaryExpression, Reg64 leftReg64, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult, Action ifBlock, Action elseBlock)
    {
        var op = binaryExpression.Operator;
        var leftReg = leftReg64.ToReg8();
        var rmReturn = leftReg;
        var rmRight = CompileToRMOrImmediate(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRMOrImmediate(rmRight,
                (mem) =>
                {
                    if (mem is MemRegDisp memDisp && memDisp.Register == Reg64.RBP)
                        return;
                    // If register does not persist accross calls
                    _asm.Mov(rightReg, mem);
                    _asm.Mov(interm, rightReg);
                    rmRight = new RMOrImmediate(interm);
                },
                (reg) => { _asm.Mov(interm, reg); rmRight = new RMOrImmediate(interm); },
                (Imm8 imm) => {
                    // Pass since immediates persist always
                });
        }

        var rmLeft = CompileToRMOrImmediate(binaryExpression.Left, leftReg64, leftXmm);

        if (op == OperatorType.BitwiseAnd)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (RM8 mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (Imm8 imm) => { _asm.Mov(leftReg, imm.Value); return leftReg; },

                (Reg8 reg, RM8 mem) => _asm.And(reg, mem),
                (Reg8 dest, Reg8 src) => _asm.And(dest, (RM8)src),
                (Reg8 reg, Imm8 imm) => _asm.And(reg, imm.Value));

        }
        else if (op == OperatorType.BitwiseOr)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (RM8 mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (Imm8 imm) => { _asm.Mov(leftReg, imm.Value); return leftReg; },

                (Reg8 reg, RM8 mem) => _asm.Or(reg, mem),
                (Reg8 dest, Reg8 src) => _asm.Or(dest, (RM8)src),
                (Reg8 reg, Imm8 imm) => _asm.Or(reg, imm.Value));

        }
        else if (op == OperatorType.BitwiseXor)
        {
            rmReturn = OperatorFactory.HandleCommutative(rmLeft, rmRight,
                (RM8 mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (Imm8 imm) => { _asm.Mov(leftReg, imm.Value); return leftReg; },

                (Reg8 reg, RM8 mem) => _asm.Xor(reg, mem),
                (Reg8 dest, Reg8 src) => _asm.Xor(dest, (RM8)src),
                (Reg8 reg, Imm8 imm) => _asm.Xor(reg, imm.Value));

        }
        else if (op == OperatorType.LessThan)
            rmReturn = HandleConditionalBranchByte(leftReg64, rmLeft, rmRight, ConditionCodes.GE, "CL", "CL_END", ifBlock, elseBlock);
        else if (op == OperatorType.LessThanOrEqual)
            rmReturn = HandleConditionalBranchByte(leftReg64, rmLeft, rmRight, ConditionCodes.G, "CLE", "CLE_END", ifBlock, elseBlock);
        else if (op == OperatorType.GreaterThan)
            rmReturn = HandleConditionalBranchByte(leftReg64, rmLeft, rmRight, ConditionCodes.LE, "CG", "CG_END", ifBlock, elseBlock);
        else if (op == OperatorType.GreaterThanOrEqual)
            rmReturn = HandleConditionalBranchByte(leftReg64, rmLeft, rmRight, ConditionCodes.L, "CGE", "CGE_END", ifBlock, elseBlock);
        else if (op == OperatorType.Equality)
            rmReturn = HandleConditionalBranchByte(leftReg64, rmLeft, rmRight, ConditionCodes.NE, "CE", "CE_END", ifBlock, elseBlock);
        else if (op == OperatorType.NotEqual)
            rmReturn = HandleConditionalBranchByte(leftReg64, rmLeft, rmRight, ConditionCodes.E, "CNE", "CNE_END", ifBlock, elseBlock);
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        if (rmReturn != null)
        {
            // then we still need to test for the branch
            var falseLabel = _asm.CreateUniqueLabel("CTST_");
            var endLabel = _asm.CreateUniqueLabel("CTST_END_");
            _asm.Test(rmReturn, rmReturn);
            _asm.Jz(Rel32.Create(falseLabel));
            ifBlock();
            _asm.Jmp(Rel32.Create(endLabel));
            _asm.Label(falseLabel);
            elseBlock();
            _asm.Label(endLabel);
        }
    }

    private Reg64 HandleConditionalByte(Reg64 leftReg64, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        var leftReg = leftReg64.ToReg8();
        OperatorFactory.Handle(rmLeft, rmRight,
                    (RM8 mem) =>
                    {
                        _asm.Mov(leftReg, mem);
                        return leftReg;
                    },
                    (Reg8 reg) =>
                    {
                        _asm.MovIfNeeded(leftReg, reg);
                        return leftReg;
                    },
                    (Imm8 imm) =>
                    {
                        _asm.Mov(leftReg, imm.Value);
                        return leftReg;
                    },
                    (Reg8 reg, RM8 mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(reg.ToReg64(), Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(reg.ToReg64(), (RM64)reg.ToReg64());
                        _asm.Label(endLabel);
                    },
                    (Reg8 regDest, Reg8 regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(regDest, (RM8)regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(regDest.ToReg64(), Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(regDest.ToReg64(), (RM64)regDest.ToReg64());
                        _asm.Label(endLabel);
                    },
                    (Reg8 reg, Imm8 imm) =>
                    {
                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, imm.Value);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        _asm.Mov(reg.ToReg64(), Imm32.Create(1));
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        _asm.Xor(reg.ToReg64(), (RM64)reg.ToReg64());
                        _asm.Label(endLabel);
                    });
        return leftReg64;
    }

    private Reg8? HandleConditionalBranchByte(Reg64 leftReg64, RMOrImmediate rmLeft, RMOrImmediate rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix, Action ifBlock, Action elseBlock)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        var leftReg = leftReg64.ToReg8();
        OperatorFactory.Handle(rmLeft, rmRight,
                    (RM8 mem) =>
                    {
                        _asm.Mov(leftReg, mem);
                        return leftReg;
                    },
                    (Reg8 reg) =>
                    {
                        _asm.MovIfNeeded(leftReg, reg);
                        return leftReg;
                    },
                    (Imm8 imm) =>
                    {
                        _asm.Mov(leftReg, imm.Value);
                        return leftReg;
                    },
                    (Reg8 reg, RM8 mem) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, mem);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    },
                    (Reg8 regDest, Reg8 regSrc) =>
                    {

                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(regDest, (RM8)regSrc);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    },
                    (Reg8 reg, Imm8 imm) =>
                    {
                        var falsePath = _asm.CreateUniqueLabel(labelPrefix);
                        var endLabel = _asm.CreateUniqueLabel(endLabelPrefix);
                        _asm.Cmp(reg, imm.Value);
                        _asm.Jcc(Rel32.Create(falsePath), conditionCode);
                        ifBlock();
                        _asm.Jmp(Rel32.Create(endLabel));
                        _asm.Label(falsePath);
                        elseBlock();
                        _asm.Label(endLabel);
                    });
        return null;
    }
    #endregion Byte

    #endregion

    #endregion

    #region Call
    private RM Compile(CallExpression callExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        // Will always return in RAX or Xmm0, then move to desired reg

        // align stack

        // if argument count is odd and greater than 4, we need to subtract an additional 8 bytes from RSP to keep RSP 16 byte aligned
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
            var rmArg = CompileToRMOrImmediate(argument, Reg64.RAX, Xmm128.XMM0);
            HandleRMOrImmediateForPush(rmArg,
                (mem) => _asm.Push((RM64)mem),
                (reg) => 
                _asm.Push(reg),
                (xmm) => {
                    _asm.Sub(Reg64.RSP, 8);
                    _asm.Movsd(Mem64.Create(Reg64.RSP), Xmm128.XMM0);
                },
                (imm) => _asm.Push(imm.Value),
                (imm) => _asm.Push(imm.Value));
        }
        if (callExpression.FunctionContext != null)
        {
            if (callExpression.FunctionContext.IsImported)
            {
                // imported function call
                var registerPassedArgumentCount = Math.Min(callExpression.Arguments.Count, 4);
                for (int i = 0; i < registerPassedArgumentCount; i++)
                {
                    var argument = callExpression.Arguments[i];
                    var mem = Mem64.Create(Reg64.RSP, i * 8); // RSP points directly at last pushed value, so first arg should be at offset 0 from rsp

                    if (argument.IsFloat32()) _asm.Movss(SelectXmmRegister(i), mem);
                    else if (argument.IsFloat64()) _asm.Movsd(SelectXmmRegister(i), mem);
                    else if (argument.IsByteType()) _asm.Mov(SelectGeneralRegister(i).ToReg8(), mem);
                    else _asm.Mov(SelectGeneralRegister(i), mem);
                }
                _asm.Call((RM64)Mem64.Create(callExpression.FunctionContext.GetDecoratedFunctionLabel()));
            }
            // it is a direct call
            else _asm.Call(Rel32.Create(callExpression.FunctionContext.GetDecoratedFunctionLabel()));
        }
        else
        {
            // Indirect call
            var indirect = Compile(callExpression.Callee, desiredReg, desiredXmm);
            if (indirect is RM64 rm)
            {
                _asm.Call(rm);
            }
            else throw new InvalidOperationException($"unexpected rm type in call. rm type was '{indirect.GetType()}'");
        }
        _asm.Add(Reg64.RSP, (callExpression.Arguments.Count * 8) + additionalStackToSubtract); // clear stack of arguments
        if (callExpression.IsFloat32() || callExpression.IsFloat64())
        {
            _asm.MovIfNeeded(desiredXmm, Xmm128.XMM0);
            return desiredXmm;
        }
        else if (callExpression.IsByteType())
        {
            _asm.MovIfNeeded(desiredReg.ToReg8(), Reg8.AL);
            return desiredReg.ToReg8();
        }
        _asm.MovIfNeeded(desiredReg, Reg64.RAX);
        return desiredReg;
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
            case 3: return Reg64.R9;
            default:
                throw new NotSupportedException("general register arguments not supported past parameter 4");
        }
    }

    #endregion
   

    #endregion


    #region Metadata Gathering
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
            case DereferenceAssignmentStatement dereferenceAssignmentStatement:
                GatherMetadata(dereferenceAssignmentStatement);
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
            case ContinueStatement continueStatement:
                GatherMetadata(continueStatement);
                break;
            case BreakStatement breakStatement:
                GatherMetadata(breakStatement);
                break;
            default:
                throw new InvalidOperationException($"Unknown statement type '{statement.GetType()}'");
        }
    }

    private void GatherMetadata(AssignmentStatement assignmentStatement)
    {
        GatherMetadata(assignmentStatement.Value);
        assignmentStatement.Metadata.StackSlotsNeeded = assignmentStatement.Value.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(DereferenceAssignmentStatement dereferenceAssignmentStatement)
    {
        GatherMetadata(dereferenceAssignmentStatement.Value);
        dereferenceAssignmentStatement.Metadata.StackSlotsNeeded = dereferenceAssignmentStatement.Value.Metadata.StackSlotsNeeded;
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

    private void GatherMetadata(ContinueStatement continueStatement)
    {

    }

    private void GatherMetadata(BreakStatement breakStatement)
    {

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

    private void GatherMetadata(WhileStatement whileStatement)
    {
        GatherMetadata(whileStatement.ConditionalTarget);
        int maxStackSlotsNeeded = whileStatement.ConditionalTarget.Metadata.StackSlotsNeeded;
        foreach (var statment in whileStatement.ThenBlock)
        {
            GatherMetadata(statment);
            maxStackSlotsNeeded = Math.Max(maxStackSlotsNeeded, statment.Metadata.StackSlotsNeeded);
        }
        whileStatement.Metadata.StackSlotsNeeded = maxStackSlotsNeeded;
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
            case DereferenceExpression dereferenceExpression:
                GatherMetadata(dereferenceExpression);
                break;
            case CastExpression castExpression:
                GatherMetadata(castExpression);
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
        binaryExpression.Metadata.ContainsCallOrBinary = true; // since it IS a binary

        if (binaryExpression.Left.Metadata.ContainsCallOrBinary && binaryExpression.Right.Metadata.ContainsCallOrBinary)
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
        callExpression.Metadata.ContainsCallOrBinary = true; // since it IS a call
        callExpression.Metadata.StackSlotsNeeded = callExpression.Arguments.Count > 0? callExpression.Arguments.Max(a => a.Metadata.StackSlotsNeeded): 0;
    }

    private void GatherMetadata(IdentifierExpression identifierExpression)
    {
        identifierExpression.Metadata.SU = 0;
        identifierExpression.Metadata.ContainsCallOrBinary = false;
        identifierExpression.Metadata.StackSlotsNeeded = 0;
    }

    private void GatherMetadata(LiteralExpression literalExpression)
    {
        literalExpression.Metadata.SU = 1;
        literalExpression.Metadata.ContainsCallOrBinary = false;
        literalExpression.Metadata.StackSlotsNeeded = 0;
    }

    private void GatherMetadata(MemberAccessExpression memberAccessExpression)
    {
        GatherMetadata(memberAccessExpression.Instance);

        memberAccessExpression.Metadata.SU = memberAccessExpression.Instance.Metadata.SU;
        memberAccessExpression.Metadata.ContainsCallOrBinary = memberAccessExpression.Instance.Metadata.ContainsCallOrBinary;
        memberAccessExpression.Metadata.StackSlotsNeeded = memberAccessExpression.Instance.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(ReferenceExpression referenceExpression)
    {
        if (referenceExpression.Instance != null)
        {
            GatherMetadata(referenceExpression.Instance);
            referenceExpression.Metadata.SU = referenceExpression.Instance.Metadata.SU;
            referenceExpression.Metadata.ContainsCallOrBinary = referenceExpression.Instance.Metadata.ContainsCallOrBinary;
            referenceExpression.Metadata.StackSlotsNeeded = referenceExpression.Instance.Metadata.StackSlotsNeeded;
        }
        else
        {
            referenceExpression.Metadata.SU = 1;
            referenceExpression.Metadata.ContainsCallOrBinary = false;
            referenceExpression.Metadata.StackSlotsNeeded = 0;
        }
    }

    private void GatherMetadata(DereferenceExpression dereferenceExpression)
    {
        GatherMetadata(dereferenceExpression.Target);
        dereferenceExpression.Metadata.ContainsCallOrBinary = dereferenceExpression.Target.Metadata.ContainsCallOrBinary;
        dereferenceExpression.Metadata.StackSlotsNeeded = dereferenceExpression.Target.Metadata.StackSlotsNeeded;
    }

    private void GatherMetadata(StackAllocateExpression stackAllocateExpression)
    {
        // Pass 
        // Stack allocation should not occur naturally in expression trees
    }

    private void GatherMetadata(CastExpression castExpression)
    {
        GatherMetadata(castExpression.Value);
        castExpression.Metadata.ContainsCallOrBinary = castExpression.Value.Metadata.ContainsCallOrBinary;
        castExpression.Metadata.StackSlotsNeeded = castExpression.Value.Metadata.StackSlotsNeeded;
    }

    #endregion

    #region Helpers

    private void HandleRMOrImmediateForPush(RMOrImmediate rmOrImm, Action<Mem> handleMemory, Action<Reg64> handleRegister, Action<Xmm128> handleXmm, Action<Imm32> handleImm32, Action<Imm8> handleImm8)
    {
        if (rmOrImm.IsReg<Xmm128>(out var xmm)) handleXmm(xmm);
        else if (rmOrImm.IsReg<Reg64>(out var reg)) handleRegister(reg);
        else if (rmOrImm.IsMem) handleMemory(rmOrImm.Mem);
        else if (rmOrImm.IsImm32) handleImm32(rmOrImm.Imm32);
        else if (rmOrImm.IsImm8) handleImm8(rmOrImm.Imm8);
        else
            throw new InvalidOperationException($"unexpected RM or Immeidate type '{rmOrImm.GetType()}'");
    }

    private void HandleRM(RM rm, Action<Mem> handleMemory, Action<Reg64> handleReg)
    {
        if (rm is Reg64 reg)
            handleReg(reg);
        else if (rm is Mem mem)
            handleMemory(mem);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }

    private Mem64 HandleRM(RM rm, Func<Mem64, Mem64> handleMemory, Func<Reg64, Mem64> handleRegister)
    {
        if (rm is Reg64 reg)
            return handleRegister(reg);
        else if (rm is Mem64 mem)
            return handleMemory(mem);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }

    private Reg64 HandleRM(RM rm, Func<Mem64, Reg64> handleMemory, Func<Reg64, Reg64> handleRegister)
    {
        if (rm is Reg64 reg)
            return handleRegister(reg);
        else if (rm is Mem64 mem)
            return handleMemory(mem);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }


    private void HandleRMOrImmediate(RMOrImmediate rmOrImm, Action<Mem> handleMemory, Action<Reg64> handleRegister, Action<Imm32> handleImm32)
    {
        if (rmOrImm.IsReg<Reg64>(out var reg)) handleRegister(reg);
        else if (rmOrImm.IsMem) handleMemory(rmOrImm.Mem);
        else if (rmOrImm.IsImm32) handleImm32(rmOrImm.Imm32);
        else
            throw new InvalidOperationException($"unexpected RM or immediate type '{rmOrImm.GetType()}'");
    }

    private void HandleRMOrImmediate(RMOrImmediate rmOrImm, Action<Mem> handleMemory, Action<Reg8> handleRegister, Action<Imm8> handleImm8)
    {
        if (rmOrImm.IsReg<Reg8>(out var reg)) handleRegister(reg);
        else if (rmOrImm.IsMem) handleMemory(rmOrImm.Mem);
        else if (rmOrImm.IsImm8) handleImm8(rmOrImm.Imm8);
        else
            throw new InvalidOperationException($"unexpected RM or immediate type '{rmOrImm.GetType()}'");
    }


    private void HandleRMOrImmediate(RMOrImmediate rmOrImm, Action<Mem> handleMemory, Action<Xmm128> handleRegister)
    {
        // Floating points will not have immediates
        if (rmOrImm.IsReg<Xmm128>(out var reg)) handleRegister(reg);
        else if (rmOrImm.IsMem) handleMemory(rmOrImm.Mem);
        else
            throw new InvalidOperationException($"unexpected RM or immediate type '{rmOrImm.GetType()}'");
    }

    private void HandleRM(RM rm, Action<Mem> handleMemory, Action<Xmm128> handleRegister)
    {
        // Floating points will not have immediates
        if (rm is Xmm128 reg) handleRegister(reg);
        else if (rm is Mem mem) handleMemory(mem);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }


    private int GetAdditonalStackSlotsNeeded(FunctionContext functionContext)
    {
        foreach (var statement in functionContext.BodyStatements)
        {
            GatherMetadata(statement);
        }
        return functionContext.BodyStatements.Max(s => s.Metadata.StackSlotsNeeded);
    }

    #endregion
}





internal static class OperatorFactory
{

    #region 64 bit ops

    internal static void Handle(Reg64 left, RMOrImmediate right, Action<Reg64, RM64> memToReg, Action<Reg64, Reg64> regToReg, Action<Reg64, Imm32> imm32ToReg)
    {
        if (right.IsReg<Reg64>(out var rightAsReg)) regToReg(left, rightAsReg);
        else if (right.IsRM64) memToReg(left, right.RM64);
        else if (right.IsImm32) imm32ToReg(left, right.Imm32);
        else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
    }

    public static void Handle(RMOrImmediate rmLeft, RMOrImmediate right, 
        Func<RM64, Reg64> moveLeftToReg, Func<Reg64, Reg64> moveRegLeftToReg, Func<Imm32, Reg64> moveImm32LeftToReg,  
        Action<Reg64, RM64> memToReg, Action<Reg64, Reg64> regToReg, Action<Reg64, Imm32> imm32ToReg)
    {
        Reg64 left;
        if (rmLeft.IsReg<Reg64>(out var leftAsReg)) left = moveRegLeftToReg(leftAsReg);
        else if (rmLeft.IsRM64) left = moveLeftToReg(rmLeft.RM64);
        else if (rmLeft.IsImm32) left = moveImm32LeftToReg(rmLeft.Imm32);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{rmLeft.GetType()}'");

        if (right.IsReg<Reg64>(out var rightAsReg)) regToReg(left, rightAsReg);
        else if (right.IsRM64) memToReg(left, right.RM64);
        else if (right.IsImm32) imm32ToReg(left, right.Imm32);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{right.GetType()}'");
    }


    public static Reg64 HandleCommutative(RMOrImmediate left, RMOrImmediate right, 
        Func<RM64, Reg64> dumpMemLeftToReg,    // These are used in the case of incompatible operands
        Func<Imm32, Reg64> dumpImm32LeftToReg, // Mem to Mem, Imm to Imm, 
                                               // Imm to Mem is also disallowed since destination should be a register

        Action<Reg64, RM64> memToReg, 
        Action<Reg64, Reg64> regToReg, 
        Action<Reg64, Imm32> imm32ToReg) 
    {
        Reg64 leftAsReg;
        if (!left.IsReg(out leftAsReg))
        {
            if (right.IsReg<Reg64>(out var rightAsReg))
            {
                if (left.IsRM64) memToReg(rightAsReg, left.RM64);
                else if (left.IsImm32) imm32ToReg(rightAsReg, left.Imm32);
                else throw new InvalidOperationException($"unexpected left RM or immediate type");
                return rightAsReg;
            }

            if (left.IsImm32) leftAsReg = dumpImm32LeftToReg(left.Imm32);
            else if (left.IsRM64) leftAsReg  = dumpMemLeftToReg(left.RM64);
            else throw new InvalidOperationException($"unexpected left RM or immediate type");
            
        }

        if (right.IsReg<Reg64>(out var rightReg)) regToReg(leftAsReg, rightReg);
        else if (right.IsRM64) memToReg(leftAsReg, right.RM64);
        else if (right.IsImm32) imm32ToReg(leftAsReg, right.Imm32);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{right.GetType()}'");

        return leftAsReg;
    }

    #endregion


    #region 8Bit Ops

    internal static void Handle(Reg8 left, RMOrImmediate right, Action<Reg8, RM8> memToReg, Action<Reg8, Reg8> regToReg, Action<Reg8, Imm8> imm8ToReg)
    {
        if (right.IsReg<Reg8>(out var rightAsReg)) regToReg(left, rightAsReg);
        else if (right.IsRM8) memToReg(left, right.RM8);
        else if (right.IsImm8) imm8ToReg(left, right.Imm8);
        else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
    }

    public static void Handle(RMOrImmediate rmLeft, RMOrImmediate right, Func<RM8, Reg8> moveLeftToReg, Func<Reg8, Reg8> moveRegLeftToReg, Func<Imm8, Reg8> moveImm8LeftToReg,
        Action<Reg8, RM8> memToReg, Action<Reg8, Reg8> regToReg, Action<Reg8, Imm8> imm8ToReg)
    {
        Reg8 left;
        if (rmLeft.IsReg<Reg8>(out var leftAsReg)) left = moveRegLeftToReg(leftAsReg);
        else if (rmLeft.IsRM64) left = moveLeftToReg(rmLeft.RM8);
        else if (rmLeft.IsImm8) left = moveImm8LeftToReg(rmLeft.Imm8);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{rmLeft.GetType()}'");

        if (right.IsReg<Reg8>(out var rightAsReg)) regToReg(left, rightAsReg);
        else if (right.IsRM64) memToReg(left, right.RM8);
        else if (right.IsImm8) imm8ToReg(left, right.Imm8);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{right.GetType()}'");
    }


    public static Reg8 HandleCommutative(RMOrImmediate left, RMOrImmediate right,
        Func<RM8, Reg8> dumpMemLeftToReg,    // These are used in the case of incompatible operands
        Func<Imm8, Reg8> dumpImm8LeftToReg, // Mem to Mem, Imm to Imm, 
                                               // Imm to Mem is also disallowed since destination should be a register

        Action<Reg8, RM8> memToReg,
        Action<Reg8, Reg8> regToReg,
        Action<Reg8, Imm8> imm8ToReg)
    {
        Reg8 leftAsReg;
        if (!left.IsReg(out leftAsReg))
        {
            if (right.IsReg<Reg8>(out var rightAsReg))
            {
                if (left.IsRM8) memToReg(rightAsReg, left.RM8);
                else if (left.IsImm8) imm8ToReg(rightAsReg, left.Imm8);
                else throw new InvalidOperationException($"unexpected left RM or immediate type");
                return rightAsReg;
            }

            if (left.IsImm8) leftAsReg = dumpImm8LeftToReg(left.Imm8);
            else if (left.IsRM8) leftAsReg = dumpMemLeftToReg(left.RM8);
            else throw new InvalidOperationException($"unexpected left RM or immediate type");

        }

        if (right.IsReg<Reg8>(out var rightReg)) regToReg(leftAsReg, rightReg);
        else if (right.IsRM8) memToReg(leftAsReg, right.RM8);
        else if (right.IsImm8) imm8ToReg(leftAsReg, right.Imm8);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{right.GetType()}'");

        return leftAsReg;
    }

    #endregion

    #region Float32 ops

    internal static void Handle(Xmm128 left, RMOrImmediate right, Action<Xmm128, Mem> memToReg, Action<Xmm128, Xmm128> regToReg)
    {
        if (right.IsReg<Xmm128>(out var rightAsReg)) regToReg(left, rightAsReg);
        else if (right.IsMem) memToReg(left, right.Mem);
        else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
    }

    public static void Handle(RMOrImmediate rmLeft, RMOrImmediate right, 
        Func<Mem, Xmm128> moveMemLeftToReg, 
        Func<Xmm128, Xmm128> moveRegLeftToReg,
        Action<Xmm128, Mem> memToReg, 
        Action<Xmm128, Xmm128> regToReg)
    {
        Xmm128 left;
        if (rmLeft.IsReg<Xmm128>(out var leftAsReg)) left = moveRegLeftToReg(leftAsReg);
        else if (rmLeft.IsMem) left = moveMemLeftToReg(rmLeft.Mem);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{rmLeft.GetType()}'");

        if (right.IsReg<Xmm128>(out var rightAsReg)) regToReg(left, rightAsReg);
        else if (right.IsMem) memToReg(left, right.Mem);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{right.GetType()}'");
    }


    public static Xmm128 HandleCommutative(RMOrImmediate left, RMOrImmediate right,
        Func<Mem, Xmm128> dumpMemLeftToReg,    // These are used in the case of incompatible operands
                                               // Mem to Mem, Imm to Imm, 
                                               // Imm to Mem is also disallowed since destination should be a register

        Action<Xmm128, Mem> memToReg,
        Action<Xmm128, Xmm128> regToReg)
    {
        Xmm128 leftAsReg;
        if (!left.IsReg(out leftAsReg))
        {
            if (right.IsReg<Xmm128>(out var rightAsReg))
            {
                if (left.IsMem) memToReg(rightAsReg, left.Mem);
                else throw new InvalidOperationException($"unexpected left RM or immediate type");
                return rightAsReg;
            }

            if (left.IsMem) leftAsReg = dumpMemLeftToReg(left.Mem);
            else throw new InvalidOperationException($"unexpected left RM or immediate type");

        }

        if (right.IsReg<Xmm128>(out var rightReg)) regToReg(leftAsReg, rightReg);
        else if (right.IsMem) memToReg(leftAsReg, right.Mem);
        else throw new InvalidOperationException($"unexpected RM or immediate type '{right.GetType()}'");

        return leftAsReg;
    }

    #endregion

}

internal class RMOrImmediate
{
    public bool IsImmediate => _imm != null;
    public bool IsImm8 => _imm is Imm8;
    public bool IsImm32 => _imm is Imm32;
    public bool IsRM => _rm != null;
    public bool IsRM8 => _rm is RM8;
    public bool IsRM32 => _rm is RM32;
    public bool IsMem => _rm is Mem;
    public bool IsRM64 => _rm is RM64;
    public bool IsRM128 => _rm is RM128;
    public bool IsReg<TReg>(out TReg reg) where TReg : Register
    {
        reg = default!;
        if (IsRM && RM is TReg typedReg)
        {
            reg = typedReg;
            return true;
        }
        return false;
    }
    public Imm8 Imm8 => _imm as Imm8 ?? throw new ArgumentNullException(nameof(Imm8));
    public Imm32 Imm32 => _imm as Imm32 ?? throw new ArgumentNullException(nameof(Imm32));
    public RM RM => _rm ?? throw new ArgumentNullException(nameof(RM));
    public RM8 RM8 => _rm as RM8 ?? throw new ArgumentNullException(nameof(RM8));
    public RM32 RM32 => _rm as RM32 ?? throw new ArgumentNullException(nameof(RM32));
    public RM64 RM64 => _rm as RM64 ?? throw new ArgumentNullException(nameof(RM64));
    public RM128 RM128 => _rm as RM128 ?? throw new ArgumentNullException(nameof(RM128));
    public Mem Mem => _rm as Mem ?? throw new ArgumentNullException(nameof(Mem));
    public RMOrImmediate(Immediate imm)
    {
        _imm = imm;
    }
    public RMOrImmediate(RM rm)
    {
        _rm = rm;
    }

    private Immediate? _imm;
    private RM? _rm;

}

public class BoxedString : IInstructionData
{
    public BoxedString(int stringLength, string stringLabel)
    {
        StringLength = stringLength;
        StringLabel = stringLabel;
    }

    public string Label { get; set; } = "";
    
    public int StringLength { get; set; }
    public string StringLabel { get; set; }


    public X86Instruction[] Emit()
    {
        return [new DefineQuadWord(StringLength), new DefineQuadWord_Address(Rel32.Create(StringLabel))];
    }
}