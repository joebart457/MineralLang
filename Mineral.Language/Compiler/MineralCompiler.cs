
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;
using System.Runtime.InteropServices;
using Tokenizer.Core.Models;
using X86_64Assembler;
using static System.Runtime.InteropServices.JavaScript.JSType;


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
            case AssignmentStatement assignmentStatement:
                Compile(assignmentStatement);
                break;
            default:
                throw new InvalidOperationException($"Unknown statement type '{statement.GetType()}'");
        }
    }

    private void Compile(ReturnStatement returnStatement)
    {
        var rm = Compile(returnStatement.ValueToReturn, Reg64.RAX, Xmm128.XMM0);
        if (returnStatement.ValueToReturn.IsFloatingPointType())
        {
            HandleRM(rm,
                (mem) => _asm.Movss(Xmm128.XMM0, mem),
                (xmm) => _asm.Movaps(Xmm128.XMM0, (RM128)xmm));
        }
        else
        {
            HandleRM(rm,
                (mem) => _asm.Mov(Reg64.RAX, mem),
                (reg) => _asm.MovIfNeeded(Reg64.RAX, reg),
                (imm) => _asm.Mov(Reg64.RAX, imm));
        }

        if (_ctx.IsErrorable) _asm.Xor(Reg64.RDX, (RM64)Reg64.RDX); // Zero out error before returning value
        _asm.TearDownStackFrame();
        _asm.Ret();
    }

    private void Compile(ErrorStatement errorStatement)
    {
        var rm = Compile(errorStatement.ErrorToReturn, Reg64.RDX, Xmm128.XMM0);
        HandleRM(rm,
            (mem) => _asm.Mov(Reg64.RDX, mem),
            (reg) => _asm.MovIfNeeded(Reg64.RDX, reg),
            (imm) => _asm.Mov(Reg64.RDX, imm));

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
        var regToTest = Reg64.RAX;
        HandleRM(rmCondition,
            (mem) => _asm.Mov(regToTest, mem),
            (reg) => _asm.MovIfNeeded(regToTest, reg),
            (imm) => _asm.Mov(regToTest, imm));


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

    private void Compile(AssignmentStatement assignmentStatement)
    {
        var errorTarget = assignmentStatement.ErrorTarget;
        if (errorTarget == null && assignmentStatement.Value is CallExpression callExpression && callExpression.Callee.ConcreteType is CallableType callableType && callableType.ReturnType.IsVoidType() && callableType.IsErrorable)
        {
            errorTarget = assignmentStatement.AssignmentTarget;
            Compile(callExpression, Reg64.RAX, Xmm128.XMM0); // ??? TODO
        }
        else
        {
            var rmValue = Compile(assignmentStatement.Value, Reg64.RAX, Xmm128.XMM0);
            var assignmentTarget = CompileAndReturnAssignmentTargetFromLValue(assignmentStatement.AssignmentTarget, Reg64.RCX);

            if (assignmentTarget != null)
            {
                if (assignmentStatement.AssignmentTarget.IsFloatingPointType())
                {
                    HandleRM(rmValue,
                        (mem) =>
                        {
                            _asm.Movss(Xmm128.XMM0, mem);
                            _asm.Movss(assignmentTarget, Xmm128.XMM0);
                        },
                        (Xmm128 xmm) => _asm.Movss(assignmentTarget, xmm));
                }
                else
                {
                    HandleRM(rmValue,
                        (mem) =>
                        {
                            _asm.Mov(Reg64.RAX, mem);
                            _asm.Mov(assignmentTarget, Reg64.RAX);
                        },
                        (reg) => _asm.Mov(assignmentTarget, reg),
                        (imm) => _asm.Mov(assignmentTarget, imm));
                }
            }




            // Errors returned in edx
            if (errorTarget != null)
            {
                var errorTargetMemoryLocation = CompileAndReturnAssignmentTargetFromLValue(errorTarget, Reg64.RDX);

                if (errorTargetMemoryLocation != null)
                {
                    HandleRM(rmValue,
                       (mem) =>
                       {
                           _asm.Mov(Reg64.RDX, mem);
                           _asm.Mov(errorTargetMemoryLocation, Reg64.RDX);
                       },
                       (reg) => _asm.Mov(errorTargetMemoryLocation, reg),
                       (imm) => _asm.Mov(errorTargetMemoryLocation, imm));
                } // else pass for discard
            }

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

                // If it is a struct type is it guaranteed to be on the stack
                if (instanceMemoryLocation.Register != Reg64.RBP || instanceMemoryLocation.Displacement is not Disp32 disp32) throw new InvalidOperationException("structs should only be stack allocated");
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

    #endregion


    #region Happy path expression compilation

    private RM Compile(ExpressionBase expression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        switch (expression)
        {
            case CallExpression callExpression:
                return Compile(callExpression, desiredReg, desiredXmm);
            case IdentifierExpression identifierExpression:
                return Compile(identifierExpression);
            case MemberAccessExpression memberAccessExpression:
                return Compile(memberAccessExpression, desiredReg, desiredXmm);
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

    private RM Compile(LiteralExpression literalExpression)
    {
        if (literalExpression.Value is int i)
            return Imm32.Create(i);
        else if (literalExpression.Value is float flt)
        {
            return _asm.AddFloatingPoint(flt);
        }
        else if (literalExpression.Value is string)
            literalExpression.TagAsType(new ConcreteType(BuiltinType.String));
        else if (literalExpression.Value is null)
            literalExpression.TagAsType(new NullPointerType());
        else
            errors.Add(literalExpression, $"Unknown literal type for value '{literalExpression.Value}'");
    }

    private Mem64 Compile(MemberAccessExpression memberAccessExpression, Reg64 desiredReg, Xmm128 desiredXmm)
    {
        var instanceLocation = Compile(memberAccessExpression.Instance, desiredReg, desiredXmm);
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
                    _asm.Lea(desiredReg, Mem64.Create((Reg64)mem.Register, mem.Displacement.Offset + offset));
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

    private RM Compile(BinaryExpression binaryExpression)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;
        
        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCall && !binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, true);

            return HandleOperator(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true);

        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCall && binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, false);

            return HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCall && binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, true);

            return HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true);

        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCall && !binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                return HandleDivision(binaryExpression, false);

            return HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false);

        }

        throw new InvalidOperationException("unexpected binary case");
    }

    private void CompileAsConditionalBlock(BinaryExpression binaryExpression, Action thenBlock, Action elseBlock)
    {
        // Binary operations always compute RHS first to keep it simple and avoid too much register swapping
        var op = binaryExpression.Operator;

        // Case 1: Lhs = CALL, Rhs = NonCall
        if (binaryExpression.Left.Metadata.ContainsCall && !binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, true, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RAX, Reg64.RCX, Xmm128.XMM0, Xmm128.XMM1, true, thenBlock, elseBlock);

        }

        // Case 2: Lhs = NonCall, Rhs = CALL
        if (!binaryExpression.Left.Metadata.ContainsCall && binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, false, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);

        }

        // Case 3: Lhs = CALL, Rhs = CALL
        if (binaryExpression.Left.Metadata.ContainsCall && binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, true, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, true, thenBlock, elseBlock);

        }

        // Case 3: Lhs = NonCall, Rhs = NonCall
        if (!binaryExpression.Left.Metadata.ContainsCall && !binaryExpression.Right.Metadata.ContainsCall)
        {

            if (op == OperatorType.Division || op == OperatorType.Modulus)
                HandleDivision(binaryExpression, false, thenBlock, elseBlock);

            else HandleOperator(binaryExpression, Reg64.RCX, Reg64.RAX, Xmm128.XMM1, Xmm128.XMM0, false, thenBlock, elseBlock);

        }

        throw new InvalidOperationException("unexpected binary case");
    }


    #region Binary Helpers

    private RM HandleDivision(BinaryExpression binaryExpression, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        var leftReg = Reg64.RAX;
        var rightReg = Reg64.RCX;
        var rmRight = Compile(binaryExpression.Right, rightReg, Xmm128.XMM1);
        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRM(rmRight,
                (mem) =>
                {
                    if (mem.Register != Reg64.RBP)
                    {
                        // If register does not persist accross calls
                        _asm.Mov(rightReg, mem);
                        _asm.Mov(interm, rightReg);
                        rmRight = interm;
                    }

                },
                (reg) => { _asm.Mov(interm, reg); rmRight = interm; },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }
        var rmLeft = Compile(binaryExpression.Right, leftReg, Xmm128.XMM0);

        // Ensure lhs is in RAX
        HandleRM(rmLeft,
            (mem) => _asm.Mov(leftReg, mem),
            (reg) => _asm.MovIfNeeded(leftReg, reg),
            (Imm32 imm) => _asm.Mov(leftReg, imm));

        OperatorFactory.Handle<Reg64, Imm32>(
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
            (reg, imm) =>
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
        var op = binaryExpression.Operator;
        var leftReg = Reg64.RAX;
        var rightReg = Reg64.RCX;
        var rmRight = Compile(binaryExpression.Right, rightReg, Xmm128.XMM1);
        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRM(rmRight,
                (mem) =>
                {
                    if (mem.Register != Reg64.RBP)
                    {
                        // If register does not persist accross calls
                        _asm.Mov(rightReg, mem);
                        _asm.Mov(interm, rightReg);
                        rmRight = interm;
                    }

                },
                (reg) => { _asm.Mov(interm, reg); rmRight = interm; },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }
        var rmLeft = Compile(binaryExpression.Right, leftReg, Xmm128.XMM0);

        // Ensure lhs is in RAX
        HandleRM(rmLeft,
            (mem) => _asm.Mov(leftReg, mem),
            (reg) => _asm.MovIfNeeded(leftReg, reg),
            (Imm32 imm) => _asm.Mov(leftReg, imm));

        OperatorFactory.Handle<Reg64, Imm32>(
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
            (reg, imm) =>
            {
                _asm.Cqo();
                _asm.Mov(rightReg, imm);
                _asm.Idiv(rightReg);
            });

        if (spillRightResult) FreeLastUsedStackSlot();
        var regToTest = op == OperatorType.Division ? Reg64.RAX : Reg64.RDX;
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

    private RM HandleOperator(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult)
    {
        var op = binaryExpression.Operator;
        RM rmReturn = leftReg;
        var rmRight = Compile(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRM(rmRight,
                (mem) =>
                {
                    if (mem.Register != Reg64.RBP)
                    {
                        // If register does not persist accross calls
                        _asm.Mov(rightReg, mem);
                        _asm.Mov(interm, rightReg);
                        rmRight = interm;
                    }

                },
                (reg) => { _asm.Mov(interm, reg); rmRight = interm; },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }

        var rmLeft = Compile(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a register
            HandleRM(rmLeft,
                (mem) => _asm.Mov(leftReg, mem),
                (reg) => _asm.MovIfNeeded(leftReg, reg),
                (Imm32 imm) => _asm.Mov(leftReg, imm));

            OperatorFactory.Handle<Reg64, Imm32>(
                leftReg, rmRight,
                (reg, mem) => _asm.Sub(reg, mem),
                (dest, src) => _asm.Sub(dest, (RM64)src),
                (reg, imm) => _asm.Sub(reg, imm.Value));

           
        }
        else if (op == OperatorType.Addition)
        {
            OperatorFactory.HandleCommutative<Reg64, Imm32>(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },
                (reg, mem) => _asm.Add(reg, mem),
                (dest, src) => _asm.Add(dest, (RM64)src),
                (reg, imm) => _asm.Add(reg, imm.Value),
                (imm, reg) => { _asm.Add(reg, imm.Value); rmReturn = reg; },
                (mem, reg) => { _asm.Add(reg, mem); rmReturn = reg; });

        }
        else if (op == OperatorType.Multiplication)
        {
            OperatorFactory.HandleCommutative<Reg64, Imm32>(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },
                (reg, mem) => _asm.Imul(reg, mem),
                (dest, src) => _asm.Imul(dest, src),
                (reg, imm) => _asm.Imul(reg, reg, imm.Value),
                (imm, reg) => { _asm.Imul(reg, reg, imm.Value); rmReturn = reg; },
                (mem, reg) => { _asm.Imul(reg, mem); rmReturn = reg; });
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
            rmReturn = HandleConditional(leftReg, rmLeft, rmRight, ConditionCodes.GE, "CNE", "CNE_END");
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        return rmReturn;
    }

    private void HandleOperator(BinaryExpression binaryExpression, Reg64 leftReg, Reg64 rightReg, Xmm128 leftXmm, Xmm128 rightXmm, bool spillRightResult, Action thenBlock, Action elseBlock)
    {
        var op = binaryExpression.Operator;
        Reg64? rmReturn = leftReg;
        var rmRight = Compile(binaryExpression.Right, rightReg, rightXmm);

        if (spillRightResult)
        {
            var interm = NextAvailableLocalStackSlot();
            HandleRM(rmRight,
                (mem) =>
                {
                    if (mem.Register != Reg64.RBP)
                    {
                        // If register does not persist accross calls
                        _asm.Mov(rightReg, mem);
                        _asm.Mov(interm, rightReg);
                        rmRight = interm;
                    }

                },
                (reg) => { _asm.Mov(interm, reg); rmRight = interm; },
                (Imm32 imm) => {
                    // Pass since immediates persist always
                });
        }

        var rmLeft = Compile(binaryExpression.Left, leftReg, leftXmm);

        if (op == OperatorType.Subtraction)
        {
            // Ensure lhs is in a register
            HandleRM(rmLeft,
                (mem) => _asm.Mov(leftReg, mem),
                (reg) => _asm.MovIfNeeded(leftReg, reg),
                (Imm32 imm) => _asm.Mov(leftReg, imm));

            OperatorFactory.Handle<Reg64, Imm32>(
                leftReg, rmRight,
                (reg, mem) => _asm.Sub(reg, mem),
                (dest, src) => _asm.Sub(dest, (RM64)src),
                (reg, imm) => _asm.Sub(reg, imm.Value));


        }
        else if (op == OperatorType.Addition)
        {
            OperatorFactory.HandleCommutative<Reg64, Imm32>(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },
                (reg, mem) => _asm.Add(reg, mem),
                (dest, src) => _asm.Add(dest, (RM64)src),
                (reg, imm) => _asm.Add(reg, imm.Value),
                (imm, reg) => { _asm.Add(reg, imm.Value); rmReturn = reg; },
                (mem, reg) => { _asm.Add(reg, mem); rmReturn = reg; });

        }
        else if (op == OperatorType.Multiplication)
        {
            OperatorFactory.HandleCommutative<Reg64, Imm32>(rmLeft, rmRight,
                (mem) => { _asm.Mov(leftReg, mem); return leftReg; },
                (imm) => { _asm.Mov(leftReg, imm); return leftReg; },
                (reg, mem) => _asm.Imul(reg, mem),
                (dest, src) => _asm.Imul(dest, src),
                (reg, imm) => _asm.Imul(reg, reg, imm.Value),
                (imm, reg) => { _asm.Imul(reg, reg, imm.Value); rmReturn = reg; },
                (mem, reg) => { _asm.Imul(reg, mem); rmReturn = reg; });
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
            rmReturn = HandleConditionalBranch(leftReg, rmLeft, rmRight, ConditionCodes.GE, "CNE", "CNE_END", thenBlock, elseBlock);
        else throw new InvalidOperationException($"operator type '{op}' is not supported");

        if (spillRightResult) FreeLastUsedStackSlot();
        if(rmReturn != null)
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

    private Reg64 HandleConditional(Reg64 leftReg, RM rmLeft, RM rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle<Reg64, Imm32>(rmLeft, rmRight,
                    (mem) =>
                    {
                        _asm.Mov(leftReg, mem);
                        return leftReg;
                    },
                    (reg) =>
                    {
                        if (leftReg != reg) _asm.Mov(leftReg, (RM64)reg);
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


    private Reg64? HandleConditionalBranch(Reg64 leftReg, RM rmLeft, RM rmRight, byte conditionCode, string labelPrefix, string endLabelPrefix, Action ifBlock, Action elseBlock)
    {
        // Note: condition code must be inverted for this logic to work
        // IE if test is less than, conditon code should be GE (greater equal)
        OperatorFactory.Handle<Reg64, Imm32>(rmLeft, rmRight,
                    (mem) =>
                    {
                        _asm.Mov(leftReg, mem);
                        return leftReg;
                    },
                    (reg) =>
                    {
                        if (leftReg != reg) _asm.Mov(leftReg, (RM64)reg);
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


    #endregion

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
            var rmArg = Compile(argument, Reg64.RAX, Xmm128.XMM0);
            HandleRMForPush(rmArg,
                (mem) => _asm.Push((RM64)mem),
                (reg) => _asm.Push(reg),
                (xmm) => {
                    _asm.Sub(Reg64.RSP, 8);
                    _asm.Movq(Mem64.Create(Reg64.RSP), Xmm128.XMM0);
                },
                (imm) => _asm.Push(imm.Value));        
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
            var indirect = Compile(callExpression.Callee, desiredReg, desiredXmm);
            if (indirect is RM64 rm)
            {
                _asm.Call(rm);
            }
            else throw new InvalidOperationException($"unexpected rm type in call. rm type was '{indirect.GetType()}'");
        }
        _asm.Add(Reg64.RSP, (callExpression.Arguments.Count * 8) + additionalStackToSubtract); // clear stack of arguments
        if (callExpression.IsFloatingPointType())
        {
            _asm.MovIfNeeded(desiredXmm, Xmm128.XMM0);
            return desiredXmm;
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
            case 4: return Reg64.R9;
            default:
                throw new NotSupportedException("general register arguments not supported past parameter 4");
        }
    }

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

    #endregion

    #region Helpers

    private void HandleRMForPush(RM rm, Action<Mem64> handleMemory, Action<Reg64> handleRegister, Action<Xmm128> handleXmm, Action<Imm32> handleImm32)
    {
        if (rm is Reg64 reg)
            handleRegister(reg);
        else if (rm is Mem64 mem)
            handleMemory(mem);
        else if (rm is Xmm128 xmm)
            handleXmm(xmm);
        else if (rm is Imm32 imm32)
            handleImm32(imm32);
        else
            throw new InvalidOperationException($"unexpected RM type '{rm.GetType()}'");
    }

    private void HandleRM(RM rm, Action<Mem64> handleMemory, Action<Xmm128> handleXmm)
    {
        if (rm is Mem64 mem)
            handleMemory(mem);
        else if (rm is Xmm128 xmm)
            handleXmm(xmm);
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


    private void HandleRM(RM rm, Action<Mem64> handleMemory, Action<Reg64> handleRegister, Action<Imm32> handleImm32)
    {
        if (rm is Reg64 reg)
            handleRegister(reg);
        else if (rm is Mem64 mem)
            handleMemory(mem);
        else if (rm is Imm32 imm32)
            handleImm32(imm32);
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





public static class OperatorFactory
{

    public static void Handle<TReg, TImm>(TReg left, RM right, Action<TReg, Mem64> memToReg, Action<TReg, TReg> regToReg, Action<TReg, TImm> immToReg) where TReg : Register where TImm : Immediate
    {
        if (right is Mem64 mem) memToReg(left, mem);
        else if (right is TReg reg) regToReg(left, reg);
        else if (right is TImm immNN) immToReg(left, immNN);
        else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
    }

    public static void Handle<TReg, TImm>(RM rmLeft, RM right, Func<Mem64, TReg> moveLeftToReg, Func<TReg, TReg> moveRegLeftToReg, Func<TImm, TReg> moveImmLeftToReg, Action<TReg, Mem64> memToReg, Action<TReg, TReg> regToReg, Action<TReg, TImm> immToReg) where TReg : Register where TImm : Immediate
    {
        TReg left;
        if (rmLeft is Mem64 leftMem)
            left = moveLeftToReg(leftMem);
        else if (rmLeft is TReg leftAsReg) left = moveRegLeftToReg(leftAsReg);
        else if (rmLeft is TImm imm) left = moveImmLeftToReg(imm);
        else throw new InvalidOperationException($"unexpected left RM type '{rmLeft.GetType()}'");
        if (right is Mem64 mem) memToReg(left, mem);
        else if (right is TReg reg) regToReg(left, reg);
        else if (right is TImm immNN) immToReg(left, immNN);
        else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
    }




    public static void HandleCommutative<TReg, TImm>(RM rmLeft, RM right, 
        Func<Mem64, TReg> moveLeftToReg,
        Func<TImm, TReg> moveImmLeftToReg, 
        Action<TReg, Mem64> memToReg, 
        Action<TReg, TReg> regToReg, 
        Action<TReg, TImm> immToReg,
        Action<TImm, TReg> regToImm,
        Action<Mem64, TReg> regToMem) where TReg : Register where TImm : Immediate
    {

        // First, if it is memory to memory, dump left hand side to a register
        if (rmLeft is Mem64 leftAsMem)
        {
            if (right is Mem64)
                rmLeft = moveLeftToReg(leftAsMem);
            else if (right is Imm32)
                rmLeft = moveLeftToReg(leftAsMem);
        }
        else if (rmLeft is TImm leftAsImm && right is Mem64)
            rmLeft = moveImmLeftToReg(leftAsImm);
        if (rmLeft is TReg left)
        {
            if (right is Mem64 mem) memToReg(left, mem);
            else if (right is TReg reg) regToReg(left, reg);
            else if (right is TImm immNN) immToReg(left, immNN);
            else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
        }
        else if (rmLeft is Mem64 memLeft)
        {
            if (right is TReg reg) regToMem(memLeft, reg);
            else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
        }
        else if (rmLeft is TImm immLeft)
        {
            if (right is TReg reg) regToImm(immLeft, reg);
            else throw new InvalidOperationException($"unexpected right RM type '{right.GetType()}'");
        }
        else throw new InvalidOperationException($"unexpected left RM type '{rmLeft.GetType()}'");
    }

}