using Mineral.Language.Compiler;
using Mineral.Language.Expressions;
using Mineral.Language.LValues;
using Mineral.Language.Statements;
using Mineral.Language.StaticAnalysis;


namespace Mineral.Language.Reducer;

internal static class ASTReducer
{
    public static void Reduce(ProgramContext program)
    {
        foreach(var module in program.Modules.Values)
        {
            foreach (var methodGroup in module.Methods.Values)
                foreach(var function in methodGroup.Overloads.Values)
                    Reduce(function);
        }
    }

    private static void Reduce(FunctionContext functionContext)
    {
        if (functionContext.IsImported) return;
        var functionBody = new List<StatementBase>();
        foreach (var statment in functionContext.BodyStatements)
        {
            var stmt = Reduce(statment);
            if (stmt != null) functionBody.Add(stmt);
        }
        functionContext.BodyStatements = functionBody;
    }

    private static StatementBase? Reduce(StatementBase statement)
    {
        switch (statement)
        {
            case ReturnStatement returnStatement:
                return Reduce(returnStatement);
            case ErrorStatement errorStatement:
                return Reduce(errorStatement);
            case ConditionalStatement conditionalStatement:
                return Reduce(conditionalStatement);
            case WhileStatement whileStatement:
                return Reduce(whileStatement);
            case AssignmentStatement assignmentStatement:
                return Reduce(assignmentStatement);
            case DereferenceAssignmentStatement dereferenceAssignmentStatement:
                return Reduce(dereferenceAssignmentStatement);
            case ContinueStatement continueStatement:
                return Reduce(continueStatement);
            case BreakStatement breakStatement:
                return Reduce(breakStatement);
            default:
                return statement;
        }
    }


    private static StatementBase? Reduce(AssignmentStatement assignmentStatement)
    {
        if (assignmentStatement.Value is StackAllocateExpression) return null;
        assignmentStatement.Value = Reduce(assignmentStatement.Value);
        return assignmentStatement;
    }

    private static StatementBase? Reduce(DereferenceAssignmentStatement dereferenceAssignmentStatement)
    {
        dereferenceAssignmentStatement.Value = Reduce(dereferenceAssignmentStatement.Value);
        return dereferenceAssignmentStatement;
    }

    private static StatementBase? Reduce(ContinueStatement continueStatement)
    {
        return continueStatement;
    }

    private static StatementBase? Reduce(BreakStatement breakStatement)
    {
        return breakStatement;
    }

    private static StatementBase? Reduce(ErrorStatement errorStatement)
    {
        errorStatement.ErrorToReturn = Reduce(errorStatement.ErrorToReturn);
        return errorStatement;
    }

    private static StatementBase? Reduce(ReturnStatement returnStatement)
    {
        returnStatement.ValueToReturn = Reduce(returnStatement.ValueToReturn);
        return returnStatement;
    }

    private static StatementBase? Reduce(ConditionalStatement ifStatement)
    {
        ifStatement.ConditionalTarget = Reduce(ifStatement.ConditionalTarget);
        var thenBlock = new List<StatementBase>();
        foreach (var statment in ifStatement.ThenBlock)
        {
            var stmt = Reduce(statment);
            if (stmt != null) thenBlock.Add(stmt);
        }
        ifStatement.ThenBlock = thenBlock;

        if (ifStatement.ElseBlock != null)
        {
            var elseBlock = new List<StatementBase>();
            foreach (var statment in ifStatement.ElseBlock)
            {
                var stmt = Reduce(statment);
                if (stmt != null) elseBlock.Add(stmt);
            }
            ifStatement.ElseBlock = elseBlock;
        }

        return ifStatement;
    }

    private static StatementBase? Reduce(WhileStatement whileStatement)
    {
        whileStatement.ConditionalTarget = Reduce(whileStatement.ConditionalTarget);
        var thenBlock = new List<StatementBase>();
        foreach(var statment in whileStatement.ThenBlock)
        {
            var stmt = Reduce(statment);
            if (stmt != null) thenBlock.Add(stmt);
        }
        whileStatement.ThenBlock = thenBlock;
        return whileStatement;
    }


    private static OperableExpresson Reduce(CastExpression castExpression)
    {
        if (castExpression.Value is LiteralExpression literalExpression)
        {
            castExpression.Value = ReduceCast(castExpression, literalExpression);
            castExpression.Value.TagAsType(castExpression.ConcreteType);
            return castExpression;
        }
        castExpression.Value = Reduce(castExpression.Value);
        return castExpression;
    }

    private static LiteralExpression ReduceCast(CastExpression castExpression, LiteralExpression literalExpression)
    {
        if (castExpression.IsFloat32() && literalExpression.Value is double dbl)
        {
            return new LiteralExpression((float)dbl);
        }
        if (castExpression.IsFloat32() && literalExpression.Value is int i)
        {
            return new LiteralExpression((float)i);
        }

        if (castExpression.IsFloat64() && literalExpression.Value is float flt)
        {
            return new LiteralExpression((double)flt);
        }
        if (castExpression.IsFloat64() && literalExpression.Value is int int2)
        {
            return new LiteralExpression((double)int2);
        }

        if (castExpression.IsInt64() && literalExpression.Value is float flt2)
        {
            return new LiteralExpression((int)flt2);
        }
        if (castExpression.IsInt64() && literalExpression.Value is double dbl2)
        {
            return new LiteralExpression((int)dbl2);
        }

        if (castExpression.IsInt64() && literalExpression.Value is short s)
        {
            return new LiteralExpression((int)s);
        }
        if (castExpression.IsInt64() && literalExpression.Value is byte b)
        {
            return new LiteralExpression((int)b);
        }

        if (castExpression.IsInt32() && literalExpression.Value is short st)
        {
            return new LiteralExpression((int)st);
        }

        if (castExpression.IsInt32() && literalExpression.Value is byte bt)
        {
            return new LiteralExpression((int)bt);
        }

        if (castExpression.IsInt16() && literalExpression.Value is long lt)
        {
            return new LiteralExpression((short)lt);
        }

        if (castExpression.IsInt16() && literalExpression.Value is byte bts)
        {
            return new LiteralExpression((short)bts);
        }

        if (castExpression.IsFloat32() && literalExpression.Value is int it)
        {
            return new LiteralExpression((float)it);
        }
        return literalExpression;
    }

    private static OperableExpresson Reduce(SizeOfExpression sizeOfExpression)
    {
        return new LiteralExpression(sizeOfExpression.ConcreteType.GetActualSize());
    }

    private static OperableExpresson Reduce(CallExpression callExpression)
    {
        var args = new List<ExpressionBase>();
        callExpression.Callee = Reduce(callExpression.Callee);
        foreach (var arg in callExpression.Arguments) args.Add(Reduce(arg));
        callExpression.Arguments = args;
        return callExpression;
    }

    private static OperableExpresson Reduce(DereferenceExpression dereferenceExpression)
    {
        dereferenceExpression.Target = Reduce(dereferenceExpression.Target);
        return dereferenceExpression;
    }

    private static OperableExpresson Reduce(BinaryExpression binaryExpression)
    {
        binaryExpression.Left = Reduce(binaryExpression.Left);
        binaryExpression.Right = Reduce(binaryExpression.Right);
        return binaryExpression;
    }

    private static OperableExpresson Reduce(MemberAccessExpression memberAccessExpression)
    {
        memberAccessExpression.Instance = Reduce(memberAccessExpression.Instance);
        return memberAccessExpression;
    }

    public static ExpressionBase Reduce(ExpressionBase expression)
    {
        switch (expression)
        {
            case CastExpression castExpression:
                return Reduce(castExpression);
            case SizeOfExpression sizeOfExpression:
                return Reduce(sizeOfExpression);
            case CallExpression callExpression: 
                return Reduce(callExpression);
            case DereferenceExpression dereferenceExpression:
                return Reduce(dereferenceExpression);
            case BinaryExpression binaryExpression: 
                return Reduce(binaryExpression);
            case MemberAccessExpression memberAccessExpression:
                return Reduce(memberAccessExpression);
            default:
                return expression;
        }
    }

    public static OperableExpresson Reduce(OperableExpresson expression)
    {
        switch (expression)
        {
            case CastExpression castExpression:
                return Reduce(castExpression);
            case SizeOfExpression sizeOfExpression:
                return Reduce(sizeOfExpression);
            case CallExpression callExpression:
                return Reduce(callExpression);
            case DereferenceExpression dereferenceExpression:
                return Reduce(dereferenceExpression);
            case BinaryExpression binaryExpression:
                return Reduce(binaryExpression);
            case MemberAccessExpression memberAccessExpression:
                return Reduce(memberAccessExpression);
            default:
                return expression;
        }
    }

    public static ConditionalExpression Reduce(ConditionalExpression expression)
    {
        switch (expression)
        {
            case CastExpression castExpression:
                return Reduce(castExpression);
            case SizeOfExpression sizeOfExpression:
                return Reduce(sizeOfExpression);
            case CallExpression callExpression:
                return Reduce(callExpression);
            case DereferenceExpression dereferenceExpression:
                return Reduce(dereferenceExpression);
            case BinaryExpression binaryExpression:
                return Reduce(binaryExpression);
            case MemberAccessExpression memberAccessExpression:
                return Reduce(memberAccessExpression);
            default:
                return expression;
        }
    }
}
