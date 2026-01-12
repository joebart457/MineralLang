
namespace Mineral.Language.Statements;

public class BreakStatement: StatementBase
{
    public WhileStatement? ParentLoop { get; set; }
}
