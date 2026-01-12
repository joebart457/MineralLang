
namespace Mineral.Language.Statements;

public class ContinueStatement: StatementBase
{
    public WhileStatement? ParentLoop { get; set; }
}
