using System;
using Tokenizer.Core.Models;

namespace Mineral.Language.Statements;

public class StatementBase
{
    public Location Start { get; set; } = Location.Zero;
    public Location End { get; set; } = Location.Zero;
}