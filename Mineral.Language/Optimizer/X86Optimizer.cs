using System.Runtime.InteropServices;
using X86_64Assembler;
using X86_64Assembler.Instructions;
using X86_64Assembler.Interfaces;

namespace Mineral.Language.Optimizer;

internal class X86Optimizer
{


    public void Optimize(X86AssemblyContext context)
    {
        
        int hardLimit = 100;
        foreach(var function in context.FunctionData)
        {
            int counter = 0;
            DisplayHeader(function, counter);
            Console.WriteLine("\n\n\n\n\n\t\t\t\t\tBEGIN");
            while (true)
            {
                Console.ReadKey(true);
                var reduced = Reduce(function, out var optimizedInstructions, out var removedIndices, out var changedIndices);
                DisplayAssembly(function, counter, function.Instructions, optimizedInstructions, changedIndices, removedIndices);
                function.Instructions = optimizedInstructions;
                if (reduced == false || counter >= hardLimit) 
                    break;
                counter++;
            }
            Console.ReadKey(true);
            DisplayHeader(function, counter);
            while (true)
            {
                Console.ReadKey(true);
                var reduced = Optimize(function, out var optimizedInstructions, out var removedIndices, out var changedIndices);
                DisplayAssembly(function, counter, function.Instructions, optimizedInstructions, changedIndices, removedIndices);
                function.Instructions = optimizedInstructions;

                if (reduced == false || counter >= hardLimit*2)
                    break;
                counter++;
            }
        }
    }

    private void DisplayHeader(X86Function function, int passNumber)
    {
        Console.Clear();
        Console.WriteLine(function.FunctionLabel + "(" + string.Join(", ", function.Parameters.Select(x => x.Alias + " " + x.StackSize.ToString())) + $") : PASS {passNumber}", ConsoleColor.Green, ConsoleColor.Black);
    }

    private void DisplayAssembly(X86Function function, int passNumber, List<X86Instruction> originalInstructions, List<X86Instruction> optimizedInstructions, List<int> changedIndices, List<int> removedIndices)
    {
        DisplayHeader(function, passNumber);
        int oI = 0;
        for(int i = 0; i < originalInstructions.Count; i++)
        {
            string optimizedInstructionString;
            var bgColor = ConsoleColor.Black;
            var fgColor = ConsoleColor.Gray;
            if (removedIndices.Contains(i))
            {
                bgColor = ConsoleColor.DarkRed;
                optimizedInstructionString = "".PadLeft(20, ' ');
            }
            else
            {

                optimizedInstructionString = new string(' ', 8) + optimizedInstructions[oI].ToAssemblyString();
                //optimizedInstructionString = optimizedInstructionString.PadRight(20);
                oI++;

            }
            if (changedIndices.Contains(i)) bgColor = ConsoleColor.DarkYellow;

            var instructionString = new string(' ', 8) + originalInstructions[i].ToAssemblyString();
            instructionString = instructionString.PadRight(60, ' ');

            var finalInstructionString = instructionString + optimizedInstructionString;
            Console.ForegroundColor = fgColor;
            Console.BackgroundColor = bgColor;
            Console.WriteLine(finalInstructionString);
            Console.ResetColor();
        }
    }

    private bool Optimize(X86Function function, out List<X86Instruction> optimizedInstructions, out List<int> removedIndices, out List<int> changedIndices)
    {
        _equivalencies.Clear();
        optimizedInstructions = function.Instructions.Select(x => x).ToList();
        removedIndices = new List<int>();
        changedIndices = new List<int>();
        var indicesToRemove = new List<int>();

        for (int i = 0; i < optimizedInstructions.Count; i++)
        {
            var instr = optimizedInstructions[i];


            if (instr is Mov_Reg64_RM64 mov && _equivalencies.TryGetValue(mov.Src, out var equivalentReg))
            {
                
                optimizedInstructions[i] = new Mov_Reg64_RM64(mov.Dest, equivalentReg);
                changedIndices.Add(i);
                break;
            }

            if (instr is Mov_RM64_Reg64 mov_rm && !IsReferenced(optimizedInstructions, i+ 1, null, mov_rm.Dest, new()))
            {
                // if RM is not referenced, no need to move to it
                indicesToRemove.Add(i);
                break;
            }


            // Fallthrough
            Track(instr);
        }
        foreach (var index in indicesToRemove) optimizedInstructions.RemoveAt(index);

        return indicesToRemove.Any() || changedIndices.Any();
    }


    private bool Reduce(X86Function function, out List<X86Instruction> optimizedInstructions, out List<int> removedIndices, out List<int> changedIndices)
    {
        _equivalencies.Clear();
        optimizedInstructions = function.Instructions.Select(x => x).ToList();
        removedIndices = new List<int>();
        changedIndices = new List<int>();
        var indicesToRemove = new List<int>();

        for (int i = 0; i < optimizedInstructions.Count; i++)
        {
            var instr = optimizedInstructions[i];

            if (instr is Add_RM64_Int32 add && add.Dest is Reg64 reg && TryFindNextLiteralAddSub(function.Instructions, i + 1, null, reg, new(), out var indexToRemove, out var offset))
            {
                removedIndices.Add(indexToRemove);
                indicesToRemove.Add(indexToRemove);
                var totalOffset = add.Imm32 + offset;

                if (totalOffset > 0)
                {
                    optimizedInstructions[i] = new Add_RM64_Int32(reg, totalOffset);
                    changedIndices.Add(i);
                }
                else if (totalOffset < 0)
                {
                    optimizedInstructions[i] = new Sub_RM64_Int32(reg, -1 * totalOffset);
                    changedIndices.Add(i);
                }
                else
                {
                    removedIndices.Add(i); // unadjusted since it will reflect the original index

                    if (indexToRemove > i) indicesToRemove.Add(i);
                    else if (indexToRemove < i) indicesToRemove.Add(i - 1);
                    // if they are the same, just remove once
                }
                break;
            }

            if (instr is Sub_RM64_Int32 sub && sub.Dest is Reg64 subReg && TryFindNextLiteralAddSub(function.Instructions, i + 1, null, subReg, new(), out var subIndexToRemove, out var subOffset))
            {
                indicesToRemove.Add(subIndexToRemove);
                removedIndices.Add(subIndexToRemove);

                var totalOffset = -sub.Imm32 + subOffset;

                if (totalOffset > 0)
                {
                    optimizedInstructions[i] = new Add_RM64_Int32(subReg, totalOffset);
                    changedIndices.Add(i);
                }
                else if (totalOffset < 0)
                {
                    optimizedInstructions[i] = new Sub_RM64_Int32(subReg, -1 * totalOffset);
                    changedIndices.Add(i);
                }
                else
                {
                    removedIndices.Add(i); // unadjusted since it will reflect the original index

                    if (subIndexToRemove > i) indicesToRemove.Add(i);
                    else if (subIndexToRemove < i) indicesToRemove.Add(i - 1);
                    // if they are the same, just remove once
                }
                break;
            }

        }

        foreach (var index in indicesToRemove) optimizedInstructions.RemoveAt(index);


        return indicesToRemove.Any();
    }


    private Dictionary<RM64, Reg64> _equivalencies = new();

    private X86Instruction Track(X86Instruction ins)
    {
        if (ins is Mov_RM64_Reg64 tIns) _equivalencies[tIns.Dest] = tIns.Src;
        else if (ins is Mov_Reg64_RM64 tiIns)
        {
            // remove stale equivalencies
            WipeIfNeeded(tiIns.Dest);
            _equivalencies[tiIns.Src] = tiIns.Dest;
        }
        else if (ins is RM64_Dest rmDest)
        {
            // remove stale equivalencies
            WipeIfNeeded(rmDest.Dest);
        }
        else if (ins is Reg64_Dest regDest)
        {
            WipeIfNeeded(regDest.Dest);
        }
        else if (ins is Call_Int32 || ins is Call_Rel32 || ins is Call_RM32 || ins is Call_RM64) _equivalencies.Clear();
        else if (ins is Ret || ins is Ret_UInt16) _equivalencies.Clear();
        else if (ins is Label) _equivalencies.Clear();
        return ins;
    }

    private void WipeIfNeeded(RM64 rm)
    {
        if (rm is Reg64 reg)
        {
            var foundKeys = _equivalencies.Keys.Where(x => x is MemRegDisp rDisp && rDisp.Register.Equals(reg)).ToList();
            foundKeys.AddRange(_equivalencies.Keys.Where(x => _equivalencies[x].Equals(reg)));
            foreach (var key in foundKeys) _equivalencies.Remove(key);
        }
        else 
        {
            var foundKeys = _equivalencies.Keys.Where(x => _equivalencies[x].Equals(rm));
            foreach (var key in foundKeys) _equivalencies.Remove(key);
        }
        
    }


    private bool IsReferenced(List<X86Instruction> instructions, int startIndex, int? endIndex, RM64 rm, HashSet<string> exploredLabels)
    {
        if (rm is MemRegDisp memDisp && memDisp.Register != Reg64.RBP) return true;
        if (endIndex == null) endIndex = instructions.Count;
        if (startIndex == endIndex) 
            return false;
        if (startIndex >= instructions.Count) 
            throw new InvalidOperationException();
        var ins = instructions[startIndex];
        if (ins is Mov_RM64_Imm32 movImm && movImm.Dest.Equals(rm)) 
            return false; // rm is overwritten before reference
        if (ins is Mov_RM64_Reg64 movReg && movReg.Dest.Equals(rm)) 
            return false; //rm is overwritten before reference
        if (ins is RM64_Dest rmDest && rmDest.Dest.Equals(rm)) return true;
        if (ins is RM64_Src rmSrc && rmSrc.Src.Equals(rm)) return true;
        if (ins is RM64_Target rmTarget && rmTarget.Target.Equals(rm)) return true;
        if (ins is Jmp_Rel32 jmp)
        {
            var label = jmp.Rel.Symbol;
            var labelIndex = instructions.FindIndex(x => x is Label l && l.Name == label);
            if (labelIndex == -1) return true; // cannot find label so err on side of caution and assume it is referened
            int? branchEndIndex;
            if (labelIndex > startIndex) branchEndIndex = null;
            else branchEndIndex = startIndex;
            exploredLabels.Add(label);
            var isReferencedInBranch = IsReferenced(instructions, labelIndex, branchEndIndex, rm, exploredLabels);
            return isReferencedInBranch; // since it is a definitive jump
        }
        if (ins is Jcc_Rel32_Byte jcc)
        {
            var label = jcc.Rel.Symbol;
            var labelIndex = instructions.FindIndex(x => x is Label l && l.Name == label);
            if (labelIndex == -1) return true; // cannot find label so err on side of caution and assume it is referened
            int? branchEndIndex;
            if (labelIndex > startIndex) branchEndIndex = null;
            else branchEndIndex = startIndex;
            exploredLabels.Add(label);
            var isReferencedInBranch = IsReferenced(instructions, labelIndex, branchEndIndex, rm, exploredLabels);
            if (isReferencedInBranch) return true;
        }
        return IsReferenced(instructions, startIndex+ 1, endIndex, rm, exploredLabels);
    }

    private bool TryFindNextLiteralAddSub(List<X86Instruction> instructions, int startIndex, int? endIndex, Reg64 reg, HashSet<string> exploredLabels, out int indexToRemove, out int offset)
    {
        indexToRemove = -1;
        offset = 0;
        if (endIndex == null) endIndex = instructions.Count;
        if (startIndex == endIndex)
            return false;
        if (startIndex >= instructions.Count) throw new InvalidOperationException();
        var ins = instructions[startIndex];
        if (ins is Add_RM64_Int32 add && add.Dest.Equals(reg))
        {
            indexToRemove = startIndex;
            offset = add.Imm32;
            return true;
        }
        if (ins is Sub_RM64_Int32 sub && sub.Dest.Equals(reg))
        {
            indexToRemove = startIndex;
            offset = -sub.Imm32;
            return true;
        }
        if (ins is Label) return false; // since we cannot confirm this is the only code path
        if (ins is RM64_Dest rmDest && rmDest.Dest.Equals(reg)) return false;
        if (ins is RM64_Src rmSrc && rmSrc.Src.Equals(reg)) return false;
        if (ins is RM64_Target rmTarget && rmTarget.Target.Equals(reg)) return false;
        if (ins is Reg64_Dest regDest && regDest.Dest.Equals(reg)) return false;
        if (ins is Reg64_Src regSrc && regSrc.Src.Equals(reg)) return false;
        if (ins is Call_Int32 || ins is Call_Rel32 || ins is Call_RM32 || ins is Call_RM64) return false;
        if (ins is Jmp_Rel32 jmp)
        {
            var label = jmp.Rel.Symbol;
            var labelIndex = instructions.FindIndex(x => x is Label l && l.Name == label);
            if (labelIndex == -1) return true; // cannot find label so err on side of caution and assume it is referened
            int? branchEndIndex;
            if (labelIndex > startIndex) branchEndIndex = null;
            else branchEndIndex = startIndex;
            exploredLabels.Add(label);
            return TryFindNextLiteralAddSub(instructions, startIndex + 1, branchEndIndex, reg, exploredLabels, out indexToRemove, out offset);
        }
        if (ins is Jcc_Rel32_Byte jcc)
        {
            return false;
        }
        return TryFindNextLiteralAddSub(instructions, startIndex + 1, endIndex, reg, exploredLabels, out indexToRemove, out offset);
    }

}
