using X86_64Assembler;

namespace Mineral.Language.Compiler;

internal static class X86AssemblyContextExtensions
{
    public static void MovIfNeeded(this X86AssemblyContext asm, Reg64 regDest, Reg64 regSrc)
    {
        if (regDest != regSrc)
            asm.Mov(regDest, (RM64)regSrc);
    }

    public static void MovIfNeeded(this X86AssemblyContext asm, Reg8 regDest, Reg8 regSrc)
    {
        if (regDest != regSrc)
            asm.Mov(regDest, (RM8)regSrc);
    }

    public static void MovIfNeeded(this X86AssemblyContext asm, Xmm128 regDest, Xmm128 regSrc)
    {
        if (regDest != regSrc)
            asm.Movaps(regDest, (RM128)regSrc);
    }
}
