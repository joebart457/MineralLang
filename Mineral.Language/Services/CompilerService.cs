using Mineral.Language.Compiler;
using Mineral.Language.Parser;
using Mineral.Language.Reducer;
using Mineral.Language.StaticAnalysis;
using System;
using System.Collections.Generic;
using System.Text;

namespace Mineral.Language.Services;

public static class CompilerService
{
    public static void CompileFile(string filePath)
    {

    }

    public static int CompileProgram(string directory)
    {
        var parser = new MineralParser();
        var typeResolver = new TypeResolver();
        var compiler = new MineralCompiler();
        var programContext = new ProgramContext();
        try
        {
            var parsedFiles = parser.ParseWorkspace(directory);
            var parsingErrors = parsedFiles.SelectMany(x => x.Errors);
            if (parsingErrors.Any())
            {
                foreach (var error in parsingErrors) Console.WriteLine(error);
                return -1;
            }
            var modules = typeResolver.ResolveWorkspace(programContext, parsedFiles);
            var typeErrors = modules.SelectMany(x => x.GetOrCreateModuleErrors().Errors);
            if (typeErrors.Any())
            {
                foreach (var error in typeErrors) Console.WriteLine(error);
                return -2;
            }

            ASTReducer.Reduce(programContext);
            var (success, compilationError) = compiler.CompileProgram("testOutput.exe", programContext);
            if (!success)
            {
                Console.WriteLine(compilationError);
                return -3;
            }
            
        } catch (Exception ex)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine(ex.ToString());
            Console.ResetColor();
            return -3;
        }
        return 0;
    }
}
