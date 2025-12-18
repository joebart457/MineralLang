using Mineral.Language.Compiler;
using Mineral.Language.Parser;
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
            var results = parser.ParseWorkspace(directory);
            var parsingErrors = results.SelectMany(x => x.Errors);
            if (parsingErrors.Any())
            {
                foreach (var error in parsingErrors) Console.WriteLine(error);
                return -1;
            }
            foreach(var fileParseResult in results)
            {
                var (module, errors) = typeResolver.ProcessModule(programContext, fileParseResult.ModuleName!, fileParseResult.Imports!, fileParseResult.DeclaredTypes, fileParseResult.DeclaredFunctions);
                if (errors.Errors.Count != 0)
                {
                    foreach (var error in errors.Errors) Console.WriteLine(error);
                    return -2;
                }
            }

            var (success, compilationError) = compiler.CompileProgram("out.txt", programContext);
            
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
