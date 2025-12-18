using Mineral.Language.StaticAnalysis;
using Tokenizer.Core.Models;

namespace Mineral.Language.Declarations;

public class TypeDeclaration : DeclarationBase
{
    public TypeDeclaration(Token typeName, List<TypeDeclarationField> members)
    {
        TypeName = typeName;
        Members = members;
    }

    public Token TypeName { get; set; }
    public List<TypeDeclarationField> Members { get; set; }
}

public class TypeDeclarationField
{
    public TypeDeclarationField(Token fieldName, TypeSymbol fieldType)
    {
        FieldName = fieldName;
        FieldType = fieldType;
    }

    public Token FieldName { get; set; }
    public TypeSymbol FieldType { get; set; }
}
