namespace DryIoc.CompileTimeContainer.SourceGenerator;

using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using DryIoc;

[Generator]
public class CompileTimeContainerGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var classDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsCandidateClass(s),
                transform: static (ctx, _) => GetSemanticTargetForGeneration(ctx))
            .Where(static m => m is not null);

        var compilationAndClasses = context.CompilationProvider.Combine(classDeclarations.Collect());

        context.RegisterSourceOutput(compilationAndClasses,
            static (spc, source) => Execute(source.Left, source.Right, spc));
    }

    private static bool IsCandidateClass(SyntaxNode node)
    {
        return node is ClassDeclarationSyntax classDeclaration &&
            classDeclaration.AttributeLists
                .SelectMany(al => al.Attributes)
                .Any(attr => attr.Name.ToString() == nameof(CompileTimeContainerAttribute));
    }

    record ClassInfo(
        string Namespace,
        string OriginalClassName,
        string GeneratedClassName,
        string[] NamespaceUsings,
        bool AllowRuntimeState,
        string RootType,
        int RegisterAttributeCount);

    private static ClassInfo GetSemanticTargetForGeneration(GeneratorSyntaxContext context)
    {
        var classDeclaration = (ClassDeclarationSyntax)context.Node;
        var semanticModel = context.SemanticModel;
        var classSymbol = semanticModel.GetDeclaredSymbol(classDeclaration);

        if (classSymbol == null)
            return null;

        // Find CompileTimeContainerAttribute
        var compileTimeAttr = classSymbol.GetAttributes()
            .FirstOrDefault(attr =>
                attr.AttributeClass?.Name == nameof(CompileTimeContainerAttribute) ||
                attr.AttributeClass?.Name.EndsWith(nameof(CompileTimeContainerAttribute)) == true);

        if (compileTimeAttr == null)
            return null;

        // Find RegisterAttributes
        var registerAttrs = classSymbol.GetAttributes()
            .Where(attr => IsRegisterAttribute(attr.AttributeClass))
            .ToArray();

        if (registerAttrs.Length == 0)
            return null;

        // Parse CompileTimeContainerAttribute properties
        var className = GetAttributeStringProperty(compileTimeAttr, "ClassName") ?? "CompileTimeContainer";
        var namespaceUsings = GetAttributeStringArrayProperty(compileTimeAttr, "NamespaceUsings") ?? new string[0];
        var allowRuntimeState = GetAttributeBoolProperty(compileTimeAttr, "AllowRuntimeState") ?? false;

        // Check if it's generic CompileTimeContainerAttribute<T>
        var rootType = compileTimeAttr.AttributeClass?.IsGenericType == true
            ? compileTimeAttr.AttributeClass.TypeArguments.FirstOrDefault()?.ToDisplayString()
            : null;

        var namespaceName = classSymbol.ContainingNamespace.IsGlobalNamespace
            ? "Global"
            : classSymbol.ContainingNamespace.ToDisplayString();

        return new ClassInfo(
            namespaceName,
            classSymbol.Name,
            className,
            namespaceUsings,
            allowRuntimeState,
            rootType,
            registerAttrs.Length);
    }

    static bool IsRegisterAttribute(INamedTypeSymbol attributeClass)
    {
        if (attributeClass == null)
            return false;

        var name = attributeClass.Name;
        return name == nameof(RegisterAttribute) ||
               name.StartsWith("RegisterAttribute") ||
               (attributeClass.BaseType != null && IsRegisterAttribute(attributeClass.BaseType));
    }

    static string GetAttributeStringProperty(AttributeData attribute, string propertyName)
    {
        return attribute.NamedArguments
            .FirstOrDefault(kvp => kvp.Key == propertyName)
            .Value.Value as string;
    }

    static string[] GetAttributeStringArrayProperty(AttributeData attribute, string propertyName)
    {
        var namedArg = attribute.NamedArguments
            .FirstOrDefault(kvp => kvp.Key == propertyName);

        if (namedArg.Value.IsNull || namedArg.Value.Values.IsEmpty)
            return null;

        return namedArg.Value.Values
            .Select(v => v.Value?.ToString())
            .Where(s => s != null)
            .ToArray();
    }

    static bool? GetAttributeBoolProperty(AttributeData attribute, string propertyName)
    {
        return attribute.NamedArguments
            .FirstOrDefault(kvp => kvp.Key == propertyName)
            .Value.Value as bool?;
    }

    static void Execute(Compilation compilation, ImmutableArray<ClassInfo> classes, SourceProductionContext context)
    {
        if (classes.IsDefaultOrEmpty)
            return;

        foreach (var classInfo in classes)
        {
            if (classInfo == null)
                continue;

            var source = "// <auto-generated/>";
            var fileName = $"{classInfo.OriginalClassName}.{classInfo.GeneratedClassName}.g.cs";

            context.AddSource(fileName, source);
        }
    }
}
