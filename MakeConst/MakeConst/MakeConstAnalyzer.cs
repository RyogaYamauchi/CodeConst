﻿using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

namespace MakeConst
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MakeConstAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MakeConst";
        private List<string> _variableList = new List<string>();
        private SyntaxNodeAnalysisContext _context;

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);
        private static readonly DiagnosticDescriptor ErrorRule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            // context.RegisterSyntaxNodeAction(AnalyzeLocalVariableNode, SyntaxKind.LocalDeclarationStatement);
            context.RegisterSyntaxNodeAction(AnalyzeLocalDecriation, SyntaxKind.MethodDeclaration);
        }
        private void AnalyzeLocalDecriation(SyntaxNodeAnalysisContext context)
        {
            _context = context;
            var methodNode = (MethodDeclarationSyntax)context.Node;
            var complexity = 1;
            complexity = AnalyzeCyclomaticComplexity(complexity, methodNode);
            var str = "\n---start\n";
            str += $"循環性参照度 : {complexity}\n";
            str += "\n---end\n";
            if (complexity > 10)
            {
                context.ReportDiagnostic(Diagnostic.Create(Rule, context.Node.GetLocation(), str));
            }
            else if(complexity > 20)
            {
                context.ReportDiagnostic(Diagnostic.Create(ErrorRule, context.Node.GetLocation(), str));
            }

        }

       private int AnalyzeCyclomaticComplexity(int complexity, SyntaxNode node)
        {
            complexity += CheckCyclomaticComplexitySyntax<WhileStatementSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<SwitchStatementSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<CaseSwitchLabelSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<CasePatternSwitchLabelSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<ConditionalExpressionSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<ConditionalAccessExpressionSyntax>(node);
            complexity += CheckCyclomaticComplexityAssignmentSyntax(node);
            complexity += CheckCyclomaticComplexityBinarySyntax(node);
            complexity += CheckCyclomaticComplexitySyntax<DoStatementSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<ForStatementSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<SwitchExpressionArmSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<SwitchExpressionArmSyntax>(node);
            complexity += CheckCyclomaticComplexitySyntax<IfStatementSyntax>(node);

            var childNodes = node.ChildNodes();
            foreach (var childNode in childNodes)
            {
                complexity += AnalyzeCyclomaticComplexity(0, childNode);
            }
            return complexity;
        }

        private int CheckCyclomaticComplexitySyntax<T>(SyntaxNode node) where T : SyntaxNode
        {
            if(node is T)
            {
                return 1;
            }
            return 0;
        }

        private int CheckCyclomaticComplexityBinarySyntax(SyntaxNode node)
        {
            if (node.IsKind(SyntaxKind.CoalesceExpression) ||
            node.IsKind(SyntaxKind.LogicalAndExpression) ||
            node.IsKind(SyntaxKind.LogicalOrExpression))
            {
                return 1;
            }

            return 0;
        }

        private int CheckCyclomaticComplexityAssignmentSyntax(SyntaxNode node)
        {
            var kind = node.Kind();
            if (kind == SyntaxKind.CoalesceAssignmentExpression)
            {
                return 1;
            }
            else if (kind is SyntaxKind.AmpersandToken || kind is SyntaxKind.BarBarToken)
            {
                return 1;
            }

            return 0;
        }


        // ローカルで変数定義の時に呼び出される
        private void AnalyzeLocalVariableNode(SyntaxNodeAnalysisContext context)
        {
            // constがついていないものに対して解析をかける
            var localDeclaration = (LocalDeclarationStatementSyntax)context.Node;
            if (localDeclaration.Modifiers.Any(SyntaxKind.ConstKeyword))
            {
                return;
            }

            DataFlowAnalysis dataFlowAnalysis = context.SemanticModel.AnalyzeDataFlow(localDeclaration);

            TypeSyntax variableTypeName = localDeclaration.Declaration.Type;
            ITypeSymbol variableType = context.SemanticModel.GetTypeInfo(variableTypeName, context.CancellationToken).ConvertedType;

            foreach (VariableDeclaratorSyntax variable in localDeclaration.Declaration.Variables)
            {
                EqualsValueClauseSyntax initializer = variable.Initializer;
                if (initializer == null)
                {
                    return;
                }

                Optional<object> constantValue = context.SemanticModel.GetConstantValue(initializer.Value, context.CancellationToken);
                if (!constantValue.HasValue)
                {
                    return;
                }

                if (constantValue.Value is string)
                {
                    if (variableType.SpecialType != SpecialType.System_String)
                    {
                        return;
                    }
                }
                else if (variableType.IsReferenceType && constantValue.Value != null)
                {
                    return;
                }

                Conversion conversion = context.SemanticModel.ClassifyConversion(initializer.Value, variableType);
                if (!conversion.Exists || conversion.IsUserDefined)
                {
                    return;
                }
            }

            foreach (VariableDeclaratorSyntax variable in localDeclaration.Declaration.Variables)
            {
                // Retrieve the local symbol for each variable in the local declaration
                // and ensure that it is not written outside of the data flow analysis region.
                ISymbol variableSymbol = context.SemanticModel.GetDeclaredSymbol(variable, context.CancellationToken);
                if (dataFlowAnalysis.WrittenOutside.Contains(variableSymbol))
                {
                    return;
                }
            }
            // レポートをコンテキストに返す
            context.ReportDiagnostic(Diagnostic.Create(Rule, context.Node.GetLocation(), localDeclaration.Declaration.Variables.First().Identifier.ValueText));

        }

        private static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

            // Find just those named type symbols with names containing lowercase letters.
            if (namedTypeSymbol.Name.ToCharArray().Any(char.IsLower))
            {
                // For all such symbols, produce a diagnostic.
                var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);

                context.ReportDiagnostic(diagnostic);
            }
        }
    }
}
