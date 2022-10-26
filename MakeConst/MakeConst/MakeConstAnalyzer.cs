using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;

namespace MakeConst
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MakeConstAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MakeConst";
        private const int _complexityOneSyntaxMaxCount = 10;
        private const int _complexityWarnigPercentCount = 21;
        private const int _complexityErrorPercentCount = 31;
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
            var complexityList = new List<SyntaxNode>();
            var onIncreaseComplexityActionList = new List<Action<DiagnosticDescriptor>>();
            var onIncreaseComplexityErrorActionList = new List<Action<DiagnosticDescriptor>>();
            AnalyzeCyclomaticComplexity(complexityList, onIncreaseComplexityActionList, onIncreaseComplexityErrorActionList, methodNode, true);

            var complexity = complexityList.Count;

            Console.WriteLine($"けっか : {complexity} >= {_complexityErrorPercentCount}");

            if (complexity >= _complexityWarnigPercentCount)
            {
                foreach (var action in onIncreaseComplexityActionList)
                {
                    action.Invoke(Rule);
                }
            }
            else if (complexity >= _complexityErrorPercentCount)
            {
                foreach (var action in onIncreaseComplexityActionList)
                {
                    action.Invoke(ErrorRule);
                }
            }
        }

        private void AnalyzeCyclomaticComplexity(List<SyntaxNode> complexityList, List<Action<DiagnosticDescriptor>> onIncreaseComplexityActionList, List<Action<DiagnosticDescriptor>> onIncreaseComplexityErrorActionList, SyntaxNode node, bool fromRootNode)
        {
            var beforeComplexity = complexityList.Count;
            AddDictionaryEachType(node, complexityList);
            var afterComplexity = complexityList.Count;

            if (beforeComplexity < afterComplexity)
            {
                onIncreaseComplexityActionList.Add((rule) =>
                {
                    _context.ReportDiagnostic(Diagnostic.Create(rule, node.GetLocation(), BuildReportText(node, beforeComplexity, afterComplexity)));
                });
            }

            var childNodes = node.ChildNodes();
            foreach (var childNode in childNodes)
            {
                var beforeCheckChildrenComplexity = complexityList.Count;
                AnalyzeCyclomaticComplexity(complexityList, onIncreaseComplexityActionList, onIncreaseComplexityErrorActionList, childNode, false);
                var afterCheckChildrenComplexity = complexityList.Count;

                if (afterCheckChildrenComplexity - beforeCheckChildrenComplexity >= _complexityOneSyntaxMaxCount && !fromRootNode)
                {
                    _context.ReportDiagnostic(Diagnostic.Create(ErrorRule, childNode.GetLocation(), BuildErrorReportText(childNode, beforeCheckChildrenComplexity, afterCheckChildrenComplexity)));
                }

            }            
        }

        private string BuildReportText(SyntaxNode node, int beforeComplexity, int afterComplexity)
        {
            var type = node.GetType();
            var className = type.Name;
            return $"\nまってや！！この関数ちょっと複雑ちゃうか！？！？\nこの部分だけ別の関数に切り出して処理を見やすくしてみいひんか？？\n{ beforeComplexity} -> { afterComplexity}\n{className}}}";
        }

        private string BuildErrorReportText(SyntaxNode node, int beforeComplexity, int afterComplexity)
        {
            var type = node.GetType();
            var className = type.Name;
            return $"\nちょっとまってや、、この部分だけで{_complexityOneSyntaxMaxCount}以上も複雑になってるで。{beforeComplexity} -> {afterComplexity}\nさすがに別の関数に分けた方がええんちゃうか...？\n{className}";
        }

        private void AddDictionaryEachType(SyntaxNode node, List<SyntaxNode> dictionary)
        {
            var isIncreased = CheckCyclomaticComplexitySyntax<WhileStatementSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<WhileStatementSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<SwitchStatementSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<CaseSwitchLabelSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<CasePatternSwitchLabelSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<ConditionalExpressionSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<ConditionalAccessExpressionSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<DoStatementSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<ForStatementSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<SwitchExpressionArmSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexitySyntax<IfStatementSyntax>(node) == 1;
            isIncreased |= CheckCyclomaticComplexityAssignmentSyntax(node) == 1;
            isIncreased |= CheckCyclomaticComplexityBinarySyntax(node) == 1;
            if (isIncreased)
            {
                dictionary.Add(node);
            }
        }


        private int CheckCyclomaticComplexitySyntax<T>(SyntaxNode node) where T : SyntaxNode
        {
            if (node is T)
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

    enum SyntaxNodeType
    {
        While = 1,
        Switch = 2,
        Case = 3,
        CasePattern = 4,
        ConsitionalExpression = 5,
        ConsitionalAccessExpression = 6,
        DoStatement = 7,
        ForStatement = 8,
        SwitchExpressionArm = 9,
        IfStatement = 10,
        SwitchLabel = 11,
    }
}
