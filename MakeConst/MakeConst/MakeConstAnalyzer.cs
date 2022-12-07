using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization.Json;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading;
using System.Xml.Serialization;

namespace MakeConst
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MakeConstAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MakeConst";
        private const int _complexityOneSyntaxMaxCount = 10;
        private const int _complexityWarnigPercentCount = 21;
        private const int _complexityErrorPercentCount = 31;
        private string directoryName => @"D:\";
        private string fileName = "sample.json";
        private DataContractJsonSerializer _serializer = new DataContractJsonSerializer(typeof(FunctionData));
        private FunctionData _functionData;
        private List<SyntaxData> _syntaxDataList = new List<SyntaxData>();

        private SyntaxNodeAnalysisContext _context;

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = "循環性複雑度";
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
            _functionData = null;
            _syntaxDataList = new List<SyntaxData>();
            _context = context;
            var methodNode = (MethodDeclarationSyntax)context.Node;
            var onIncreaseComplexityErrorActionList = new List<Action<DiagnosticDescriptor>>();
            var complexity = 1;

            // depth1のノードで計測 2は型、引数、ボディなので2
            var childNodes = methodNode.ChildNodes().ToArray()[2].ChildNodes();
            foreach (var childNode in childNodes)
            {
                // -1しているのはそれぞれのsyntaxでcomplexityを表示しているため
                complexity += AnalyzeCyclomaticComplexity(onIncreaseComplexityErrorActionList, childNode) - 1;
                Console.WriteLine(childNode.GetType().ToString() + complexity);
            }

            if (complexity >= _complexityWarnigPercentCount)
            {
                _context.ReportDiagnostic(Diagnostic.Create(Rule, methodNode.GetLocation(), BuildReportText(methodNode, 0, complexity)));
            }
            else if (complexity >= _complexityErrorPercentCount)
            {
                _context.ReportDiagnostic(Diagnostic.Create(ErrorRule, methodNode.GetLocation(), BuildReportText(methodNode, 0, complexity)));
            }

            _functionData = new FunctionData()
            {
                MethodName = methodNode.Identifier.ToString(),
                ComplexityCount = complexity,
                ChildSyntaxData = _syntaxDataList,
            };
            SerializeData();
        }

        private void SerializeData()
        {
            using (var stream = new MemoryStream())
            using (var fs = new FileStream(directoryName + fileName, FileMode.Create))
            using (var sw = new StreamWriter(fs))
            {
                _serializer.WriteObject(stream, _functionData);
                var str2write = Encoding.UTF8.GetString(stream.ToArray());
                sw.Write(str2write);
            }
        }

        private SyntaxData ConvertFrom(SyntaxNode node)
        {
            var childSyntaxDataList = new List<SyntaxData>();
            foreach(var childNode in node.ChildNodes())
            {
                var childSyntaxData = ConvertFrom(childNode);
                childSyntaxDataList.Add(childSyntaxData);
            }
            return new SyntaxData()
            {
                Body = node.ToString(),
                ChildSyntaxData = childSyntaxDataList,
            };
        }
        

        private int AnalyzeCyclomaticComplexity(List<Action<DiagnosticDescriptor>> onIncreaseComplexityErrorActionList, SyntaxNode node)
        {
            var complexityList = new List<SyntaxNode>();
            var allChildNodeList = new List<SyntaxNode>();

            // 自分自身を数える
            AddListEachType(node, allChildNodeList);

            // 子供を数え上げる
            AddAllChildNodes(node, allChildNodeList);

            foreach (var childNode in allChildNodeList)
            {
                AddListEachType(childNode, complexityList);
            }
            // 初期値1
            var complexity = 1;
            complexity += complexityList.Count;

            if (complexity >= _complexityOneSyntaxMaxCount)
            {
                _context.ReportDiagnostic(Diagnostic.Create(ErrorRule, node.GetLocation(), BuildErrorReportText(complexityList)));
                _syntaxDataList.Add(ConvertFrom(node));
            }

            return complexity;
        }

        private void AddAllChildNodes(SyntaxNode node, List<SyntaxNode> allNodeList)
        {
            foreach(var childNode in node.ChildNodes())
            {
                allNodeList.Add(childNode);
                AddAllChildNodes(childNode, allNodeList);
            }
        }

        private string BuildReportText(SyntaxNode node, int beforeComplexity, int afterComplexity)
        {
            var type = node.GetType();
            var className = type.Name;
            return $"\nまってや！！この関数ちょっと複雑ちゃうか！？！？\nこの部分だけ別の関数に切り出して処理を見やすくしてみいひんか？？\n{ beforeComplexity} -> { afterComplexity}\n{className}}}";
        }

        private string BuildErrorReportText(List<SyntaxNode> nodes)
        {
            var sb = new StringBuilder();
            sb.AppendLine($"ちょっとまってや、、この部分だけで{nodes.Count}も複雑になってるで。");
            var kindNodeNames = nodes.Select(node => node.GetType().Name).Distinct();
            foreach (var nodeName in kindNodeNames)
            {
                var count = nodes.Count(node => node.GetType().Name == nodeName);
                sb.AppendLine($"{nodeName}で上がっているcomplexity...{count}");
            }
            return sb.ToString();
        }

        private void AddListEachType(SyntaxNode node, List<SyntaxNode> list)
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
                list.Add(node);
            }
        }

        private bool IsComplexityNode(SyntaxNode node)
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
            return isIncreased;
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

    [Serializable]
    class SyntaxData
    {
        public string Body { get; set; }
        public List<SyntaxData >ChildSyntaxData { get; set; }
    }

    [Serializable]
    class FunctionData
    {
        public string MethodName { get; set; }
        public int ComplexityCount { get; set; }
        public List<SyntaxData> ChildSyntaxData { get; set; }
    }
}
