using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.Serialization.Json;
using System.Text;

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
            _functionData = new FunctionData()
            {
                MethodName = methodNode.Identifier.ToString(),
                ComplexityCount = complexity,
                ChildSyntaxData = _syntaxDataList,
            };

            if (complexity >= _complexityWarnigPercentCount)
            {
                _context.ReportDiagnostic(Diagnostic.Create(Rule, methodNode.GetLocation(), Display.BuildReport(_functionData)));
            }
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

        private SyntaxData ConvertFrom(SyntaxNode node, int nestCount)
        {
            nestCount++;
            var childSyntaxDataList = new List<SyntaxData>();
            foreach (var childNode in node.ChildNodes())
            {
                var childSyntaxData = ConvertFrom(childNode, nestCount);
                childSyntaxDataList.Add(childSyntaxData);
            }
            return new SyntaxData()
            {
                Body = node.ToString(),
                ChildSyntaxData = childSyntaxDataList,
                NestCount = nestCount,
                SyntaxType = ConvertAs(node),
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
                var syntaxData = ConvertFrom(node, 0);
                var report = Display.BuildReport(syntaxData, complexity);
                _context.ReportDiagnostic(Diagnostic.Create(ErrorRule, node.GetLocation(), report));
                _syntaxDataList.Add(syntaxData);
            }

            return complexity;
        }

        private void AddAllChildNodes(SyntaxNode node, List<SyntaxNode> allNodeList)
        {
            foreach (var childNode in node.ChildNodes())
            {
                allNodeList.Add(childNode);
                AddAllChildNodes(childNode, allNodeList);
            }
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

        private SyntaxNodeType ConvertAs(SyntaxNode node)
        {
            switch (node)
            {
                case IfStatementSyntax syntax:
                    return SyntaxNodeType.IfStatement;
                case BinaryExpressionSyntax syntax:
                    return SyntaxNodeType.Binary;
                case WhileStatementSyntax syntax:
                    return SyntaxNodeType.While;
                case ForStatementSyntax syntax:
                    return SyntaxNodeType.ForStatement;
                case SwitchStatementSyntax syntax:
                    return SyntaxNodeType.Switch;
                case SwitchLabelSyntax syntax:
                    return SyntaxNodeType.SwitchLabel;
                    case DoStatementSyntax syntax:
                    return SyntaxNodeType.DoStatement;
                case AssignmentExpressionSyntax syntax:
                    return SyntaxNodeType.Assignment;
            }
            return SyntaxNodeType.None;
            
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

        enum SyntaxNodeType
        {
            None = 0,
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
            Binary = 12,
            Assignment = 13,
        }

        [Serializable]
        class SyntaxData
        {
            public string Body { get; set; }
            public List<SyntaxData> ChildSyntaxData { get; set; }
            public int NestCount { get; set; }
            public SyntaxNodeType SyntaxType { get; set; }
        }

        [Serializable]
        class FunctionData
        {
            public string MethodName { get; set; }
            public int ComplexityCount { get; set; }
            public List<SyntaxData> ChildSyntaxData { get; set; }
        }

        static class Display
        {
            private static int _showBodyCount = 0;
            // 特定の部分でのレポートを出す 
            public static string BuildReport(SyntaxData syntaxData, int complexity)
            {
                var allSyntaxData = new List<SyntaxData>();
                SelectManySyntax(syntaxData, allSyntaxData);
                var sb = new StringBuilder();
                sb.AppendLine("");

                if (syntaxData.NestCount > 2)
                {
                    return "";
                }

                // ネストが一定数以上の場合
                var maxNestCount = GetNest(syntaxData);
                if (maxNestCount > ConfigData.AllowNestCount)
                {
                    var body = new string(syntaxData.Body.Take(_showBodyCount).ToArray());

                    sb.AppendLine($"ネストが規定以上あります。");
                }

                // 条件が一定数以上の場合
                var binaryCount = allSyntaxData.Count(node => node.SyntaxType == SyntaxNodeType.Binary);
                if (binaryCount > ConfigData.AllowBinaryConditioncount)
                {
                    var body = new string(syntaxData.Body.Take(_showBodyCount).ToArray());
                    sb.AppendLine($"条件が規定以上あります。");
                }

                // switchの場合
                var switchCaseCount = allSyntaxData.Count(node => node.SyntaxType == SyntaxNodeType.SwitchExpressionArm);
                var switchNode = allSyntaxData.FirstOrDefault(node => node.SyntaxType == SyntaxNodeType.Switch);
                if (switchCaseCount > ConfigData.AllowSwitchCaseCount)
                {
                    var body = new string(syntaxData.Body.Take(_showBodyCount).ToArray());
                    sb.AppendLine($"Switchの分岐が規定以上あります。");
                }

                // 循環性複雑度が一定数以上の場合
                if (complexity > ConfigData.AllowSyntaxComplexityCount)
                {
                    var body = new string(syntaxData.Body.Take(_showBodyCount).ToArray());
                    sb.AppendLine($"この部分で局所的に複雑になっているようです。");
                }
                sb.AppendLine("この部分を別の関数に切り出してみませんか？");
                sb.AppendLine($"循環的複雑度...{complexity}");


                return sb.ToString();
            }

            // 関数全体でのレポートを出す
            public static string BuildReport(FunctionData functionData)
            {
                var sb = new StringBuilder();

                // 循環性複雑度が一定数以上の場合
                if (functionData.ComplexityCount > ConfigData.AllowFunctionComplexityCount)
                {
                    sb.AppendLine($"関数全体で複雑になっているようです。処理部分ごとに別の関数に切り出して複雑度を下げませんか？");
                }

                return sb.ToString();
            }


            private static void SelectManySyntax(SyntaxData syntaxData, List<SyntaxData> list)
            {
                var currentSyntaxData = syntaxData;
                if (currentSyntaxData.ChildSyntaxData == null)
                {
                    return;
                }
                foreach (var childSyntaxData in currentSyntaxData.ChildSyntaxData)
                {
                    list.Add(childSyntaxData);
                    SelectManySyntax(childSyntaxData, list);
                }
            }


            private static int GetNest(SyntaxData syntaxData)
            {
                return GetNestInternal(syntaxData, 0);
            }

            private static int GetNestInternal(SyntaxData syntaxData, int nestCount)
            {
                var maxNest = nestCount;
                var childNodes = syntaxData.ChildSyntaxData;
                if (childNodes == null)
                {
                    return maxNest;
                }
                if(syntaxData.SyntaxType == SyntaxNodeType.Binary)
                {
                    return maxNest;
                }
                foreach (var childNode in childNodes)
                {
                    var childNest = GetNestInternal(childNode, nestCount + 1);
                    if (maxNest < childNest)
                    {
                        maxNest = childNest;
                    }
                }
                return maxNest;
            }
        }
    

        public static class ConfigData
        {
            public static int AllowNestCount = 2;
            public static int AllowSyntaxComplexityCount = 3;// ここを変えたら表示される
            public static int AllowFunctionComplexityCount = 5;// ここを変えたら表示される
            public static int AllowBinaryConditioncount = 2;
            public static int AllowSwitchCaseCount = 5;
        }
    }
}