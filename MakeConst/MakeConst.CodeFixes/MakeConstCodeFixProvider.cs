using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Simplification;


namespace MakeConst
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(MakeConstCodeFixProvider)), Shared]
    public class MakeConstCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(MakeConstAnalyzer.DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic.
            var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<LocalDeclarationStatementSyntax>().First();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: CodeFixResources.CodeFixTitle,
                    createChangedDocument: c => MakeConstAsync(context.Document, declaration, c),
                    equivalenceKey: nameof(CodeFixResources.CodeFixTitle)),
                diagnostic);
        }

        // Constを適用できる変数定義に対して、自動コード生成を行う
        private static async Task<Document> MakeConstAsync(Document document,
            LocalDeclarationStatementSyntax localDeclaration,
            CancellationToken cancellationToken)
        {
            SyntaxToken firstToken = localDeclaration.GetFirstToken();
            // Triviaが何かあまり理解してない
            SyntaxTriviaList leadingTrivia = firstToken.LeadingTrivia;
            // ローカル変数定義の前にEmptyのsyntaxを追加しようとしている？
            LocalDeclarationStatementSyntax trimmedLocal = localDeclaration.ReplaceToken(
                firstToken, firstToken.WithLeadingTrivia(SyntaxTriviaList.Empty));

            // constのトークンを新規に作成。Factoryが用意されている。便利。
            SyntaxToken constToken = SyntaxFactory.Token(leadingTrivia, SyntaxKind.ConstKeyword, SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker));

            // modifiersをトリミングしたローカルのSyntaxの0番目に無理やり差し込もうとしている？
            SyntaxTokenList newModifiers = trimmedLocal.Modifiers.Insert(0, constToken);

            // If the type of the declaration is 'var', create a new type name
            // for the inferred type.
            VariableDeclarationSyntax variableDeclaration = localDeclaration.Declaration;
            TypeSyntax variableTypeName = variableDeclaration.Type;
            if (variableTypeName.IsVar)
            {
                SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

                // Special case: Ensure that 'var' isn't actually an alias to another type
                // (e.g. using var = System.String).
                IAliasSymbol aliasInfo = semanticModel.GetAliasInfo(variableTypeName, cancellationToken);
                if (aliasInfo == null)
                {
                    // Retrieve the type inferred for var.
                    ITypeSymbol type = semanticModel.GetTypeInfo(variableTypeName, cancellationToken).ConvertedType;

                    // Special case: Ensure that 'var' isn't actually a type named 'var'.
                    if (type.Name != "var")
                    {
                        // Create a new TypeSyntax for the inferred type. Be careful
                        // to keep any leading and trailing trivia from the var keyword.
                        TypeSyntax typeName = SyntaxFactory.ParseTypeName(type.ToDisplayString())
                            .WithLeadingTrivia(variableTypeName.GetLeadingTrivia())
                            .WithTrailingTrivia(variableTypeName.GetTrailingTrivia());

                        // Add an annotation to simplify the type name.
                        TypeSyntax simplifiedTypeName = typeName.WithAdditionalAnnotations(Simplifier.Annotation);

                        // Replace the type in the variable declaration.
                        variableDeclaration = variableDeclaration.WithType(simplifiedTypeName);
                    }
                }
            }
            // Produce the new local declaration.
            LocalDeclarationStatementSyntax newLocal = trimmedLocal.WithModifiers(newModifiers)
                                       .WithDeclaration(variableDeclaration);

            // フォーマッターを使っていい感じにフォーマットしている？
            LocalDeclarationStatementSyntax formattedLocal = newLocal.WithAdditionalAnnotations(Formatter.Annotation);

            // 前の状態と新しくしたい状態のノードを作ってドキュメントを返そうとしている？
            SyntaxNode oldRoot = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            SyntaxNode newRoot = oldRoot.ReplaceNode(localDeclaration, formattedLocal);

            return document.WithSyntaxRoot(newRoot);


        }
    }
}
