Imports FSDVBAnalyser.Exts

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class FSD_VB_AnalyserAnalyzer
  Inherits DiagnosticAnalyzer

    Private Shared _FSA As New FSD.FormatStringParser

    Public Const DiagnosticId = "Q1"

    ' You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
    Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.AnalyzerTitle), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.AnalyzerMessageFormat), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.AnalyzerDescription), My.Resources.ResourceManager, GetType(My.Resources.Resources))
    Private Const Category = "Format Strings"

    Public Shared Rule0 As New DiagnosticDescriptor("FSD0", Title, MessageFormat, Category, DiagnosticSeverity.Info, isEnabledByDefault:=True, description:=Description)
    Public Shared Rule1 As New DiagnosticDescriptor("FSD1", Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault:=True, description:=Description)

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule0, Rule1)
        End Get
    End Property


    Public Overrides Sub Initialize(context As AnalysisContext)
    ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
    ' context.RegisterSymbolAction(AddressOf Analyze, SymbolKind.Method)
    Debug.WriteLine("Initializing ...")
    context.RegisterSyntaxNodeAction(AddressOf AnalyzeNode, SyntaxKind.SimpleMemberAccessExpression)
    Debug.WriteLine("... Initiailized")
  End Sub



    Private Sub AnalyzeNode(context As SyntaxNodeAnalysisContext)
        Try
            Dim x = CType(context.Node, MemberAccessExpressionSyntax)
            If x Is Nothing Then Exit Sub
            If x.OperatorToken.ValueText = "." Then
                Debug.WriteLine("Hello")
                Dim _MethodName = x.Name.ToString
                If _MethodName = "" Then Exit Sub
                Debug.WriteLine($"{_MethodName}")
                Dim _CalledOnObjOfType = x.CalledOnType(context.SemanticModel, context.CancellationToken)
                Dim _TypeName = If(_CalledOnObjOfType Is Nothing, "", _CalledOnObjOfType.ToFullyQualifiedName)
                Dim _TypeNameA() = _TypeName.Split("."c)
                Dim _InvokeExpr = TryCast(x.Parent, InvocationExpressionSyntax)
                If _InvokeExpr Is Nothing Then Exit Sub

                If _TypeName <> "System.String" Then Exit Sub
                If _MethodName <> "Format" Then Exit Sub

                Dim Args = _InvokeExpr.ArgumentList


                Dim ArgObjs = Args.GetArgumentAsObjects(context.SemanticModel, context.CancellationToken).ToArray
                Dim fo = Args.Arguments(0)
                Dim fs = fo.GetText.ToString
                Dim res = _FSA.Parse(fs)
                FSD.Module1.ComplexIssues(context, fo, res, Args, ArgObjs)
                '                Dim d = Diagnostic.Create(Rule, x.GetLocation)
                '               context.ReportDiagnostic(d)
                Debug.WriteLine("mum")

            End If
        Catch ex As Exception
            Throw ex
        End Try

    End Sub

End Class