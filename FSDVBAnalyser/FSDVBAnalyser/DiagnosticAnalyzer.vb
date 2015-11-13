Imports FSDVBAnalyser.Exts

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class FSD_VB_AnalyserAnalyzer
  Inherits DiagnosticAnalyzer

  Public Const DiagnosticId = "FSD_VB_Analyser"

  ' You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
  Private Shared ReadOnly Title As LocalizableString = New LocalizableResourceString(NameOf(My.Resources.AnalyzerTitle), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Private Shared ReadOnly MessageFormat As LocalizableString = New LocalizableResourceString(NameOf(My.Resources.AnalyzerMessageFormat), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Private Shared ReadOnly Description As LocalizableString = New LocalizableResourceString(NameOf(My.Resources.AnalyzerDescription), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Private Const Category = "Naming"

  Private Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault:=True, description:=Description)

  Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
    Get
      Return ImmutableArray.Create(Rule)
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

      Dim Args = _InvokeExpr.ArgumentList


      Dim ArgObjs = Args.GetArgumentAsObjects(context.SemanticModel, context.CancellationToken)
      Dim d = Diagnostic.Create(Rule, x.GetLocation)
      context.ReportDiagnostic(d)
      Debug.WriteLine("mum")

    End If

  End Sub


  'Private Sub AnalyzeSymbol(context As SymbolAnalysisContext)
  '      ' TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find

  '      Dim namedTypeSymbol = CType(context.Symbol, INamedTypeSymbol)

  '      ' Find just those named type symbols with names containing lowercase letters.
  '      If namedTypeSymbol.Name.ToCharArray.Any(AddressOf Char.IsLower) Then
  '          ' For all such symbols, produce a diagnostic.
  '          Dim diag = Diagnostic.Create(Rule, namedTypeSymbol.Locations(0), namedTypeSymbol.Name)

  '          context.ReportDiagnostic(diag)
  '      End If
  '  End Sub
End Class