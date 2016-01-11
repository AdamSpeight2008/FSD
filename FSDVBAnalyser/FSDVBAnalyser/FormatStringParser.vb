Imports FSD.FormatStringParser

Namespace Global.FSD
    Public Class FormatStringParser

        Public Sub New()
        End Sub

        Public Function Parse(fs As String) As Span
            Return Parse_FormatString(fs)
        End Function

        Private Function NextChar(fs As String, x As Integer) As Char?
            x = x + 1
            Return If(x >= fs.Length, New Char?(), New Char?(fs(x)))
        End Function

        Private Function Parse_FormatString(fs As String) As Span
            Dim content As New LinkedList(Of Span), c As Char, n As Char?, x = 0, bx As Int32?
            While x < fs.Length
                c = fs(x)
                Select Case c
                    Case "{"c
                        AddText(fs, content, bx, x)
                        ' Is it an escaped opening brace?
                        n = NextChar(fs, x)
                        If n.HasValue Then
                            ' Possibly
                            If n.Value = "{" Then
                                x = Add_EscapedOpeningBrace(fs, content, x) ' Escaped Opening Brace
                            Else
                                x = content.AddLast(Parse_ArgHole(fs, x)).Value.ex
                            End If
                        Else
                            ' Opening brace found at end of text
                            ' Error as it can't be either ;=
                            ' a) Escaped
                            ' b) Start of an Arg Hole
                            x = Add_UnexpectedOpeningBrace(fs, content, x)
                        End If

                    Case "}"c
                        AddText(fs, content, bx, x)
                        ' Is it an escaped closing brace?
                        n = NextChar(fs, x)
                        If n.HasValue AndAlso (n.Value = "}"c) Then
                            x = Add_EscapedClosingBrace(fs, content, x) ' Escape Closing Brace
                        Else
                            x = Add_UnexpectedClosingBrace(fs, content, x)
                        End If
                    Case Else
                        ' Treat as normal text
                        If bx.HasValue = False Then bx = New Int32?(x)
                        x += 1
                End Select
            End While
            Return New Span(fs, SpanKind.FormatString, 0, x, content)
        End Function

        Private Sub AddText(fs As String, ByRef contents As LinkedList(Of Span), ByRef bx As Int32?, x As Int32)
            If bx.HasValue Then
                contents.AddLast(New Span(fs, SpanKind.Text, bx.Value, x))
                bx = Nothing
            End If
        End Sub

        Private Function Add_UnexpectedEndOfText(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Unexpected_EOT, x, x)).Value.ex
        End Function

        Private Function Add_ClosingBrace(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Closing_Brace, x, x + 1)).Value.ex
        End Function

        Private Function Add_OpeningBrace(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Opening_Brace, x, x + 1)).Value.ex
        End Function

        Private Function Add_UnexpectedChar(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Error_Unexpected_Char, x, x + 1)).Value.ex
        End Function

        Private Function Add_UnexpectedClosingBrace(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Error_Unexpected_Closing_Brace, x, x + 1)).Value.ex
        End Function

        Private Function Add_UnexpectedOpeningBrace(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Error_Unexpected_Opening_Brace, x, x + 1)).Value.ex
        End Function

        Private Function Add_EscapedClosingBrace(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Escaped_Closing_Brace, x, x + 2)).Value.ex
        End Function

        Private Function Add_EscapedOpeningBrace(fs As String, ByRef content As LinkedList(Of Span), x As Integer) As Integer
            Return content.AddLast(New Span(fs, SpanKind.Escaped_Opening_Brace, x, x + 2)).Value.ex
        End Function

        Private Function Parse_ArgHole(fs As String, x As Integer) As Span
            Dim bx = x, contents As New LinkedList(Of Span)
            x = contents.AddLast(New Span(fs, SpanKind.Opening_Brace, bx, bx + 1)).Value.ex
            x = contents.AddLast(Parse_Whitespace(fs, x)).Value.ex
            x = contents.AddLast(Parse_Arg_Index(fs, x)).Value.ex
            x = contents.AddLast(Parse_Whitespace(fs, x)).Value.ex
            If x >= fs.Length Then Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_UnexpectedEndOfText(fs, contents, x), contents)
            Dim ch = fs(x)
            ' Is it the Alignment seperator glyph?
            If ch = "," Then
                x = contents.AddLast(Parse_Arg_Align(fs, x)).Value.ex
                If x >= fs.Length Then Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_UnexpectedEndOfText(fs, contents, x), contents)
                ch = fs(x)
            End If
            ' Is it the format seperator glyph?
            If ch = ":" Then
                x = contents.AddLast(Parse_Arg_Format(fs, x)).Value.ex
                If x >= fs.Length Then Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_UnexpectedEndOfText(fs, contents, x), contents)
                ch = fs(x)
            End If
            ' Is it the opening brace glyph?
            If ch = "{" Then
                Dim nc = NextChar(fs, x)
                If nc.HasValue Then
                    If nc.Value = "{"c Then Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_EscapedOpeningBrace(fs, contents, x), contents)
                    Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_UnexpectedChar(fs, contents, x), contents)
                Else
                    Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_UnexpectedEndOfText(fs, contents, x), contents)
                End If
            ElseIf ch = "}" Then
                Dim nc = NextChar(fs, x)
                If nc.HasValue AndAlso nc.Value = "}"c Then Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_EscapedClosingBrace(fs, contents, x), contents)
                Return New Span(fs, SpanKind.Arg_Hole, bx, Add_ClosingBrace(fs, contents, x), contents)
            Else
                Return New Span(fs, SpanKind.Error_Arg_Hole, bx, Add_UnexpectedChar(fs, contents, x), contents)
            End If
        End Function

        Private Function Parse_Arg_Format(fs As String, x As Integer) As Span
            Dim bx = x, contents As New LinkedList(Of Span), ch As Char, tx As Int32? = Nothing
            x = contents.AddLast(New Span(fs, SpanKind.Colon, bx, x + 1)).Value.ex
            While True
                If x >= fs.Length Then Return New Span(fs, SpanKind.Error_Arg_Format, bx, Add_UnexpectedEndOfText(fs, contents, x), contents) ' Unexpected EOT
                ch = fs(x)
                Select Case ch
                    Case "}"c
                        AddText(fs, contents, tx, x)
                        Dim nc = NextChar(fs, x)
                        If nc.HasValue = False OrElse nc.Value <> "}"c Then Return New Span(fs, SpanKind.Arg_Format, bx, x, contents)
                        x = Add_EscapedClosingBrace(fs, contents, x) ' Escaped Closing Brace
                    Case "{"c
                        AddText(fs, contents, tx, x)
                        Dim nc = NextChar(fs, x)
                        If nc.HasValue = False Then Return New Span(fs, SpanKind.Error_Arg_Format, bx, Add_UnexpectedEndOfText(fs, contents, x), contents)
                        If nc.Value = "{"c Then
                            ' Escaped Opening Brace
                            x = Add_EscapedOpeningBrace(fs, contents, x)
                        Else
                            x = Add_UnexpectedOpeningBrace(fs, contents, x)
                        End If
                    Case Else
                        If tx.HasValue = False Then tx = New Int32?(x)
                        x += 1
                        '                        x = contents.AddLast(New Span(fs, SpanKind.Text, bx, x + 1)).Value.ex
                End Select
            End While
            ' Unexpectedly reached impossible place
            Return New Span(fs, SpanKind.Error_Arg_Format, bx,
                        contents.AddLast(New Span(fs, SpanKind.Error_Arg_Format_Impossible, x, x)).Value.ex, contents)
        End Function

        Private Function Parse_Arg_Align(fs As String, x As Integer) As Span
            Dim bx = x, contents As New LinkedList(Of Span)
            x = contents.AddLast(New Span(fs, SpanKind.Comma, x, x + 1)).Value.ex
            If x >= fs.Length Then Return New Span(fs, SpanKind.Error_Arg_Align, bx, Add_UnexpectedEndOfText(fs, contents, x), contents)  ' Unexpect EOT
            Dim ws = Parse_Whitespace(fs, x)
            x = contents.AddLast(ws).Value.ex
            Dim ch = fs(x)
            If ch = "-"c Then
                x = contents.AddLast(New Span(fs, SpanKind.Minus, x, x + 1)).Value.ex
                x = contents.AddLast(Parse_Digits(fs, x)).Value.ex
                Return New Span(fs, SpanKind.Arg_Alignment, bx, x, contents)
            ElseIf IsDigit(ch) Then
                Return New Span(fs, SpanKind.Arg_Alignment, bx, contents.AddLast(Parse_Digits(fs, x)).Value.ex, contents)
            Else
                Return New Span(fs, SpanKind.Error_Arg_Align, bx, x, contents)
            End If
        End Function

        Private Function Parse_Arg_Index(fs As String, x As Integer) As Span
            Dim bx = x, digits = Parse_Digits(fs, x)
            x = digits.ex
            Return New Span(fs, If(digits.Kind = SpanKind.Empty, SpanKind.Error_Arg_Index, SpanKind.Arg_Index), bx, x, digits)
        End Function

        Private Function Parse_Whitespace(fs As String, x As Integer) As Span
            Dim bx = x, c As Char
            While x < fs.Length
                c = fs(x)
                If c <> " "c Then Exit While
                x += 1
            End While
            Dim d = x - bx
            Return New Span(fs, If(d = 0, SpanKind.Empty, SpanKind.Whitespace), bx, x)
        End Function

        Private Function IsDigit(ch As Char) As Boolean
            Return ("0"c <= ch) AndAlso (ch <= "9"c)
        End Function

        Private Function Parse_Digits(fs As String, x As Integer) As Span
            Dim bx = x, c As Char
            While x < fs.Length
                c = fs(x)
                If IsDigit(c) = False Then Exit While
                x += 1
            End While
            Dim d = x - bx
            Return New Span(fs, If(d = 0, SpanKind.Empty, SpanKind.Digits), bx, x)
        End Function

        Public Class Span
            Public ReadOnly Property fs As String
            Public ReadOnly Property bx As Integer
            Public ReadOnly Property ex As Integer
            Public ReadOnly Property Kind As SpanKind
            Public ReadOnly Property Contents As New LinkedList(Of Span)

            Friend Sub New(fs As String, Kind As SpanKind, bx As Integer, ex As Integer)
                Me.fs = fs
                Me.Kind = Kind
                Me.bx = bx
                Me.ex = ex
            End Sub

            Friend Sub New(fs As String, Kind As SpanKind, bx As Integer, ex As Integer, s As Span)
                Me.New(fs, Kind, bx, ex)
                If s IsNot Nothing Then Me.Contents.AddLast(s)
            End Sub

            Friend Sub New(fs As String, Kind As SpanKind, bx As Integer, ex As Integer, c As LinkedList(Of Span))
                Me.New(fs, Kind, bx, ex)
                Me.Contents = New LinkedList(Of Span)(c)
            End Sub

            Friend Sub New(fs As String, Kind As SpanKind, bx As Integer, ex As Integer, c As LinkedList(Of Span), s As Span)
                Me.New(fs, Kind, bx, ex, c)
                If s IsNot Nothing Then Me.Contents.AddLast(s)
            End Sub

            Public Overrides Function ToString() As String
                Return $"({Kind}) [{GetSpanText()}]"
            End Function

            Public Function GetSpanText() As String
                Dim d = ex - bx
                Return If(d <= 0, "", fs.Substring(bx, d))
            End Function

            Public Shared Operator +(s0 As Span, s1 As Span) As Span
                If (s0 Is Nothing) OrElse (s1 Is Nothing) OrElse (s0.fs <> s1.fs) Then Return Nothing
                Return New Span(s0.fs, SpanKind.Empty, If(s0.bx <= s1.bx, s0.bx, s1.bx), If(s1.ex <= s0.ex, s0.ex, s1.ex))
            End Operator
        End Class

        Public Enum SpanKind As Integer
            Empty = 0
            FormatString
            Digits
            Whitespace
            Text
            Escaped_Opening_Brace
            Escaped_Closing_Brace
            Arg_Hole
            Opening_Brace
            Closing_Brace
            Arg_Index
            Comma
            Arg_Alignment
            Colon
            Arg_Format
            Minus
            Unexpected_EOT
            Error_Arg_Index
            Error_Arg_Align
            Error_Arg_Format
            Error_Arg_Hole
            Error_FormatString
            Error_Arg_Format_Impossible
            Error_Unexpected_Opening_Brace
            Error_Unexpected_Closing_Brace
            Error_Unexpected_Char
        End Enum

    End Class


    Public Module Module1

        'Sub Main()
        '    Dim p As New FSD.FormatStringParser
        '    Dim fs = "a {01234567,1234567:x2}bb{1:aaa}"
        '    Dim s = p.Parse(fs)
        '    Colorise(s)
        '    Console.WriteLine()
        '    Dim Holes = ArgHoles(s).ToArray
        '    Dim HoleIndice = ArgIndice(Holes).ToArray
        '    Dim ec = ErrorCount(s)
        '    Dim f = Flatten(s).ToArray
        '    Dim args = {"AA", "BB", "CC"}
        '    ComplexIssues(s, args)
        'End Sub

        Function ErrorCount(s As FSD.FormatStringParser.Span) As Integer?
            If s?.Kind <> SpanKind.FormatString Then Return Nothing
            Return s.Contents.Where(Function(x) IsErrorKind(x)).Count
        End Function

        Public Function IsErrorKind(s As FSD.FormatStringParser.Span) As Boolean
            Select Case s.Kind
                Case SpanKind.Error_Arg_Align,
                     SpanKind.Error_Arg_Format,
                     SpanKind.Error_Arg_Format_Impossible,
                     SpanKind.Error_Arg_Hole,
                     SpanKind.Error_Arg_Index,
                     SpanKind.Error_FormatString,
                     SpanKind.Error_Unexpected_Char,
                     SpanKind.Error_Unexpected_Closing_Brace,
                     SpanKind.Error_Unexpected_Opening_Brace,
                     SpanKind.Unexpected_EOT
                    Return True
            End Select
            Return False
        End Function

        Public Function ArgHoles(s As FSD.FormatStringParser.Span) As IEnumerable(Of FSD.FormatStringParser.Span)
            Return s.Contents.Where(Function(p) p.Kind = SpanKind.Arg_Hole OrElse p.Kind = SpanKind.Error_Arg_Hole)
        End Function

        Public Iterator Function ArgIndice(xs As IEnumerable(Of FSD.FormatStringParser.Span)) As IEnumerable(Of Integer?)
            For Each h In xs
                If h.Kind <> SpanKind.Error_Arg_Hole AndAlso h.Kind <> SpanKind.Arg_Hole Then Continue For
                For Each p In h.Contents
                    If p.Kind <> SpanKind.Arg_Index AndAlso p.Kind <> SpanKind.Error_Arg_Index Then Continue For
                    Dim r = p.Contents.FirstOrDefault(Function(x) x.Kind = SpanKind.Digits)
                    If r Is Nothing Then Continue For
                    Dim value As Integer
                    If Integer.TryParse(r.GetSpanText, value) = False Then Continue For
                    Yield value
                Next
            Next
        End Function

        'Public Sub Colorise(s As FSD.FormatStringParser.Span)
        '    Select Case s.Kind
        '        Case SpanKind.Closing_Brace,
        '             SpanKind.Opening_Brace
        '            Console.ForegroundColor = ConsoleColor.Green
        '            Console.Write(s.GetSpanText)
        '        Case SpanKind.Colon,
        '             SpanKind.Comma
        '            Console.ForegroundColor = ConsoleColor.Green
        '            Console.Write(s.GetSpanText)
        '        Case SpanKind.Escaped_Closing_Brace,
        '             SpanKind.Escaped_Opening_Brace
        '            Console.ForegroundColor = ConsoleColor.DarkGray
        '            Console.Write(s.GetSpanText)
        '        Case SpanKind.Error_Arg_Format,
        '             SpanKind.Error_Arg_Align,
        '             SpanKind.Error_Arg_Hole,
        '             SpanKind.Error_Arg_Index
        '            Console.BackgroundColor = ConsoleColor.White
        '            Console.ForegroundColor = ConsoleColor.Red
        '            Console.Write(s.GetSpanText)
        '            Console.ResetColor()
        '        Case SpanKind.FormatString,
        '             SpanKind.Arg_Hole,
        '             SpanKind.Arg_Index,
        '             SpanKind.Arg_Alignment,
        '             SpanKind.Arg_Format

        '            For Each c In s.Contents
        '                Colorise(c)
        '            Next
        '        Case Else
        '            'If s.Contents.Any = False Then
        '            Console.ForegroundColor = ConsoleColor.White
        '            Console.Write(s.GetSpanText)

        '            'End If
        '    End Select
        '    Console.ResetColor()
        'End Sub

        Iterator Function Flatten(s As FSD.FormatStringParser.Span) As IEnumerable(Of FSD.FormatStringParser.Span)
            If s Is Nothing Then Return ' Enumerable.Empty(Of FSD.FormatStringParser.Span)
            Dim xs As New LinkedList(Of FSD.FormatStringParser.Span)
            xs.AddFirst(s)
            While xs.Any
                Dim xn = xs.First
                Dim x = xn.Value
                Yield x
                xs.RemoveFirst()
                If x.Contents.Any = False Then Continue While
                Dim n = x.Contents.Count - 1
                Dim f = xs.AddFirst(x.Contents(0))
                For i = 1 To n
                    f = xs.AddAfter(f, x.Contents(i))
                Next
            End While
        End Function

        Iterator Function SimpleIssues(s As FSD.FormatStringParser.Span) As IEnumerable(Of FSD.FormatStringParser.Span)
            Dim fs = Flatten(s)
            If fs.Any = False Then Return
            For Each f In fs
                If IsErrorKind(f) Then Yield f
            Next
        End Function

        Sub ComplexIssues(context As SyntaxNodeAnalysisContext,
                            fsobj As ArgumentSyntax,
                                s As FSD.FormatStringParser.Span,
                             args As ArgumentListSyntax,
                          ArgObjs As Object()) 'As IEnumerable(Of FSD.FormatStringParser.Span)
            If (s Is Nothing) OrElse (ArgObjs Is Nothing) Then Return
            If ArgObjs.Count <= 1 Then
                Report(context, fsobj, "No arguments supplied.")
            Else
                Dim holes = ArgHoles(s).ToArray
                Dim holeCount = holes.Count
                Dim ArgCount = ArgObjs.Count - 1
                If holeCount = 0 Then
                    Report(context, fsobj, "No ArgHoles in the format string.")
                Else
                    Dim used As New HashSet(Of Integer)
                    For Each hole In holes
                        Dim fs = Flatten(hole).ToArray

                        For ix = 0 To fs.Count - 1
                            Dim f = fs(ix)
                            Select Case f.Kind
                                Case SpanKind.Arg_Index
                                    Analyse_ArgHole_Index(context, f, ArgCount, fs, used, ix, fsobj)
                                Case SpanKind.Arg_Alignment
                                    Analyse_ArgHole_Align(context, fsobj, f, fs, ix)
                            End Select
                        Next
                    Next
                    ' report unused arg indice
                    Dim unused = Enumerable.Range(0, ArgCount).Except(used)
                    For Each u In unused
                        Report(context, fsobj, $"Argument Index:= {u} unused.")
                    Next
                End If
            End If
        End Sub

        Private Sub Report(context As SyntaxNodeAnalysisContext, fsobj As ArgumentSyntax, v As String)
            context.ReportDiagnostic(Diagnostic.Create(FSDVBAnalyser.FSD_VB_AnalyserAnalyzer.Rule0, fsobj.GetLocation, v))
            '   Throw New NotImplementedException()
        End Sub
        Private Sub Report(context As SyntaxNodeAnalysisContext, fsobj As ArgumentSyntax, s As Span, v As String)
            context.ReportDiagnostic(Diagnostic.Create(FSDVBAnalyser.FSD_VB_AnalyserAnalyzer.Rule1,
                                     Location.Create(fsobj.SyntaxTree, TextSpan.FromBounds(fsobj.SpanStart + s.bx + 1, fsobj.SpanStart + s.ex + 1)), v))
        End Sub

        Sub Analyse_ArgHole_Index(
                             context As SyntaxNodeAnalysisContext,
                                   s As FSD.FormatStringParser.Span,
                           holeCount As Integer,
                                  fs As IEnumerable(Of FSD.FormatStringParser.Span),
                                used As HashSet(Of Integer),
                                  ix As Integer,
                                   fsobj As ArgumentSyntax
                                 )
            If s?.Kind <> SpanKind.Arg_Index Then Return
            Dim value = GetValue(Nothing, fs(ix + 1))
            If value.HasValue Then
                If value.Value < 0 Then
                    Report(context, fsobj, s, $"Negative Arg Index Not allowed (Range: 0 <= Arg Index (Value: {value.Value}) < {holeCount} )")
                Else
                    If value.Value >= DotNetLimit Then
                        Report(context, fsobj, s, $"Arg Index exceeds .Net internal limit of {DotNetLimit}.")
                    ElseIf value.Value >= holeCount Then
                        Report(context, fsobj, s, $"Arg Index exceeds upper bound. (Range: 0 <= Arg Index (Value: {value.Value}) < {holeCount} )")
                    Else
                        ' Mark Arg Index as being used.
                        used.Add(value.Value)
                    End If
                End If
            Else
                'report

            End If

        End Sub


        Private Const DotNetLimit As Integer = 1000000

        Sub Analyse_ArgHole_Align(context As SyntaxNodeAnalysisContext,
                                  fsobj As ArgumentSyntax,
                                   s As FSD.FormatStringParser.Span,
                                  fs As IEnumerable(Of FSD.FormatStringParser.Span),
                                  ix As Integer
                                 )
            If s?.Kind <> SpanKind.Arg_Alignment Then Return
            Dim minus = s.Contents.FirstOrDefault(Function(m) m.Kind = SpanKind.Minus)
            Dim digits = s.Contents.FirstOrDefault(Function(m) m.Kind = SpanKind.Digits)
            Dim value = GetValue(minus, digits)
            If value.HasValue Then
                If (value.Value <= -DotNetLimit) Then
                    Report(context, fsobj, minus + digits, $"Arg Align value is not within allowed range. (Range: -{DotNetLimit} < Arg Index (Value: {value.Value}) < {DotNetLimit} )")
                ElseIf (value.Value >= DotNetLimit) Then
                    Report(context, fsobj, digits, $"Arg Align value is not within allowed range. (Range: -{DotNetLimit} < Arg Index (Value: {value.Value}) < {DotNetLimit} )")
                End If
            Else
                'report
            End If
        End Sub

        Function GetValue(minus As FSD.FormatStringParser.Span, digits As FSD.FormatStringParser.Span) As Integer?
            Dim value As Integer
            If minus IsNot Nothing Then
                Return If(digits IsNot Nothing AndAlso Integer.TryParse(digits.GetSpanText, value), New Integer?(-value), Nothing)
            Else
                Return If(digits IsNot Nothing AndAlso Integer.TryParse(digits.GetSpanText, value), New Integer?(value), Nothing)
            End If
        End Function

    End Module
End Namespace