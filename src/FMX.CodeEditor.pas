// Eduardo - 27/06/2021
unit FMX.CodeEditor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.UITypes,
  FMX.Controls,
  FMX.Types,
  FMX.TextLayout,
  FMX.Graphics,
  FMX.Objects,
  FMX.CodeEditor.Part,
  FMX.CodeEditor.Attribute,
  FMX.CodeEditor.Caret,
  FMX.CodeEditor.Selection;

type
  TFMXCodeEditor = class(TControl)
  private
    procedure SetCaretPos(const Value: Integer);
  protected
    FFont: TFont;
    FFontColor: TAlphaColor;
    FBackColor: TAlphaColor;
    FCaretColor: TAlphaColor;
    FLayout: TTextLayout;
    FCaret: TInternalCaret;
    FAttributes: TObjectList<TInternalAttribute>;
    FParts: TObjectList<TInternalPart>;
    FCaretPos: Integer;
    FCode: TStrings;
    FSyntax: TStrings;
    FTheme: TStrings;
    FAutoSize: Boolean;
    FLineCount: Integer;
    FLineHeight: Single;
    FSelection: TInternalSelection;
    property CaretPos: Integer read FCaretPos write SetCaretPos;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  private
    procedure CalcExpression;
    procedure CalcAttribute;
    procedure CalcSelection;
    procedure CalcPainting;
    procedure CalcCaret;
    procedure CalcSize;
    function GetCode: TStrings;
    function GetSyntax: TStrings;
    function GetTheme: TStrings;
    procedure SetCode(const Value: TStrings);
    procedure SetSyntax(const Value: TStrings);
    procedure SetTheme(const Value: TStrings);
    procedure ItemOnChange(Sender: TObject);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function Ready: Boolean;
    procedure SetCaretColor(const Value: TAlphaColor);
    procedure SetAutoSize(const Value: Boolean);
    procedure CalcLine;
    procedure BackSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LineCount: Integer read FLineCount;
    property LineHeight: Single read FLineHeight;
  published
    property Font: TFont read GetFont write SetFont stored True;
    property FontColor: TAlphaColor read FFontColor write FFontColor stored True;
    property BackColor: TAlphaColor read FBackColor write FBackColor stored True;
    property CaretColor: TAlphaColor read FCaretColor write SetCaretColor stored True;
    property Code: TStrings read GetCode write SetCode stored True;
    property Syntax: TStrings read GetSyntax write SetSyntax stored True;
    property Theme: TStrings read GetTheme write SetTheme stored True;
    property AutoSize: Boolean read FAutoSize write SetAutoSize stored False;
    property Align;
    property Anchors;
    property Width;
    property Height;
    property Size;
    property Position;
  end;

implementation

{ TFMXCodeEditor }

uses
  System.Types,
  System.SysUtils,
  System.JSON,
  System.RegularExpressions,
  System.Math,
  System.StrUtils;

constructor TFMXCodeEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CanFocus := True;
  TabStop  := True;

  FFont           := TFont.Create;
  FFont.OnChanged := ItemOnChange;
  FFont.Family    := 'Cascadia Mono';

  FFontColor  := TAlphaColorRec.Black;
  FBackColor  := TAlphaColorRec.White;
  FCaretColor := TAlphaColorRec.Black;

  FLayout     := TTextLayoutManager.DefaultTextLayout.Create;
  FAttributes := TObjectList<TInternalAttribute>.Create;
  FParts      := TObjectList<TInternalPart>.Create;
  FSelection  := TInternalSelection.Create;

  FCaret  := TInternalCaret.Create(Self);

  FCode   := TStringList.Create;
  FSyntax := TStringList.Create;
  FTheme  := TStringList.Create;

  FCode.Text := EmptyStr;
  FSyntax.Text := '{}';
  FTheme.Text := '{}';

  TStringList(FCode).OnChange   := ItemOnChange;
  TStringList(FSyntax).OnChange := ItemOnChange;
  TStringList(FTheme).OnChange  := ItemOnChange;

  CaretPos := 0;
  Width    := 200;
  Height   := 100;

  FSelection.Start := 13;
  FSelection.Length := 5;
end;

destructor TFMXCodeEditor.Destroy;
begin
  FLayout.DisposeOf;
  FAttributes.DisposeOf;
  FParts.DisposeOf;
  FSelection.DisposeOf;
  FFont.DisposeOf;
  FCode.DisposeOf;
  FSyntax.DisposeOf;
  FTheme.DisposeOf;
  FCaret.DisposeOf;
  inherited;
end;

function TFMXCodeEditor.Ready: Boolean;
begin
  Result :=
    Assigned(FFont) and
    Assigned(FCode) and
    Assigned(FSyntax) and
    Assigned(FTheme) and
    not (FCode.Text.IsEmpty or FSyntax.Text.IsEmpty or FTheme.Text.IsEmpty);
end;

procedure TFMXCodeEditor.ItemOnChange(Sender: TObject);
begin
  if not Ready then
    Exit;

  CalcAttribute;
  CalcExpression;
  CalcSelection;
  CalcPainting;
  CalcLine;
  CalcCaret;
  CalcSize;
end;

procedure TFMXCodeEditor.CalcAttribute;
var
  oJSON: TJSONObject;
  oItem: TJSONValue;
  vItem: TJSONValue;
  Attib: TInternalAttribute;
  I: Integer;
begin
  FAttributes.Clear;
  oJSON := TJSONObject(TJSONObject.ParseJSONValue(FTheme.Text));
  try
    if not Assigned(oJSON) then
      Exit;

    for I := 0 to Pred(oJSON.Count) do
    begin
      oItem := oJSON.Pairs[I].JsonValue;

      Attib := TInternalAttribute.Create;
      Attib.ID := oJSON.Pairs[I].JsonString.Value;

      Attib.Font.Size := FFont.Size;

      if not Assigned(oItem.FindValue('font')) then
        Attib.Font.Family := FFont.Family
      else
        Attib.Font.Family := oItem.GetValue<String>('font');

      if not Assigned(oItem.FindValue('style')) then
        Attib.Font.Style := FFont.Style
      else
      begin
        Attib.Font.Style := [];
        for vItem in oItem.GetValue<TJSONArray>('style') do
          Attib.Font.Style := Attib.Font.Style + [TFontStyle(TJSONNumber(vItem).AsInt)];
      end;

      if not Assigned(oItem.FindValue('color')) then
        Attib.Color := FFontColor
      else
        Attib.Color := StrToInt('$'+ oItem.GetValue<String>('color'));

      FAttributes.Add(Attib);
    end;
  finally
    oJSON.DisposeOf;
  end;
end;

procedure TFMXCodeEditor.CalcExpression;
var
  oJSON: TJSONObject;
  I: Integer;
  Item: TMatch;
  Part: TInternalPart;
  Attib: TInternalAttribute;
  bFind: Boolean;
begin
  FParts.Clear;
  oJSON := TJSONObject(TJSONObject.ParseJSONValue(FSyntax.Text));
  try
    if not Assigned(oJSON) then
      Exit;

    for I := 0 to Pred(oJSON.Count) do
    begin
      for Item in TRegEx.Matches(FCode.Text, oJSON.Pairs[I].JsonValue.Value, []) do
      begin
        if Item.Length <= 0 then
          Continue;

        Attib := nil;
        bFind := False;
        for Attib in FAttributes do
        begin
          if Attib.ID = oJSON.Pairs[I].JsonString.Value then
          begin
            bFind := True;
            Break;
          end;
        end;
        if not bFind then
          Continue;

        Part := TInternalPart.Create;
        Part.Start := Pred(Item.Index);
        Part.Lenght := Item.Length;
        Part.Attribute := Attib;
        FParts.Add(Part);
      end;
    end;
  finally
    oJSON.DisposeOf;
  end;
end;

procedure TFMXCodeEditor.CalcSelection;
var
  Part: TInternalPart;
  Attib: TInternalAttribute;
begin
  Attib := TInternalAttribute.Create;
  Attib.Font.Size := FFont.Size;
  Attib.Font.Family := FFont.Family;
  Attib.Font.Style := FFont.Style;
  Attib.Color := TAlphaColorRec.White;
  FAttributes.Add(Attib);

  Part := TInternalPart.Create;
  Part.Start := FSelection.Start;
  Part.Lenght := FSelection.Length;
  Part.Attribute := Attib;
  FParts.Add(Part);
end;

procedure TFMXCodeEditor.CalcPainting;
var
  Parte: TInternalPart;
begin
  FLayout.BeginUpdate;
  try
    FLayout.ClearAttributes;

    FLayout.Text := FCode.Text;

    for Parte in FParts do
      FLayout.AddAttribute(Parte.AttributeRange);
  finally
    FLayout.EndUpdate;
  end;
  Repaint;
end;

procedure TFMXCodeEditor.CalcLine;
begin
  FLineCount  := Length(FLayout.Text.Split([sLineBreak]));
  FLineHeight := FLayout.TextHeight / FLineCount;
end;

procedure TFMXCodeEditor.CalcCaret;
begin
  FCaret.Resize(FLineHeight);
end;

procedure TFMXCodeEditor.CalcSize;
begin
  if FAutoSize then
  begin
    Self.Width := FLayout.TextWidth;
    Self.Height := FLayout.TextHeight;
  end;
end;

procedure TFMXCodeEditor.BackSelection;
var
  r: TRectF;
begin
  // Desenvolver desenho da seleção
  r := TRectF.Create(0, 0, 8 * FSelection.Length, FLineHeight + 1);
  r.SetLocation(FLineHeight * 5 + 1, 16);

  Canvas.Fill.Color := TAlphaColorRec.Blue;
  Canvas.FillRect(r, 1, 1, [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight], 1);
end;

procedure TFMXCodeEditor.Paint;
begin
  Canvas.BeginScene;
  try
    // Cor de fundo
    Canvas.Fill.Color := FBackColor;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 0, 0, [], 1);

    // Cor de fundo da seleção
    BackSelection;

    // Texto
    FLayout.RenderLayout(Canvas);
  finally
    Canvas.EndScene;
  end;
end;

procedure TFMXCodeEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseDown(Button, Shift, X, Y);

  CaretPos := FLayout.PositionAtPoint(TPointF.Create(X, Y));

  FIsFocused := True;
  FCaret.Show;
end;

procedure TFMXCodeEditor.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
var
  sBegin: String;
  sEnd: String;
  iPosAtual: Integer;
  iPosAnterior: Integer;
  iPosProximo: Integer;
  I: Integer;
begin
  inherited;

  sBegin := Copy(FLayout.Text, 0, CaretPos);
  sEnd   := Copy(FLayout.Text, CaretPos + 1);

  // Implementar demais teclas
  if KeyChar <> #0 then
  begin
    FCode.Text := sBegin + KeyChar + sEnd;
    CaretPos := CaretPos + 1;
  end
  else
  case Key of
    vkBack:
    begin
      if (Length(sBegin) > 2) and sBegin.EndsWith(sLineBreak) then
      begin
        FCode.Text := Copy(sBegin, 1, Length(sBegin) - 2) + sEnd;
        CaretPos := CaretPos - 2;
      end
      else
      begin
        FCode.Text := Copy(sBegin, 1, Length(sBegin) - 1) + sEnd;
        CaretPos := CaretPos - 1;
      end;
    end;
    vkDelete:
    begin
      if (Length(sEnd) > 2) and sEnd.StartsWith(sLineBreak) then
         FCode.Text := sBegin + Copy(sEnd, 3)
      else
        FCode.Text := sBegin + Copy(sEnd, 2);
    end;
    vkReturn:
    begin
      FCode.Text := sBegin + sLineBreak + sEnd;
    end;
    vkRight:
    begin
      if LeftStr(sEnd, 1) = #13 then
        CaretPos := CaretPos + 2
      else
        CaretPos := CaretPos + 1;
    end;
    vkLeft:
    begin
      if RightStr(sBegin, 1) = #10 then
        CaretPos := CaretPos - 2
      else
        CaretPos := CaretPos - 1;
    end;
    vkUp:
    begin
      iPosAtual := 0;
      for I := Length(sBegin) downto 1 do
      begin
        Inc(iPosAtual);
        if sBegin[I] = #13 then
          Break;
      end;

      iPosAnterior := 0;
      for I := Length(sBegin) - iPosAtual downto 1 do
      begin
        Inc(iPosAnterior);
        if sBegin[I] = #13 then
          Break;
      end;
      
      CaretPos := (CaretPos - iPosAtual) - (iPosAnterior - iPosAtual);
    end;
    vkDown:
    begin    
      iPosAtual := 0;
      for I := Length(sBegin) downto 1 do
      begin
        Inc(iPosAtual);
        if sBegin[I] = #13 then
          Break;
      end;

      iPosProximo := 0;
      for I := 1 to Length(sEnd) do
      begin
        Inc(iPosProximo);
        if sEnd[I] = #13 then
          Break;
      end;

      CaretPos := CaretPos + iPosProximo + iPosAtual - 1;
    end;
    vkHome:
    begin
      iPosAtual := 0;
      for I := Length(sBegin) downto 1 do
      begin
        Inc(iPosAtual);
        if sBegin[I] = #13 then
          Break;
      end;

      CaretPos := CaretPos - iPosAtual + 2;
    end;
    vkEnd:
    begin
      iPosProximo := 0;
      for I := 1 to Length(sEnd) do
      begin
        Inc(iPosProximo);
        if sEnd[I] = #13 then
          Break;
      end;

      CaretPos := CaretPos + iPosProximo - 1;
    end;
  end;
end;

procedure TFMXCodeEditor.DoEnter;
begin
  inherited DoEnter;
end;

procedure TFMXCodeEditor.DoExit;
begin
  inherited DoExit;
end;

function TFMXCodeEditor.GetFont: TFont;
begin
  Result := FFont;
end;

function TFMXCodeEditor.GetCode: TStrings;
begin
  Result := FCode;
end;

function TFMXCodeEditor.GetSyntax: TStrings;
begin
  Result := FSyntax;
end;

function TFMXCodeEditor.GetTheme: TStrings;
begin
  Result := FTheme;
end;

procedure TFMXCodeEditor.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TFMXCodeEditor.SetCaretColor(const Value: TAlphaColor);
begin
  FCaretColor := Value;
  FCaret.Color := FCaretColor;
end;

procedure TFMXCodeEditor.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
end;

procedure TFMXCodeEditor.SetSyntax(const Value: TStrings);
begin
  FSyntax.Assign(Value);
end;

procedure TFMXCodeEditor.SetTheme(const Value: TStrings);
begin
  FTheme.Assign(Value);
end;

procedure TFMXCodeEditor.SetCaretPos(const Value: Integer);
var
  sPart: String;
  aParts: TArray<String>;
  iRow: Integer;
  regPos: TRegion;
  iCaretPosX: Single;
begin
  FCaretPos := Value;

  sPart  := Copy(FLayout.Text, 0, CaretPos);
  aParts := sPart.Split([sLineBreak]);
  iRow   := Length(aParts) - 1;

  iCaretPosX := 0;
  if sPart.IsEmpty then
    iRow := LineCount - 1
  else
  begin
    regPos := FLayout.RegionForRange(TTextRange.Create(Length(sPart) - Length(aParts[iRow]), Length(aParts[iRow])));
    iCaretPosX := regPos[0].Width;
  end;

  FCaret.X := iCaretPosX;
  FCaret.Y := iRow * LineHeight
end;

procedure TFMXCodeEditor.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  CalcSize;
end;

end.
