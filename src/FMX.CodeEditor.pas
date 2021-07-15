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
  FMX.CodeEditor.Attribute;

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
    FCaret: TCaret;
    FAttributes: TObjectList<TInternalAttribute>;
    FParts: TObjectList<TInternalPart>;
    FCaretPos: Integer;
    FCode: TStrings;
    FSyntax: TStrings;
    FTheme: TStrings;
    FKeys: String;
    property CaretPos: Integer read FCaretPos write SetCaretPos;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  private
    procedure CalcExpression;
    procedure CalcAttribute;
    procedure CalcPainting;
    procedure CalcCaret;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font: TFont read GetFont write SetFont stored True;
    property FontColor: TAlphaColor read FFontColor write FFontColor stored True;
    property BackColor: TAlphaColor read FBackColor write FBackColor stored True;
    property CaretColor: TAlphaColor read FCaretColor write SetCaretColor stored True;
    property Code: TStrings read GetCode write SetCode stored True;
    property Syntax: TStrings read GetSyntax write SetSyntax stored True;
    property Theme: TStrings read GetTheme write SetTheme stored True;
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
  System.RegularExpressions;

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

  FCaret         := TCaret.Create(Self);
  FCaret.Width   := 1;
  FCaret.Pos     := TPointF.Create(0, 0);
  FCaret.Visible := True;

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
end;

destructor TFMXCodeEditor.Destroy;
begin
  FLayout.DisposeOf;
  FAttributes.DisposeOf;
  FParts.DisposeOf;
  FFont.DisposeOf;
  FCode.DisposeOf;
  FSyntax.DisposeOf;
  FTheme.DisposeOf;
  FCaret.DisposeOf;
  inherited;
end;

function TFMXCodeEditor.Ready: Boolean;
begin
  Result := Assigned(FFont) and Assigned(FCode) and
            Assigned(FSyntax) and Assigned(FTheme) and
            not (FCode.Text.IsEmpty or FSyntax.Text.IsEmpty or FTheme.Text.IsEmpty);
end;

procedure TFMXCodeEditor.ItemOnChange(Sender: TObject);
begin
  if not Ready then
    Exit;

  CalcExpression;
  CalcAttribute;
  CalcPainting;
  CalcCaret;
end;

procedure TFMXCodeEditor.CalcExpression;
var
  oJSON: TJSONObject;
  I: Integer;
  Item: TMatch;
  oParte: TJSONObject;
  aItems: TJSONArray;
begin
  oJSON := TJSONObject(TJSONObject.ParseJSONValue(FSyntax.Text));
  try
    if not Assigned(oJSON) then
      Exit;

    oParte := TJSONObject.Create;
    try
      for I := 0 to Pred(oJSON.Count) do
      begin
        aItems := TJSONArray.Create;
        oParte.AddPair(oJSON.Pairs[I].JsonString.Value, aItems);

        for Item in TRegEx.Matches(FCode.Text, oJSON.Pairs[I].JsonValue.Value, []) do
          if Item.Length > 0 then
            aItems
              .Add(Pred(Item.Index))
              .Add(Item.Length);
      end;
      FKeys := oParte.Format;
    finally
      oParte.DisposeOf;
    end;
  finally
    oJSON.DisposeOf;
  end;
end;

procedure TFMXCodeEditor.CalcAttribute;
var
  oJSON: TJSONObject;
  oItem: TJSONValue;
  vItem: TJSONValue;
  aItem: TJSONArray;
  Attib: TInternalAttribute;
  bFind: Boolean;
  Part: TInternalPart;
  I, J: Integer;
begin
  FAttributes.Clear;
  FParts.Clear;
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

  oJSON := TJSONObject(TJSONObject.ParseJSONValue(FKeys));
  try
    if not Assigned(oJSON) then
      Exit;

    Part := nil;
    for I := 0 to Pred(oJSON.Count) do
    begin
      aItem := TJSONArray(oJSON.Pairs[I].JsonValue);

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

      for J := 0 to Pred(aItem.Count) do
      begin
        if not Odd(J) then
        begin
          Part := TInternalPart.Create;
          Part.Start := TJSONNumber(aItem.Items[J]).AsInt;
        end
        else
        begin
          Part.Lenght := TJSONNumber(aItem.Items[J]).AsInt;
          Part.Attribute := Attib;
          FParts.Add(Part)
        end;
      end;
    end;
  finally
    oJSON.DisposeOf;
  end;
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

procedure TFMXCodeEditor.CalcCaret;
var
  H: Single;
begin
  H := FLayout.TextHeight / Length(FLayout.Text.Split([sLineBreak]));
  H := H + (H * 0.2);
  FCaret.Size := TSizeF.Create(1, H);
end;

procedure TFMXCodeEditor.Paint;
begin
  Canvas.BeginScene;
  try
    // Cor de fundo
    Canvas.Fill.Color := FBackColor;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 0, 0, [], 1);

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
    vkRight: CaretPos := CaretPos + 1;
    vkLeft: CaretPos := CaretPos - 1;
    vkBack:
    begin
      FCode.Text := Copy(sBegin, 1, Length(sBegin) - 1) + sEnd;
      CaretPos := CaretPos - 1;
    end;
    vkReturn:
    begin
      FCode.Text := sBegin + sLineBreak + sEnd;
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
  iLineCount: Integer;
  iLineHeight: Single;
  sPart: String;
  aParts: TArray<String>;
  iRow: Integer;
  regPos: TRegion;
  iCaretPosX: Single;
begin
  FCaretPos := Value;

  iLineCount  := Length(FLayout.Text.Split([sLineBreak]));
  iLineHeight := FLayout.TextHeight / iLineCount;

  sPart  := Copy(FLayout.Text, 0, CaretPos);
  aParts := sPart.Split([sLineBreak]);
  iRow   := Length(aParts) - 1;

  iCaretPosX := 0;
  if sPart.IsEmpty then
    iRow := iLineCount - 1
  else
  begin
    regPos := FLayout.RegionForRange(TTextRange.Create(Length(sPart) - Length(aParts[iRow]), Length(aParts[iRow])));
    iCaretPosX := regPos[0].Width;
  end;

  FCaret.Pos := TPointF.Create(iCaretPosX, iRow * iLineHeight);
end;

end.
