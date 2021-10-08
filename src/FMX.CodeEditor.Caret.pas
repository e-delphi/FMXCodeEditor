// Eduardo - 13/09/2021
unit FMX.CodeEditor.Caret;

interface

uses
  FMX.Controls,
  System.UITypes,
  FMX.Types;

type
  TInternalCaret = class
  private
    FCaret: TCaret;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    function GetX: Single;
    function GetY: Single;
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
  public
    property Color: TAlphaColor read GetColor write SetColor;
    property X: Single read GetX write SetX;
    property Y: Single read GetY write SetY;
    procedure Resize(LineHeight: Single);
    procedure Show;
    constructor Create(const AOwner: TFMXObject);
    destructor Destroy; override;
  end;

implementation

uses
  System.Types;

{ TInternalCaret }

constructor TInternalCaret.Create(const AOwner: TFMXObject);
begin
  FCaret := TCaret.Create(AOwner);
  FCaret.Width   := 1;
  FCaret.Pos     := TPointF.Create(0, 0);
  FCaret.Visible := True;
end;

destructor TInternalCaret.Destroy;
begin
  FCaret.DisposeOf;
  inherited;
end;

function TInternalCaret.GetColor: TAlphaColor;
begin
  Result := FCaret.Color;
end;

procedure TInternalCaret.SetColor(const Value: TAlphaColor);
begin
  FCaret.Color := Value;
end;

function TInternalCaret.GetX: Single;
begin
  Result := FCaret.Pos.X;
end;

procedure TInternalCaret.SetX(const Value: Single);
begin
  FCaret.Pos := TPointF.Create(Value, FCaret.Pos.Y);
end;

function TInternalCaret.GetY: Single;
begin
  Result := FCaret.Pos.Y;
end;

procedure TInternalCaret.SetY(const Value: Single);
begin
  FCaret.Pos := TPointF.Create(FCaret.Pos.X, Value);
end;

procedure TInternalCaret.Resize(LineHeight: Single);
begin
  if FCaret.Size.Height <> LineHeight then
    FCaret.Size := TSizeF.Create(1, LineHeight);
end;

procedure TInternalCaret.Show;
begin
  FCaret.Show;
end;

end.
