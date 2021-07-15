// Eduardo - 27/06/2021
unit FMX.CodeEditor.Attribute;

interface

uses
  System.UITypes,
  FMX.TextLayout,
  FMX.Graphics;

type
  TInternalAttribute = class
  strict private
    FID: String;
    FFont: TFont;
    FColor: TAlphaColor;
  public
    property ID: String read FID write FID;
    property Font: TFont read FFont;
    property Color: TAlphaColor read FColor write FColor;
    function Attribute: TTextAttribute;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TInternalAttribute }

constructor TInternalAttribute.Create;
begin
  FFont := TFont.Create;
  FColor := TAlphaColorRec.Black;
end;

function TInternalAttribute.Attribute: TTextAttribute;
begin
  Result := TTextAttribute.Create(Font, Color);
end;

destructor TInternalAttribute.Destroy;
begin
  FFont.DisposeOf;
  inherited;
end;

end.
