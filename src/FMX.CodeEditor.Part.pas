// Eduardo - 27/06/2021
unit FMX.CodeEditor.Part;

interface

uses
  FMX.TextLayout,
  FMX.CodeEditor.Attribute;

type
  TInternalPart = class
  private
    FStart: Integer;
    FLenght: Integer;
    FAttribute: TInternalAttribute;
  public
    property Start: Integer read FStart write FStart;
    property Lenght: Integer read FLenght write FLenght;
    property Attribute: TInternalAttribute read FAttribute write FAttribute;
    function AttributeRange: TTextAttributedRange;
  end;

implementation

{ TInternalPart }

function TInternalPart.AttributeRange: TTextAttributedRange;
begin
  Result := TTextAttributedRange.Create(TTextRange.Create(Start, Lenght), Attribute.Attribute);
end;

end.
