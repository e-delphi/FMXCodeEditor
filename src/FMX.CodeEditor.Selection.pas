// Eduardo - 13/09/2021
unit FMX.CodeEditor.Selection;

interface

type
  TInternalSelection = class
  private
    FStart: Integer;
    FLength: Integer;
  public
    property Start: Integer read FStart write FStart;
    property Length: Integer read FLength write FLength;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TInternalSelection }

constructor TInternalSelection.Create;
begin

end;

destructor TInternalSelection.Destroy;
begin

  inherited;
end;

end.
