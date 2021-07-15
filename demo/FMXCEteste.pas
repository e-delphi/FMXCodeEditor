unit FMXCEteste;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.CodeEditor;

type
  TFMXCE = class(TForm)
    FMXCodeEditor1: TFMXCodeEditor;
    FMXCodeEditor2: TFMXCodeEditor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMXCE: TFMXCE;

implementation

{$R *.fmx}

end.
