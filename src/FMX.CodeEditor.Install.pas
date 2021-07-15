// Eduardo - 27/06/2021
unit FMX.CodeEditor.Install;

interface

uses
  System.Classes;

procedure Register;

implementation

uses
  FMX.CodeEditor;

procedure Register;
begin
  RegisterComponents('FMXCodeEditor', [TFMXCodeEditor]);
end;

end.

