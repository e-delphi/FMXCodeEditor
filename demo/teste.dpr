program teste;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXCEteste in 'FMXCEteste.pas' {FMXCE};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TFMXCE, FMXCE);
  Application.Run;
end.
