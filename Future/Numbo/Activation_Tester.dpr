program Activation_Tester;

uses
  Forms,
  ActivationTester in 'ActivationTester.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
