program ChunkProject1;

{%TogetherDiagram 'ModelSupport_ChunkProject1\default.txaPackage'}
{%ToDo 'ChunkProject1.todo'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\ExternalMemoryClass\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\ChunkTheoryUnit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\ChunkProject1\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\ChunkProject\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\NumboConnotations\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\NumboConnotations\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\ChunkProject1\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ChunkProject1\ChunkProject\default.txvpck'}



uses                                        
  Forms,
  ChunkProject in 'ChunkProject.pas' {Form1},                 
  FARG_Framework_Chunk in 'FARG_Framework_Chunk.pas',            
  ExternalMemoryClass in 'ExternalMemoryClass.pas',                   
  NumboConnotations in 'NumboConnotations.pas',
  FrameworkConnotationInfo in 'FrameworkConnotationInfo.pas',
  NumboSlipnet1Unit in 'NumboSlipnet1Unit.pas',
  Node1Unit in 'Node1Unit.pas',
  Activation1Unit in 'Activation1Unit.pas';

{R$*.res}




begin
  Application.Initialize;
  Application.Title := 'Human Intuition Framework';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.