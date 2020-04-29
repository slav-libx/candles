program candles;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Charts.Math in 'Candles\Charts.Math.pas',
  Charts.Controls in 'Candles\Charts.Controls.pas',
  Chart.Candles in 'Candles\Chart.Candles.pas',
  Chart.Measures in 'Candles\Chart.Measures.pas',
  Frame.CandleSummary in 'Frame.CandleSummary.pas' {CandleSummaryFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
