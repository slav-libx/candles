unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Classes,
  System.Variants, System.Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  Candles.Types,
  Charts.Math,
  Charts.Controls,
  Chart.Candles,
  Chart.Measures,
  Frame.CandleSummary;

type

  TForm1 = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Layout1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    Chart: TCandleChart;
    Candle: TCandle;
    Summary: TCandleSummaryFrame;
    function GenerateCandles: TCandles;
    procedure CreateData;
    procedure OnCandleSelect(Sender: TObject; const Candle: TCandle);
    procedure OnCandleDeselect(Sender: TObject);
    procedure OnSummaryClick(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

function RandomValue(FromValue,Range1,Range2: Extended): Extended;
begin
  Result:=EnsureValue(Random,0,1,FromValue+Range1,FromValue+Range2);
end;

function TForm1.GenerateCandles: TCandles;
var
  D: Integer;
begin

  Randomize;

  Result:=nil;

  D:=1000; // duration: 1 sec

  // start candle

  Candle.Close:=0.002;
  Candle.Time:=-D;

//  C.Time:=1000000;
//  Area:=RectF(0,0.003,C.Time+300000,0); // times: 0-100 sec. volume: 0-0.003

  while Candle.Time<1000*100 do
  begin

    Candle.Title:='';
    Candle.DateTime:=Now;
    Candle.Open:=Candle.Close;
    Candle.Close:=RandomValue(Candle.Open,-0.0004,0.0003);
    Candle.Max:=RandomValue(Max(Candle.Open,Candle.Close),0,0.0002);
    Candle.Min:=RandomValue(Min(Candle.Open,Candle.Close),0,-0.0002);
    Candle.Volume:=RandomValue(0.1,-0.02,+0.02);
    Candle.Time:=Candle.Time+D;
    Candle.Duration:=D;

    Result:=Result+[Candle];

  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  Summary:=TCandleSummaryFrame.Create(Self);
  Summary.OnClick:=OnSummaryClick;

  Chart:=TCandleChart.Create(Self);
  Chart.Align:=TAlignLayout.Client;
  Chart.Parent:=Layout1;
  Chart.OnCandleSelect:=OnCandleSelect;
  Chart.OnCandleDeselect:=OnCandleDeselect;

  CreateData;

end;

procedure TForm1.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  {$IFDEF DEBUG}
  Canvas.DrawDashRect(ARect,0,0,AllCorners,1,claBlueviolet);
  {$ENDIF}
end;

procedure TForm1.Button2Click(Sender: TObject);
begin

  Candle.Title:='';
  Candle.DateTime:=Now;
  Candle.Close:=RandomValue(Candle.Open,-0.0004,0.0003);
  Candle.Max:=RandomValue(Max(Candle.Open,Candle.Close),0,0.0002);
  Candle.Min:=RandomValue(Min(Candle.Open,Candle.Close),0,-0.0002);
  Candle.Volume:=RandomValue(0.1,-0.02,+0.02);

  Chart.SetCandlesData([Candle]);

  Chart.AutoCalcRanges;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Chart.ClearData;
end;

procedure TForm1.CreateData;
var
  Candles: TCandles;
  Measures: TMeasures;
  Measure: TMeasure;
  Elapses: TElapses;
  Elapse: TElapse;
begin

  Candles:=GenerateCandles;

  Measure.Value:=0;
  Measure.Text:='0.0';
  Measures:=[Measure];

  Measure.Value:=0.001;
  Measure.Text:='0.001';
  Measures:=Measures+[Measure];

  if Length(Candles)>0 then
  begin

  Elapse.Value:=Candles[4].Time;
  Elapse.Text:='24 апреля';
  Elapses:=[Elapse];

  Elapse.Value:=Candles[20].Time;
  Elapse.Text:='25 апреля';
  Elapses:=Elapses+[Elapse];

  end;

  Chart.SetCandlesData(Candles);
  Chart.SetValueMeasure(Measures);
  Chart.SetElapsesMeasure(Elapses);

  Chart.AutoCalcRanges;
  Chart.SetCandlesTimes(Candles[0].Time,Candles[High(Candles)].Time);

  Chart.Duration:=60000;
//  Chart.Elapsed:=60000;

end;

procedure TForm1.OnCandleSelect(Sender: TObject; const Candle: TCandle);
begin
  Summary.SetValues(Candle.Title,Candle.DateTime,Candle.Open,Candle.Close,Candle.Max,Candle.Min,Candle.Volume);
  Summary.Show(Chart,Chart.GetCandleRect(Candle,False));
end;

procedure TForm1.OnCandleDeselect(Sender: TObject);
begin
  Summary.Hide;
end;

procedure TForm1.OnSummaryClick(Sender: TObject);
begin
  Chart.Deselect;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateData;
  Chart.Repaint;
end;

end.
