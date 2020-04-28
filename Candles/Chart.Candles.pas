unit Chart.Candles;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.UIConsts,
  FMX.Types,
  FMX.Graphics,
  Charts.Math;

type
  TCandle = record
    Min: Extended;
    Max: Extended;
    Open: Extended;
    Close: Extended;
    Time: Int64;
    Duration: Int64;
  end;

  TCandles = array of TCandle;

  TCandlesIndicator = record
  private
    FElapsed: Int64;
    FDuration: Int64;
    FDurationMin: Int64;
    procedure DrawCandle(Canvas: TCanvas; const Candle: TCandle; const DRect,VRect: TRectF);
    procedure CalcRanges;
    procedure SetElapsed(Value: Int64);
    function GetElapsedMax: Int64;
    function GetElapsedMin: Int64;
    procedure SetDuration(const Value: Int64);
    function GetDurationMax: Int64;
  public
    Data: TCandles;
    Area: TRectF;
    IndexMin: Integer;
    IndexMax: Integer;
    BullColor: TAlphaColor;
    BearColor: TAlphaColor;
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
    procedure SetData(const Candles: TCandles);
    property Elapsed: Int64 read FElapsed write SetElapsed;
    property ElapsedMax: Int64 read GetElapsedMax;
    property ElapsedMin: Int64 read GetElapsedMin;
    property Duration: Int64 read FDuration write SetDuration;
    property DurationMax: Int64 read GetDurationMax;
    property DurationMin: Int64 read FDurationMin;
  end;

implementation

procedure TCandlesIndicator.SetData(const Candles: TCandles);
begin
  Self.Data:=Candles;
  CalcRanges;
end;

procedure TCandlesIndicator.CalcRanges;
var I: Integer;
begin

  FDurationMin:=1000;
  FDuration:=5000;

  FElapsed:=0;

  IndexMin:=-1;
  IndexMax:=-1;

  if Length(Data)=0 then Exit;

  for I:=0 to High(Data) do
  begin
    if (IndexMin=-1) or (Data[IndexMin].Min>Data[I].Min) then IndexMin:=I;
    if (IndexMax=-1) or (Data[IndexMax].Max<Data[I].Max) then IndexMax:=I;
  end;

  Area.Bottom:=Data[IndexMin].Min;
  Area.Top:=Data[IndexMax].Max;

  Area.Left:=Data[0].Time;
  Area.Right:=Data[High(Data)].Time+Data[High(Data)].Duration;

  FDurationMin:=1000;
  FDuration:=DurationMax;

  FElapsed:=Data[0].Time;

end;

procedure TCandlesIndicator.SetDuration(const Value: Int64);
begin
  FDuration:=EnsureRange(Value,DurationMin,DurationMax);
end;

procedure TCandlesIndicator.SetElapsed(Value: Int64);
begin
  FElapsed:=EnsureRange(Value,ElapsedMin,ElapsedMax);
end;

function TCandlesIndicator.GetElapsedMax: Int64;
begin
  Result:=Round(Area.Right-Duration*0.8);
end;

function TCandlesIndicator.GetElapsedMin: Int64;
begin
  Result:=Round(Area.Left-Duration*0.1);
end;

function TCandlesIndicator.GetDurationMax: Int64;
begin
  Result:=Round(Area.Width);
end;

procedure TCandlesIndicator.DrawCandle(Canvas: TCanvas; const Candle: TCandle; const DRect,VRect: TRectF);
var R,S: TRectF;
begin

  R.Left:=Candle.Time;
  R.Right:=Candle.Time+Candle.Duration;
  R.Top:=Candle.Open;
  R.Bottom:=Candle.Close;
  R:=EnsureRect(R,VRect,DRect);
  if R.Width>3 then R.Inflate(-1.5,0);

  S.Left:=Candle.Time;
  S.Right:=Candle.Time+Candle.Duration;
  S.Top:=Candle.Max;
  S.Bottom:=Candle.Min;
  S:=EnsureRect(S,VRect,DRect);

  S.Left:=S.CenterPoint.X;
  S.Right:=S.Left;

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Thickness:=2;
  Canvas.Stroke.Color:=claBlack;

  Canvas.DrawLine(S.TopLeft,S.BottomRight,1);

  if Candle.Open<Candle.Close then
    Canvas.Fill.Color:=BullColor
  else
    Canvas.Fill.Color:=BearColor;
  Canvas.Fill.Kind:=TBrushKind.Solid;

  Canvas.FillRect(R,0,0,AllCorners,1);
  Canvas.DrawRect(R,0,0,AllCorners,1);

end;

procedure TCandlesIndicator.DrawTo(Canvas: TCanvas; const ARect: TRectF);
var V: TRectF;
begin

  V:=RectF(Elapsed,Area.Top,Elapsed+Duration,Area.Bottom);

  for var C in Data do
  if InRange(C.Time,V.Left-C.Duration,V.Right-C.Duration) then DrawCandle(Canvas,C,ARect,V);

end;

end.
