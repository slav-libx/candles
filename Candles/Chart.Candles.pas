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
    procedure DrawCandle(Canvas: TCanvas; const Candle: TCandle; const DRect,VRect: TRectF;
      IsSelected: Boolean);
    procedure CalcRanges;
    procedure SetElapsed(Value: Int64);
    function GetElapsedMax: Int64;
    function GetElapsedMin: Int64;
    procedure SetDuration(const Value: Int64);
    function GetDurationMax: Int64;
    function GetRect: TRectF;
  public
    Data: TCandles;
    Area: TRectF;
    IndexMin: Integer;
    IndexMax: Integer;
    SelectedIndex: Integer;
    BullColor: TAlphaColor;
    BearColor: TAlphaColor;
    SelectedColor: TAlphaColor;
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
    procedure SetData(const Candles: TCandles);
    function Get(const P: TPointF): Integer;
    function GetCandleSourceRect(const Candle: TCandle; BodyOnly: Boolean): TRectF;
    function GetCandleRect(const Candle: TCandle; const ARect: TRectF; BodyOnly: Boolean=True): TRectF;
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
  SelectedIndex:=-1;
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

procedure TCandlesIndicator.DrawCandle(Canvas: TCanvas; const Candle: TCandle;
  const DRect,VRect: TRectF; IsSelected: Boolean);
var R,S: TRectF;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Dash:=TStrokeDash.Solid;
  Canvas.Stroke.Thickness:=2;
  Canvas.Stroke.Color:=claBlack;

  R:=GetCandleSourceRect(Candle,True);
  R:=EnsureRect(R,VRect,DRect);

  if R.Width>3 then R.Inflate(-1.5,0);

  S:=GetCandleSourceRect(Candle,False);
  S:=EnsureRect(S,VRect,DRect);

  if IsSelected then
  begin
    Canvas.Fill.Color:=SelectedColor;
    Canvas.FillRect(S,0,0,AllCorners,1);
  end;

  S.Left:=S.CenterPoint.X;
  S.Right:=S.Left;

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
var
  I: Integer;
  V: TRectF;
begin

  V:=GetRect;

  for I:=0 to High(Data) do
  if InRange(Data[I].Time,V.Left-Data[I].Duration,V.Right{-Data[I].Duration}) then DrawCandle(Canvas,Data[I],ARect,V,I=SelectedIndex);

end;

function TCandlesIndicator.Get(const P: TPointF): Integer;
begin
  for Result:=0 to High(Data) do
  if RectF(Data[Result].Time,Data[Result].Min,Data[Result].Time+
    Data[Result].Duration,Data[Result].Max).Contains(P) then Exit;
  Result:=-1;
end;

function TCandlesIndicator.GetRect: TRectF;
begin
  Result:=RectF(Elapsed,Area.Top,Elapsed+Duration,Area.Bottom);
end;

function TCandlesIndicator.GetCandleSourceRect(const Candle: TCandle; BodyOnly: Boolean): TRectF;
begin

  Result.Left:=Candle.Time;
  Result.Right:=Candle.Time+Candle.Duration;

  if BodyOnly then
  begin
    Result.Top:=Candle.Open;
    Result.Bottom:=Candle.Close;
  end else begin
    Result.Top:=Candle.Max;
    Result.Bottom:=Candle.Min;
  end;

end;

function TCandlesIndicator.GetCandleRect(const Candle: TCandle; const ARect: TRectF; BodyOnly: Boolean): TRectF;
begin
  Result:=EnsureRect(GetCandleSourceRect(Candle,BodyOnly),GetRect,ARect);
end;

end.
