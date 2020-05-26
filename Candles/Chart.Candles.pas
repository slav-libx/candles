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
  Charts.Math,
  Candles.Types;

type
  TCandlesIndicator = record
  private
    FElapsed: Int64;
    FDuration: Int64;
    FDurationMin: Int64;
    procedure DrawCandle(Canvas: TCanvas; const Candle: TCandle; const DRect: TRectF; Area: TRectA;
      IsSelected: Boolean);
    procedure SetElapsed(Value: Int64);
    function GetElapsedMax: Int64;
    function GetElapsedMin: Int64;
    procedure SetDuration(const Value: Int64);
    function GetDurationMax: Int64;
    function GetDurationMin: Int64;
    function GetRect: TRectA;
  public
    Data: TCandles;
    Area: TRectA;
    SelectedIndex: Integer;
    BullColor: TAlphaColor;
    BearColor: TAlphaColor;
    SelectedColor: TAlphaColor;
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
    procedure Clear;
    procedure AddCandle(const Candle: TCandle);
    procedure SetData(const Candles: TCandles);
    procedure CalcRanges;
    function Get(const P: TPointA): Integer;
    function GetCandleSourceRect(const Candle: TCandle; BodyOnly: Boolean): TRectA;
    function GetCandleRect(const Candle: TCandle; const ARect: TRectF; BodyOnly: Boolean=True): TRectF;
    property Elapsed: Int64 read FElapsed write SetElapsed;
    property ElapsedMax: Int64 read GetElapsedMax;
    property ElapsedMin: Int64 read GetElapsedMin;
    property Duration: Int64 read FDuration write SetDuration;
    property DurationMax: Int64 read GetDurationMax;
    property DurationMin: Int64 read GetDurationMin;
  end;

implementation

procedure TCandlesIndicator.Clear;
begin
  Data:=nil;
end;

procedure TCandlesIndicator.AddCandle(const Candle: TCandle);
var I: Integer;
begin

  for I:=High(Data) downto 0 do
  if Data[I].Time<=Candle.Time then
  begin

    if Data[I].Time=Candle.Time then
      Data[I]:=Candle
    else
      Insert(Candle,Data,I+1);

    Exit;

  end;

  Data:=Data+[Candle];

end;

procedure TCandlesIndicator.SetData(const Candles: TCandles);
begin

  if Length(Data)=0 then
    Data:=Candles
  else
    for var C in Candles do AddCandle(C);

end;

procedure TCandlesIndicator.CalcRanges;
var I,IndexMin,IndexMax: Integer; M: Extended;
begin

  if Length(Data)=0 then Exit;

  IndexMin:=-1;
  IndexMax:=-1;

  for I:=0 to High(Data) do
  begin
    if (IndexMin=-1) or (Data[IndexMin].Min>Data[I].Min) then IndexMin:=I;
    if (IndexMax=-1) or (Data[IndexMax].Max<Data[I].Max) then IndexMax:=I;
  end;

  if Length(Data)>4 then
    M:=(Data[IndexMax].Max-Data[IndexMin].Min)/7
  else
    M:=(Data[IndexMax].Max-Data[IndexMin].Min)/3;

  if M=0 then M:=0.1;

  Area.ValueMax:=Data[IndexMax].Max+M;
  Area.ValueMin:=Data[IndexMin].Min-M;

//  Area.Left:=Data[0].Time;
//  Area.Right:=Data[High(Data)].Time+Data[High(Data)].Duration;

//  FDurationMin:=1000;
//  FDuration:=DurationMax;

//  FElapsed:=Data[0].Time;

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
  Result:=Area.TimeTo-Duration;
end;

function TCandlesIndicator.GetElapsedMin: Int64;
begin
  Result:=Area.TimeFrom;//-Duration*0.1);
end;

function TCandlesIndicator.GetDurationMax: Int64;
begin
  Result:=Area.TimeTo-Area.TimeFrom;
end;

function TCandlesIndicator.GetDurationMin: Int64;
begin
  if Length(Data)>0 then
    Result:=Data[0].Duration
  else
    Result:=1000;
end;

procedure DrawRect(Canvas: TCanvas; ARect: TRectF);
begin

  ARect.Height:=Max(1,ARect.Height);

  Canvas.FillRect(ARect,0,0,AllCorners,1);

  ARect.Inflate(-Canvas.Stroke.Thickness/2,0);

  ARect.Top:=Canvas.AlignToPixelVertically(ARect.Top);
  ARect.Bottom:=Canvas.AlignToPixelVertically(ARect.Bottom);

  Canvas.DrawRect(ARect,0,0,AllCorners,1);

end;

procedure TCandlesIndicator.DrawCandle(Canvas: TCanvas; const Candle: TCandle;
  const DRect: TRectF; Area: TRectA; IsSelected: Boolean);
var
  CR: TRectF;
  CS: TRectF;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Dash:=TStrokeDash.Solid;
  Canvas.Stroke.Color:=claBlack;
  Canvas.Stroke.Cap:=TStrokeCap.Flat;
  Canvas.Stroke.Join:=TStrokeJoin.Miter;

  Canvas.Fill.Kind:=TBrushKind.Solid;

  CR:=GetCandleRect(Candle,DRect,True);

  CS:=GetCandleRect(Candle,DRect,False);

  CR.Top:=Canvas.AlignToPixelVertically(CR.Top);
  CR.Bottom:=Canvas.AlignToPixelVertically(CR.Bottom);

  CS.Top:=Canvas.AlignToPixelVertically(CS.Top);
  CS.Bottom:=Canvas.AlignToPixelVertically(CS.Bottom);

  if IsSelected then
  begin
    Canvas.Fill.Color:=SelectedColor;
    Canvas.FillRect(CS,0,0,AllCorners,1);
  end;

  CS.Left:=CS.CenterPoint.X;
  CS.Right:=CS.Left;

  Canvas.Stroke.Thickness:=2;

  {$IFDEF MSWINDOWS}

  CS.Inflate(0,-Canvas.Stroke.Thickness/2);

  {$ENDIF}

  if CS.Height>1 then
    Canvas.DrawLine(CS.TopLeft,CS.BottomRight,1);

  if Candle.Open<Candle.Close then
    Canvas.Fill.Color:=BullColor
  else
    Canvas.Fill.Color:=BearColor;

  Canvas.Stroke.Thickness:=1;

  DrawRect(Canvas,CR);

end;

procedure TCandlesIndicator.DrawTo(Canvas: TCanvas; const ARect: TRectF);
var
  I: Integer;
  V: TRectA;
begin

  V:=GetRect;

  for I:=0 to High(Data) do
  if InRange(Data[I].Time,V.TimeFrom-Data[I].Duration,V.TimeTo) then DrawCandle(Canvas,Data[I],ARect,V,I=SelectedIndex);

end;

function TCandlesIndicator.Get(const P: TPointA): Integer;
begin
  for Result:=0 to High(Data) do
  if GetCandleSourceRect(Data[Result],False).Contains(P) then Exit;
  Result:=-1;
end;

function TCandlesIndicator.GetRect: TRectA;
begin
  Result.TimeFrom:=Elapsed;
  Result.TimeTo:=Elapsed+Duration;
  Result.ValueMin:=Area.ValueMin;
  Result.ValueMax:=Area.ValueMax;
end;

function TCandlesIndicator.GetCandleSourceRect(const Candle: TCandle; BodyOnly: Boolean): TRectA;
begin

  Result.TimeFrom:=Candle.Time;
  Result.TimeTo:=Candle.Time+Candle.Duration;

  if BodyOnly then
  begin
    Result.ValueMax:=Max(Candle.Open,Candle.Close);
    Result.ValueMin:=Min(Candle.Open,Candle.Close);
  end else begin
    Result.ValueMax:=Candle.Max;
    Result.ValueMin:=Candle.Min;
  end;

end;

function TCandlesIndicator.GetCandleRect(const Candle: TCandle; const ARect: TRectF; BodyOnly: Boolean): TRectF;
begin
  Result:=EnsureRect(GetCandleSourceRect(Candle,BodyOnly),GetRect,ARect);
  Result.NormalizeRect;
  if Result.Width>2 then Result.Inflate(0,0,-1.5,0) else Result.Inflate(0,0,0,0);
end;

end.
