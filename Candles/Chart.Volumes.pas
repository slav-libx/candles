unit Chart.Volumes;

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

  TVolumesIndicator = record
  private
    procedure DrawVolume(Canvas: TCanvas; const Candle: TCandle; const DRect: TRectF; VRect: TRectA);
  public
    Area: TRectA;
    Data: TCandles;
    BullColor: TAlphaColor;
    BearColor: TAlphaColor;
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
    procedure Clear;
    procedure AddCandle(const Candle: TCandle);
    procedure SetData(const Candles: TCandles);
    procedure SetRange(Elapsed,Duration: Int64);
    procedure CalcRanges;
    function Get(const P: TPointA): Integer;
    function GetVolumeSourceRect(const Candle: TCandle): TRectA;
  end;

implementation

procedure TVolumesIndicator.Clear;
begin
  Data:=nil;
end;

procedure TVolumesIndicator.AddCandle(const Candle: TCandle);
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

procedure TVolumesIndicator.SetData(const Candles: TCandles);
begin

  if Length(Data)=0 then
    Data:=Candles
  else
    for var C in Candles do AddCandle(C);

end;

procedure TVolumesIndicator.SetRange(Elapsed,Duration: Int64);
begin
  Area.TimeFrom:=Elapsed;
  Area.TimeTo:=Elapsed+Duration;
end;

procedure TVolumesIndicator.CalcRanges;
var I,Index: Integer;
begin

  Index:=-1;

  if Length(Data)=0 then Exit;

  for I:=0 to High(Data) do
  if (Index=-1) or (Data[Index].Volume<Data[I].Volume) then Index:=I;

  Area.ValueMax:=Data[Index].Volume;
  Area.ValueMin:=0;
  Area.TimeFrom:=Data[0].Time;
  Area.TimeTo:=Data[High(Data)].Time+Data[High(Data)].Duration;

end;

procedure TVolumesIndicator.DrawVolume(Canvas: TCanvas; const Candle: TCandle;
  const DRect: TRectF; VRect: TRectA);
var R: TRectA;
    CR: TRectF;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Dash:=TStrokeDash.Solid;
  Canvas.Stroke.Thickness:=2;
  Canvas.Stroke.Color:=claBlack;

  R:=GetVolumeSourceRect(Candle);
  CR:=EnsureRect(R,VRect,DRect);

  if CR.Width>2 then CR.Inflate(0,0,-1.5,0);

  if Candle.Open<Candle.Close then
    Canvas.Fill.Color:=BullColor
  else
    Canvas.Fill.Color:=BearColor;
  Canvas.Fill.Kind:=TBrushKind.Solid;

  Canvas.FillRect(CR,0,0,AllCorners,1);

end;

procedure TVolumesIndicator.DrawTo(Canvas: TCanvas; const ARect: TRectF);
begin

  for var C in Data do
  if InRange(C.Time,Area.TimeFrom-C.Duration,Area.TimeTo) then DrawVolume(Canvas,C,ARect,Area);

end;

function TVolumesIndicator.Get(const P: TPointA): Integer;
begin
  for Result:=0 to High(Data) do
  if GetVolumeSourceRect(Data[Result]).Contains(P) then Exit;
  Result:=-1;
end;

function TVolumesIndicator.GetVolumeSourceRect(const Candle: TCandle): TRectA;
begin

  Result.TimeFrom:=Candle.Time;
  Result.TimeTo:=Candle.Time+Candle.Duration;

  Result.ValueMax:=Candle.Volume;
  Result.ValueMin:=Area.ValueMin;

end;

end.
