unit Chart.Measures;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.UIConsts,
  FMX.Types,
  FMX.Graphics,
  Measures.Types,
  Charts.Math;

type
  TValueMeasure = record
  private
    ValueMin: Extended;
    ValueMax: Extended;
    FLast: TMeasure;
    procedure DrawRules(Canvas: TCanvas; const Measure: TMeasure; const DRect: TRectF; VRect: TRectA);
    procedure DrawLastValue(Canvas: TCanvas; const DRect: TRectF; VRect: TRectA);
  public
    Data: TMeasures;
    LastColor: TAlphaColor;
    Width: Single;
    procedure Clear;
    procedure SetData(const Measures: TMeasures);
    procedure SetRange(ValueMin,ValueMax: Extended);
    procedure SetLastValue(const CandleRect: TRectF; Value: Extended; const Text: string);
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
  end;

  TElapsesMeasure = record
  private
    FElapsed: Int64;
    FDuration: Int64;
    procedure DrawRules(Canvas: TCanvas; const Elapse: TElapse; const DRect: TRectF; VRect: TRectA);
  public
    Data: TElapses;
    Height: Single;
    procedure Clear;
    procedure SetData(const Elapses: TElapses);
    procedure SetRange(Elapsed,Duration: Int64);
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
  end;

implementation

function GetOpacity(Value,Border1,Border2,Distance: Single): Single;
begin

  if Value<Border1 then Result:=0 else
  if Value<Border1+Distance then Result:=EnsureValue(Value,Border1,Border1+Distance,Single(0),1) else
  if Value<Border2-Distance then Result:=1 else
  if Value<Border2 then Result:=EnsureValue(Value,Border2-Distance,Border2,Single(1),0) else
    Result:=0;

end;

{ TValueMeasure }

procedure TValueMeasure.Clear;
begin
  Data:=nil;
end;

procedure TValueMeasure.SetData(const Measures: TMeasures);
begin
  Data:=Measures;
end;

procedure TValueMeasure.SetRange(ValueMin,ValueMax: Extended);
begin
  Self.ValueMin:=ValueMin;
  Self.ValueMax:=ValueMax;
end;

procedure TValueMeasure.SetLastValue(const CandleRect: TRectF; Value: Extended; const Text: string);
begin
  FLast.CandleRect:=CandleRect;
  FLast.Value:=Value;
  FLast.Text:=Text;
end;

function CalcInterval(Min,Max: Extended): Extended;
begin
  Result:=Max-Min;
  Result:=Result/20;
end;

procedure TValueMeasure.DrawRules(Canvas: TCanvas; const Measure: TMeasure;
  const DRect: TRectF; VRect: TRectA);
var
  P1,P2: TPointF;
  TextRect: TRectF;
  Text: string;
  Opacity: Single;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Dash:=TStrokeDash.Solid;
  Canvas.Stroke.Thickness:=0.5;
  Canvas.Stroke.Color:=claGray;

  Canvas.Fill.Color:=claBlack;
  Canvas.Fill.Kind:=TBrushKind.Solid;
  Canvas.Font.Size:=12;
  Canvas.Font.Family:='Lucida Grande';

  Text:=Measure.Text;

  P1:=PointF(DRect.Left,Canvas.AlignToPixelVertically(EnsureValue(Measure.Value,
    VRect.ValueMax,VRect.ValueMin,DRect.Top,DRect.Bottom))+0.5);

  P2:=PointF(DRect.Right-Width,P1.Y);

  TextRect:=RectF(DRect.Right-Width+2,P1.Y-20,DRect.Right,P1.Y+20);

  Opacity:=GetOpacity(P1.Y,DRect.Top,DRect.Bottom,30);

  Canvas.DrawLine(P1,P2,Opacity);

  Canvas.FillText(TextRect,Text,False,Opacity,[],TTextAlign.Leading,TTextAlign.Center);

end;

procedure TValueMeasure.DrawLastValue(Canvas: TCanvas; const DRect: TRectF; VRect: TRectA);
var
  P1,P2: TPointF;
  TextRect: TRectF;
  Text: string;
begin

  if FLast.CandleRect.Width=0 then Exit;

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Thickness:=2;
  Canvas.Stroke.Color:=LastColor;
  Canvas.Stroke.Dash:=TStrokeDash.Dash;

  Canvas.Fill.Kind:=TBrushKind.Solid;
  Canvas.Font.Size:=12;
  Canvas.Font.Family:='Lucida Grande';

  P1:=PointF(DRect.Left{FLast.CandleRect.Right},Canvas.AlignToPixelVertically(EnsureValue(FLast.Value,
    VRect.ValueMax,VRect.ValueMin,DRect.Top,DRect.Bottom))+0.5);

  P2:=PointF(DRect.Right-Width,P1.Y);

  Canvas.DrawLine(P1,P2,1);

  TextRect:=TRectF.Create(PointF(0,0),Width,100);

  Canvas.MeasureText(TextRect,FLast.Text,False,[],TTextAlign.Leading,TTextAlign.Leading);

  TextRect.Height:=TextRect.Height+4;

  TextRect:=TRectF.Create(PointF(DRect.Right-Width,P1.Y-TextRect.Height/2),Width,TextRect.Height);

  Canvas.Fill.Color:=LastColor;

  Canvas.FillRect(TextRect,0,0,AllCorners,1);

  Canvas.Fill.Color:=claBlack;

  Canvas.FillText(TextRect,FLast.Text,False,1,[],TTextAlign.Center,TTextAlign.Center);

end;

procedure TValueMeasure.DrawTo(Canvas: TCanvas; const ARect: TRectF);
var V: TRectA;
begin

  V:=RectA(0,100,ValueMin,ValueMax);

  for var C in Data do
  if InRange(C.Value,ValueMin,ValueMax) then
    DrawRules(Canvas,C,ARect,V);

  DrawLastValue(Canvas,ARect,V);

end;

{ TElapsesMeasure }

procedure TElapsesMeasure.Clear;
begin
  Data:=nil;
end;

procedure TElapsesMeasure.SetData(const Elapses: TElapses);
begin
  Data:=Elapses;
end;

procedure TElapsesMeasure.SetRange(Elapsed,Duration: Int64);
begin
  FElapsed:=Elapsed;
  FDuration:=Duration;
end;

procedure TElapsesMeasure.DrawRules(Canvas: TCanvas; const Elapse: TElapse;
  const DRect: TRectF; VRect: TRectA);
var
  P1,P2: TPointF;
  TextRect: TRectF;
  Text: string;
  Opacity: Single;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Dash:=TStrokeDash.Solid;
  Canvas.Stroke.Thickness:=0.5;
  Canvas.Stroke.Color:=claGray;

  Canvas.Fill.Color:=claBlack;
  Canvas.Fill.Kind:=TBrushKind.Solid;
  Canvas.Font.Size:=10;
  Canvas.Font.Family:='Lucida Grande';

  Text:=Elapse.Text;

  if Text='' then Text:=Elapse.Time.ToString;

  P1:=PointF(Canvas.AlignToPixelHorizontally(EnsureValue(Elapse.Time,
    VRect.TimeFrom,VRect.TimeTo,DRect.Left,DRect.Right))+0.5,DRect.Top);

  P2:=PointF(P1.X,DRect.Bottom);

  TextRect:=RectF(P1.X-100,DRect.Bottom-100,P1.X+100,DRect.Bottom);

  Opacity:=GetOpacity(P1.X,DRect.Left,DRect.Right,20);

  Canvas.DrawLine(P1,P2-PointF(0,Height-6),Opacity);

  Canvas.FillText(TextRect,Text,False,Opacity,[],
    TTextAlign.Center,TTextAlign.Trailing);

end;

procedure TElapsesMeasure.DrawTo(Canvas: TCanvas; const ARect: TRectF);
var V: TRectA;
begin

  V:=RectA(FElapsed,FElapsed+FDuration,0,100);

  for var C in Data do
  if InRange(C.Time,V.TimeFrom,V.TimeTo) then
  DrawRules(Canvas,C,ARect,V);

end;

end.
