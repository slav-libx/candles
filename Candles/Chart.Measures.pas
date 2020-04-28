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
  Charts.Math;

type
  TMeasure = record
    Value: Extended;
    Text: string;
  end;

  TMeasures = array of TMeasure;

  TValueMeasure = record
  private
    FRangeFrom: Extended;
    FRangeTo: Extended;
    procedure DrawRules(Canvas: TCanvas; const Measure: TMeasure; const DRect,VRect: TRectF);
  public
    Data: TMeasures;
    procedure SetData(const Measures: TMeasures);
    procedure SetRange(RangeFrom,RangeTo: Extended);
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
  end;

  TElapse = record
    Value: Extended;
    Text: string;
  end;

  TElapses = array of TElapse;

  TElapsesMeasure = record
  private
    FElapsed: Int64;
    FDuration: Int64;
    procedure DrawRules(Canvas: TCanvas; const Elapse: TElapse; const DRect,VRect: TRectF);
  public
    Data: TElapses;
    procedure SetData(const Elapses: TElapses);
    procedure SetInterval(Elapsed,Duration: Int64);
    procedure DrawTo(Canvas: TCanvas; const ARect: TRectF);
  end;

implementation

procedure TValueMeasure.SetData(const Measures: TMeasures);
begin
  Data:=Measures;
end;

procedure TValueMeasure.SetRange(RangeFrom,RangeTo: Extended);
begin
  FRangeFrom:=RangeFrom;
  FRangeTo:=RangeTo;
end;

function CalcInterval(Min,Max: Extended): Extended;
begin
  Result:=Max-Min;
  Result:=Result/20;
end;

procedure TValueMeasure.DrawRules(Canvas: TCanvas; const Measure: TMeasure; const DRect,VRect: TRectF);
var
  P1,P2: TPointF;
  TextRect: TRectF;
  Text: string;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Thickness:=0.5;
  Canvas.Stroke.Color:=claGray;

  Canvas.Fill.Color:=claBlack;
  Canvas.Fill.Kind:=TBrushKind.Solid;
  Canvas.Font.Size:=12;
  Canvas.Font.Family:='Lucida Grande';

  Text:=Measure.Text;

  if Text='' then Text:=FormatFloat('0.0######',Measure.Value);

  P1:=PointF(DRect.Left,Canvas.AlignToPixelVertically(EnsureValue(Measure.Value,
    VRect.Bottom,VRect.Top,DRect.Top,DRect.Bottom))+0.5);

  P2:=PointF(DRect.Right,P1.Y);

  TextRect:=RectF(DRect.Left,P1.Y,DRect.Right,P1.Y+100);

  Canvas.DrawLine(P1,P2,1);

  Canvas.FillText(TextRect,Text,False,1,[],TTextAlign.Trailing,TTextAlign.Leading);


//  VInterval:=CalcInterval(VRect.Bottom,VRect.Top);
//  DInterval:=EnsureValue(VRect.Bottom+VInterval,VRect.Bottom,VRect.Top,DRect.Top,DRect.Bottom);
//
//  V:=FloatDiv(VRect.Bottom,VInterval);
//  Y:=EnsureValue(V,VRect.Bottom,VRect.Top,DRect.Bottom,DRect.Top);
//
//  while Y>DRect.Top do
//  begin
//
//    P:=PointF(DRect.Left,Canvas.AlignToPixelVertically(Y)+0.5);
//
//    TextRect:=RectF(DRect.Left,P.Y,DRect.Right,P.Y+100);
//
//    Canvas.DrawLine(P,PointF(DRect.Right,P.Y),1);
//    Canvas.FillText(TextRect,FormatFloat('0.0######',V),False,1,[],TTextAlign.Trailing,TTextAlign.Leading);
//
//    Y:=Y-DInterval;
//    V:=V+VInterval;
//
//  end;

end;

procedure TValueMeasure.DrawTo(Canvas: TCanvas; const ARect: TRectF);
var V: TRectF;
begin

  V:=RectF(0,FRangeFrom,100,FRangeTo);

  for var C in Data do
  if InRange(C.Value,FRangeFrom,FRangeTo) then
  DrawRules(Canvas,C,ARect,V);

end;

{ TElapsesMeasure }

procedure TElapsesMeasure.SetData(const Elapses: TElapses);
begin
  Data:=Elapses;
end;

procedure TElapsesMeasure.SetInterval(Elapsed,Duration: Int64);
begin
  FElapsed:=Elapsed;
  FDuration:=Duration;
end;

procedure TElapsesMeasure.DrawRules(Canvas: TCanvas; const Elapse: TElapse; const DRect,VRect: TRectF);
var
  P1,P2: TPointF;
  TextRect: TRectF;
  Text: string;
begin

  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Thickness:=0.5;
  Canvas.Stroke.Color:=claGray;

  Canvas.Fill.Color:=claBlack;
  Canvas.Fill.Kind:=TBrushKind.Solid;
  Canvas.Font.Size:=12;
  Canvas.Font.Family:='Lucida Grande';

  Text:=Elapse.Text;

  if Text='' then Text:=Elapse.Value.ToString;

  P1:=PointF(Canvas.AlignToPixelHorizontally(EnsureValue(Elapse.Value,
    VRect.Left,VRect.Right,DRect.Left,DRect.Right))+0.5,DRect.Top);

  P2:=PointF(P1.X,DRect.Bottom);

  TextRect:=RectF(P1.X-100,DRect.Bottom-100,P1.X+100,DRect.Bottom);

  Canvas.DrawLine(P1,P2,1);

  Canvas.FillText(TextRect,Text,False,1,[],TTextAlign.Center,TTextAlign.Trailing);

end;

procedure TElapsesMeasure.DrawTo(Canvas: TCanvas; const ARect: TRectF);
var V: TRectF;
begin

  V:=RectF(FElapsed,100,FElapsed+FDuration,0);

  for var C in Data do
  if InRange(C.Value,V.Left,V.Right) then
  DrawRules(Canvas,C,ARect,V);

end;

end.
