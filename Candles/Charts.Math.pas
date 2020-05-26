unit Charts.Math;

interface

uses
  System.Types,
  System.Math;

type

  TPoint64 = record
    X,Y: Int64;
  end;

  TRect64 = record
  case Integer of
    0: (Left,Top,Right,Bottom: Int64);
    1: (TopLeft, BottomRight: TPoint64);
  end;

  TPointA = record
    Time: Int64;
    Value: Extended;
  end;

  TRectA = record
    TimeFrom: Int64;
    TimeTo: Int64;
    ValueMin: Extended;
    ValueMax: Extended;
    function Contains(const Point: TPointA): Boolean;
  end;

function EnsureValue(const AValue: Single; const ValueMin,ValueMax,EnsureMin,EnsureMax: Single): Single; overload;
function EnsureValue(const AValue: Int64; const ValueMin,ValueMax: Int64; EnsureMin,EnsureMax: Single): Single; overload;
function EnsureValue(const AValue: Single; const ValueMin,ValueMax: Single; EnsureMin,EnsureMax: Int64): Int64; overload;
function EnsureValue(const AValue: Int64; const ValueMin,ValueMax,EnsureMin,EnsureMax: Int64): Int64; overload;
function EnsureValue(const AValue: TRectF; const ValueRect,EnsureRect: TRectF): TRectF; overload;
function EnsureValue(const AValue: Single; const Values,Ensures: TSingleDynArray): Single; overload;
function EnsurePoint(const APoint: TPointF; const ValueRect,EnsureRect: TRectF): TPointF; overload;
function EnsurePoint(const APoint: TPoint64; const ValueRect: TRect64; EnsureRect: TRectF): TPointF; overload;
function EnsurePoint(const APoint: TPointA; const Area: TRectA; EnsureRect: TRectF): TPointF; overload;
function EnsurePoint(const APoint: TPointF; const ValueRect: TRectF; const EnsureRect: TRectA): TPointA; overload;
function EnsureRect(const ARect: TRectF; const ValueRect,EnsureRect: TRectF): TRectF; overload;
function EnsureRect(const ARect: TRect64; const ValueRect: TRect64; EnsureRect: TRectF): TRectF; overload;
function EnsureRect(const ARect: TRectA; const ValueArea: TRectA; EnsureRect: TRectF): TRectF; overload;
function FloatMod(const V,D: Extended): Extended;
function FloatDiv(const V,D: Extended): Extended;

function PointA(Time: Int64; Value: Extended): TPointA;
function RectA(TimeFrom,TimeTo: Int64; ValueMin,ValueMax: Extended): TRectA;

implementation

function TRectA.Contains(const Point: TPointA): Boolean;
begin
  Result:=
    InRange(Point.Time,Self.TimeFrom,Self.TimeTo) and
    InRange(Point.Value,Self.ValueMin,Self.ValueMax);
end;

function PointA(Time: Int64; Value: Extended): TPointA;
begin
  Result.Time:=Time;
  Result.Value:=Value;
end;

function RectA(TimeFrom,TimeTo: Int64; ValueMin,ValueMax: Extended): TRectA;
begin
  Result.TimeFrom:=TimeFrom;
  Result.TimeTo:=TimeTo;
  Result.ValueMin:=ValueMin;
  Result.ValueMax:=ValueMax
end;

function EnsureValue(const AValue: Single; const ValueMin,ValueMax,EnsureMin,EnsureMax: Single): Single;
begin
  if AValue=ValueMin then
    Result:=EnsureMin
  else
    Result:=(AValue-ValueMin)/(ValueMax-ValueMin)*(EnsureMax-EnsureMin)+EnsureMin;
end;

function EnsureValue(const AValue: Single; const ValueMin,ValueMax: Single; EnsureMin,EnsureMax: Int64): Int64;
begin
  if AValue=ValueMin then
    Result:=EnsureMin
  else
    Result:=Round((AValue-ValueMin)/(ValueMax-ValueMin)*(EnsureMax-EnsureMin))+EnsureMin;
end;

function EnsureValue(const AValue: Int64; const ValueMin,ValueMax: Int64; EnsureMin,EnsureMax: Single): Single;
begin
  if ValueMax=ValueMin then
    Result:=EnsureMin
  else
    Result:=(AValue-ValueMin)/(ValueMax-ValueMin)*(EnsureMax-EnsureMin)+EnsureMin;
end;

function EnsureValue(const AValue: Int64; const ValueMin,ValueMax,EnsureMin,EnsureMax: Int64): Int64;
begin
  if AValue=ValueMin then
    Result:=EnsureMin
  else
    Result:=Round((AValue-ValueMin)/(ValueMax-ValueMin)*(EnsureMax-EnsureMin)+EnsureMin);
end;

function EnsureValue(const AValue: TRectF; const ValueRect,EnsureRect: TRectF): TRectF;
begin
  Result.Left:=EnsureValue(AValue.Left,ValueRect.Left,ValueRect.Right,EnsureRect.Left,EnsureRect.Right);
  Result.Right:=EnsureValue(AValue.Right,ValueRect.Left,ValueRect.Right,EnsureRect.Left,EnsureRect.Right);
  Result.Top:=EnsureValue(AValue.Top,ValueRect.Top,ValueRect.Bottom,EnsureRect.Top,EnsureRect.Bottom);
  Result.Bottom:=EnsureValue(AValue.Bottom,ValueRect.Top,ValueRect.Bottom,EnsureRect.Top,EnsureRect.Bottom);
end;

function EnsureValue(const AValue: Single; const Values,Ensures: TSingleDynArray): Single;
var I: Integer;
begin
  Result:=AValue;
  for I:=1 to High(Values) do
  if Result<Values[I] then Exit(EnsureValue(Result,Values[I-1],Values[I],Ensures[I-1],Ensures[I]));
  Result:=Ensures[High(Ensures)];
end;

function EnsurePoint(const APoint: TPointF; const ValueRect,EnsureRect: TRectF): TPointF;
begin
  Result:=PointF(
    EnsureValue(APoint.X,ValueRect.Left,ValueRect.Right,EnsureRect.Left,EnsureRect.Right),
    EnsureValue(APoint.Y,ValueRect.Top,ValueRect.Bottom,EnsureRect.Top,EnsureRect.Bottom));
end;

function EnsurePoint(const APoint: TPoint64; const ValueRect: TRect64; EnsureRect: TRectF): TPointF;
begin
  Result:=PointF(
    EnsureValue(APoint.X,ValueRect.Left,ValueRect.Right,EnsureRect.Left,EnsureRect.Right),
    EnsureValue(APoint.Y,ValueRect.Top,ValueRect.Bottom,EnsureRect.Top,EnsureRect.Bottom));
end;

function EnsurePoint(const APoint: TPointA; const Area: TRectA; EnsureRect: TRectF): TPointF; overload;
begin
  Result:=PointF(
    EnsureValue(APoint.Time,Area.TimeFrom,Area.TimeTo,EnsureRect.Left,EnsureRect.Right),
    EnsureValue(APoint.Value,Area.ValueMin,Area.ValueMax,EnsureRect.Bottom,EnsureRect.Top));
end;

function EnsurePoint(const APoint: TPointF; const ValueRect: TRectF; const EnsureRect: TRectA): TPointA;
begin
  Result:=PointA(
    EnsureValue(APoint.X,ValueRect.Left,ValueRect.Right,EnsureRect.TimeFrom,EnsureRect.TimeTo),
    EnsureValue(APoint.Y,ValueRect.Top,ValueRect.Bottom,EnsureRect.ValueMax,EnsureRect.ValueMin));
end;

function EnsureRect(const ARect: TRectF; const ValueRect,EnsureRect: TRectF): TRectF;
begin
  Result.TopLeft:=EnsurePoint(ARect.TopLeft,ValueRect,EnsureRect);
  Result.BottomRight:=EnsurePoint(ARect.BottomRight,ValueRect,EnsureRect);
end;

function EnsureRect(const ARect: TRect64; const ValueRect: TRect64; EnsureRect: TRectF): TRectF;
begin
  Result.TopLeft:=EnsurePoint(ARect.TopLeft,ValueRect,EnsureRect);
  Result.BottomRight:=EnsurePoint(ARect.BottomRight,ValueRect,EnsureRect);
end;

function EnsureRect(const ARect: TRectA; const ValueArea: TRectA; EnsureRect: TRectF): TRectF;
begin
  Result.TopLeft:=EnsurePoint(PointA(ARect.TimeFrom,ARect.ValueMin),ValueArea,EnsureRect);
  Result.BottomRight:=EnsurePoint(PointA(ARect.TimeTo,ARect.ValueMax),ValueArea,EnsureRect);
end;

function FloatMod(const V,D: Extended): Extended;
begin
  Result:=Frac(V/D)*D;
end;

function FloatDiv(const V,D: Extended): Extended;
begin
  Result:=V-FloatMod(V,D);
end;

end.
