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

function EnsureValue(const AValue: Single; const ValueMin,ValueMax,EnsureMin,EnsureMax: Single): Single; overload;
function EnsureValue(const AValue: Int64; const ValueMin,ValueMax: Int64; EnsureMin,EnsureMax: Single): Single; overload;
function EnsureValue(const AValue: Single; const ValueMin,ValueMax: Single; EnsureMin,EnsureMax: Int64): Int64; overload;
function EnsureValue(const AValue: Int64; const ValueMin,ValueMax,EnsureMin,EnsureMax: Int64): Int64; overload;
function EnsureValue(const AValue: TRectF; const ValueRect,EnsureRect: TRectF): TRectF; overload;
function EnsureValue(const AValue: Single; const Values,Ensures: TSingleDynArray): Single; overload;
function EnsurePoint(const APoint: TPointF; const ValueRect,EnsureRect: TRectF): TPointF; overload;
function EnsurePoint(const APoint: TPoint64; const ValueRect: TRect64; EnsureRect: TRectF): TPointF; overload;
function EnsureRect(const ARect: TRectF; const ValueRect,EnsureRect: TRectF): TRectF; overload;
function EnsureRect(const ARect: TRect64; const ValueRect: TRect64; EnsureRect: TRectF): TRectF; overload;
function FloatMod(const V,D: Extended): Extended;
function FloatDiv(const V,D: Extended): Extended;

implementation

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

function FloatMod(const V,D: Extended): Extended;
begin
  Result:=Frac(V/D)*D;
end;

function FloatDiv(const V,D: Extended): Extended;
begin
  Result:=V-FloatMod(V,D);
end;

end.
