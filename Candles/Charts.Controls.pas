unit Charts.Controls;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  System.Math,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  Charts.Math,
  Chart.Candles,
  Chart.Measures;

type
  TOnCandleClick = procedure (Sender: TObject; const Candle: TCandle) of object;

  TCandleChart = class(TControl)
  private
    Candles: TCandlesIndicator;
    ValueMeasure: TValueMeasure;
    ElapsesMeasure: TElapsesMeasure;
//    Area: TRectF;
//    Candles: TCandles;
//    IndexMin: Integer;
//    IndexMax: Integer;
//    FElapsed: Int64;
//    FDuration: Int64;
//    DurationMin: Int64;
    FElapsedDownX: Int64;
    FZoomDuration: Int64;
    FZoomElapsed: Int64;
    FOnCandleClick: TOnCandleClick;
    procedure DoCandleClick;
//    procedure CalcRanges;
//    function GetElapsedMax: Int64;
//    function GetElapsedMin: Int64;
//    function GetDurationMax: Int64;
    procedure SetElapsed(Value: Int64);
    procedure SetDuration(const Value: Int64);
    procedure SetBullColor(const Value: TAlphaColor);
    procedure SetBearColor(const Value: TAlphaColor);
    //procedure DrawCandle(Canvas: TCanvas; const Candle: TCandle; const DRect,VRect: TRectF);
//    procedure DrawRules(Canvas: TCanvas; const DRect,VRect: TRectF);
//    procedure CalcRanges;
    function GetDuration: Int64;
//    function GetDurationMax: Int64;
    function GetElapsed: Int64;
//    function GetElapsedMax: Int64;
//    function GetElapsedMin: Int64;
    function GetCandlesRect: TRectF;
  protected
    FMouseMoved: Boolean;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure Click; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetData(const Candles: TCandles);
    procedure SetValueMeasure(const Measures: TMeasures);
    procedure SetElapsesMeasure(const Elapses: TElapses);
    property Elapsed: Int64 read GetElapsed write SetElapsed;
    property Duration: Int64 read GetDuration write SetDuration;
    property BullColor: TAlphaColor read Candles.BullColor write SetBullColor;
    property BearColor: TAlphaColor read Candles.BearColor write SetBearColor;
    property OnCandleClick: TOnCandleClick read FOnCandleClick write FOnCandleClick;
  end;

implementation

constructor TCandleChart.Create(AOwner: TComponent);
begin
  inherited;

  HitTest:=True;
  AutoCapture:=True;

  Touch.InteractiveGestures:=Touch.InteractiveGestures+[TInteractiveGesture.Zoom];

  Candles.BullColor:=claGreen;
  Candles.BearColor:=claRed;

end;

destructor TCandleChart.Destroy;
begin
  inherited;
end;

procedure TCandleChart.SetBullColor(const Value: TAlphaColor);
begin
  Candles.BullColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetBearColor(const Value: TAlphaColor);
begin
  Candles.BearColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetData(const Candles: TCandles);
begin
  Self.Candles.SetData(Candles);
  ValueMeasure.SetRange(Self.Candles.Area.Bottom,Self.Candles.Area.Top);
  Repaint;
end;

procedure TCandleChart.SetValueMeasure(const Measures: TMeasures);
begin
  ValueMeasure.SetData(Measures);
end;

procedure TCandleChart.SetElapsesMeasure(const Elapses: TElapses);
begin
  ElapsesMeasure.SetData(Elapses);
end;

procedure TCandleChart.SetDuration(const Value: Int64);
begin
  Candles.Duration:=Value;
  Repaint;
end;

procedure TCandleChart.SetElapsed(Value: Int64);
begin
  Candles.Elapsed:=Value;
  Repaint;
end;

function TCandleChart.GetCandlesRect: TRectF;
begin
  Result:=LocalRect;
  Result.Inflate(-10,-20,-60,-30);
end;

procedure TCandleChart.DoCandleClick;
var
  P: TPointF;
  R: TRectF;
begin
  if Assigned(FOnCandleClick) then
  begin
    P:=EnsurePoint(PressedPosition,GetCandlesRect,RectF(Elapsed,Candles.Area.Top,Elapsed+Duration,Candles.Area.Bottom));
    for var C in Candles.Data do
    begin
      R:=RectF(C.Time,C.Min,C.Time+C.Duration,C.Max);
      if R.Contains(P) then
      begin
        FOnCandleClick(Self,C);
        Exit;
      end;
    end;
  end;
end;

function TCandleChart.GetElapsed: Int64;
begin
  Result:=Candles.Elapsed;
end;

function TCandleChart.GetDuration: Int64;
begin
  Result:=Candles.Duration;
end;

procedure TCandleChart.Paint;
var R: TRectF;
begin

  ElapsesMeasure.SetInterval(Elapsed,Duration);

  var SaveState:=Canvas.SaveState;

  R:=GetCandlesRect;
  R.Right:=LocalRect.Right;
  ValueMeasure.DrawTo(Canvas,R);

  R:=GetCandlesRect;
  R.Bottom:=LocalRect.Bottom;
  ElapsesMeasure.DrawTo(Canvas,R);

  R:=GetCandlesRect;
  Canvas.IntersectClipRect(R);
  Candles.DrawTo(Canvas,R);

  Canvas.RestoreState(SaveState);

  Canvas.Stroke.Thickness:=1;
  Canvas.DrawDashRect(R,0,0,AllCorners,1,claBlueviolet);

end;

procedure TCandleChart.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  P: TPointF;
  R: TRectF;
begin

  if EventInfo.GestureID=igiZoom then
  begin

    Pressed:=False;

    R:=GetCandlesRect;
    P:=AbsoluteToLocal(EventInfo.Location)-PointF(EventInfo.Distance/2,0);

    if TInteractiveGestureFlag.gfEnd in EventInfo.Flags then
    else
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    begin

      FZoomDuration:=EnsureValue(EventInfo.Distance,0,R.Width,0,Duration);

      FZoomElapsed:=Elapsed+EnsureValue(P.X,0,R.Right,0,Duration);

    end else
    begin

      Duration:=Round(FZoomDuration*R.Width/EventInfo.Distance);

      Elapsed:=FZoomElapsed-EnsureValue(P.X,0,R.Right,0,Duration);

    end;

    Handled:=True;

  end;

end;

procedure TCandleChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseMoved:=False;
  FElapsedDownX:=Elapsed;
end;

procedure TCandleChart.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then
  if ssLeft in Shift then
  begin
    Elapsed:=FElapsedDownX-EnsureValue(X-PressedPosition.X,0,GetCandlesRect.Width,0,Duration);
    if not FMouseMoved and (PressedPosition.Distance(PointF(X,Y))>4) then
      FMouseMoved:=True;
  end;
end;

procedure TCandleChart.Click;
begin
  inherited;
  if not FMouseMoved then DoCandleClick;
end;

end.
