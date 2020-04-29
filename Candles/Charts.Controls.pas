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
  TOnCandleSelect = procedure (Sender: TObject; const Candle: TCandle) of object;

  TCandleChart = class(TControl)
  private
    Candles: TCandlesIndicator;
    ValueMeasure: TValueMeasure;
    ElapsesMeasure: TElapsesMeasure;
    FElapsedDownX: Int64;
    FZoomDuration: Int64;
    FZoomElapsed: Int64;
    FOnCandleSelect: TOnCandleSelect;
    FOnCandleDeselect: TNotifyEvent;
    procedure DoCandleClick;
    procedure DoCandleDeselect;
    procedure SetElapsed(Value: Int64);
    procedure SetDuration(const Value: Int64);
    procedure SetBullColor(const Value: TAlphaColor);
    procedure SetBearColor(const Value: TAlphaColor);
    function GetDuration: Int64;
    function GetElapsed: Int64;
    function GetCandlesRect: TRectF;
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetMeasureLastColor(const Value: TAlphaColor);
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
    function GetCandleRect(const Candle: TCandle; BodyOnly: Boolean=True): TRectF;
    property Elapsed: Int64 read GetElapsed write SetElapsed;
    property Duration: Int64 read GetDuration write SetDuration;
    property BullColor: TAlphaColor read Candles.BullColor write SetBullColor;
    property BearColor: TAlphaColor read Candles.BearColor write SetBearColor;
    property SelectedColor: TAlphaColor read Candles.SelectedColor write SetSelectedColor;
    property MeasureLastColor: TAlphaColor read ValueMeasure.LastColor write SetMeasureLastColor;
    property OnCandleSelect: TOnCandleSelect read FOnCandleSelect write FOnCandleSelect;
    property OnCandleDeselect: TNotifyEvent read FOnCandleDeselect write FOnCandleDeselect;
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
  Candles.SelectedColor:=claGold;
  ValueMeasure.LastColor:=claGoldenrod;

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

procedure TCandleChart.SetSelectedColor(const Value: TAlphaColor);
begin
  Candles.SelectedColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetMeasureLastColor(const Value: TAlphaColor);
begin
  ValueMeasure.LastColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetData(const Candles: TCandles);
begin
  DoCandleDeselect;
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
  DoCandleDeselect;
  if Assigned(FOnCandleSelect) then
  begin
    P:=EnsurePoint(PressedPosition,GetCandlesRect,RectF(Elapsed,Candles.Area.Top,Elapsed+Duration,Candles.Area.Bottom));
    Candles.SelectedIndex:=Candles.Get(P);
    if Candles.SelectedIndex<>-1 then
    begin
      Repaint;
      FOnCandleSelect(Self,Candles.Data[Candles.SelectedIndex]);
      Exit;
    end;
  end;
end;

procedure TCandleChart.DoCandleDeselect;
begin
  if Candles.SelectedIndex<>-1 then
  begin
    if Assigned(FOnCandleDeselect) then FOnCandleDeselect(Self);
    Candles.SelectedIndex:=-1;
    Repaint;
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
var
  R: TRectF;
  C: TCandle;
begin

  ElapsesMeasure.SetInterval(Elapsed,Duration);

  var SaveState:=Canvas.SaveState;

  R:=GetCandlesRect;
  R.Right:=LocalRect.Right;

  C:=Default(TCandle);

  if Length(Candles.Data)>0 then
    C:=Candles.Data[High(Candles.Data)];

  ValueMeasure.SetLastValue(Candles.GetCandleRect(C,R,True),C.Close,FormatFloat('0.00####',C.Close));

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
  DoCandleDeselect;
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

function TCandleChart.GetCandleRect(const Candle: TCandle; BodyOnly: Boolean): TRectF;
begin
  Result:=Candles.GetCandleRect(Candle,GetCandlesRect,BodyOnly);
end;

end.
