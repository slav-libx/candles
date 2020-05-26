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
  Candles.Types,
  Measures.Types,
  Charts.Math,
  Chart.Candles,
  Chart.Volumes,
  Chart.Measures;

type
  TOnCandleSelect = procedure (Sender: TObject; const Candle: TCandle) of object;

  TCandleChart = class(TControl)
  private
    Candles: TCandlesIndicator;
    Volumes: TVolumesIndicator;
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
    function GetAreaRect: TRectF;
    function GetCandlesRect: TRectF;
    function GetVolumesRect: TRectF;
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetMeasureLastColor(const Value: TAlphaColor);
    procedure SetMeasureHeight(const Value: Single);
    procedure SetMeasureWidth(const Value: Single);
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
    procedure ClearData;
    procedure SetCandlesData(const Candles: TCandles);
    procedure SetValueMeasure(const Measures: TMeasures);
    procedure SetElapsesMeasure(const Elapses: TElapses);
    procedure SetCandlesTimes(StartTime,EndTime: Int64);
    procedure SetCandlesRange(MinValue,MaxValue: Extended);
    procedure AutoCalcRanges;
    function GetCandleRect(const Candle: TCandle; BodyOnly: Boolean=True): TRectF;
    function EOE: Boolean;
    procedure Deselect;
    property Elapsed: Int64 read GetElapsed write SetElapsed;
    property Duration: Int64 read GetDuration write SetDuration;
    property BullColor: TAlphaColor read Candles.BullColor write SetBullColor;
    property BearColor: TAlphaColor read Candles.BearColor write SetBearColor;
    property SelectedColor: TAlphaColor read Candles.SelectedColor write SetSelectedColor;
    property MeasureLastColor: TAlphaColor read ValueMeasure.LastColor write SetMeasureLastColor;
    property MeasureWidth: Single read ValueMeasure.Width write SetMeasureWidth;
    property MeasureHeight: Single read ElapsesMeasure.Height write SetMeasureHeight;
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

  Volumes.BullColor:=claGreen;
  Volumes.BearColor:=claRed;

  ValueMeasure.LastColor:=$FFED9A23;

  ValueMeasure.Width:=65;
  ElapsesMeasure.Height:=20;

end;

destructor TCandleChart.Destroy;
begin
  inherited;
end;

procedure TCandleChart.SetBullColor(const Value: TAlphaColor);
begin
  Candles.BullColor:=Value;
  Volumes.BullColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetBearColor(const Value: TAlphaColor);
begin
  Candles.BearColor:=Value;
  Volumes.BearColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetSelectedColor(const Value: TAlphaColor);
begin
  Candles.SelectedColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetMeasureHeight(const Value: Single);
begin
  ElapsesMeasure.Height:=Value;
  Repaint;
end;

procedure TCandleChart.SetMeasureLastColor(const Value: TAlphaColor);
begin
  ValueMeasure.LastColor:=Value;
  Repaint;
end;

procedure TCandleChart.SetMeasureWidth(const Value: Single);
begin
  ValueMeasure.Width:=Value;
  Repaint;
end;

function TCandleChart.EOE: Boolean;
begin
  Result:=Elapsed=Candles.ElapsedMax;
end;

procedure TCandleChart.ClearData;
begin
  DoCandleDeselect;
  Self.Candles.Clear;
  Self.Volumes.Clear;
  Self.ValueMeasure.Clear;
  Self.ElapsesMeasure.Clear;
  Repaint;
end;

procedure TCandleChart.SetCandlesData(const Candles: TCandles);
begin
  DoCandleDeselect;
  Self.Candles.SetData(Candles);
  Self.Volumes.SetData(Candles);
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

procedure TCandleChart.SetCandlesTimes(StartTime,EndTime: Int64);
begin
  Candles.Area.TimeFrom:=StartTime;
  Candles.Area.TimeTo:=EndTime;
  Repaint;
end;

procedure TCandleChart.SetCandlesRange(MinValue,MaxValue: Extended);
begin
  Candles.Area.ValueMin:=MinValue;
  Candles.Area.ValueMax:=MaxValue;
  Repaint;
end;

procedure TCandleChart.AutoCalcRanges;
begin
  Candles.CalcRanges;
  ValueMeasure.SetRange(Candles.Area.ValueMin,Candles.Area.ValueMax);
  Volumes.CalcRanges;
  Repaint;
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

function TCandleChart.GetAreaRect: TRectF;
begin
  Result:=LocalRect;
  Result.Inflate(-20,0,-ValueMeasure.Width,-MeasureHeight);
end;

function TCandleChart.GetCandlesRect: TRectF;
begin
  Result:=GetAreaRect;
  Result.Height:=Round(Result.Height*0.9);
end;

function TCandleChart.GetVolumesRect: TRectF;
begin
  Result:=GetAreaRect;
  Result.Top:=GetCandlesRect.Bottom;
end;

procedure TCandleChart.DoCandleClick;
var P: TPointA;
begin

  DoCandleDeselect;

  if Assigned(FOnCandleSelect) then
  begin

    P:=EnsurePoint(PressedPosition,GetCandlesRect,RectA(Elapsed,Elapsed+Duration,Candles.Area.ValueMin,Candles.Area.ValueMax));
    Candles.SelectedIndex:=Candles.Get(P);
    if Candles.SelectedIndex<>-1 then
    begin
      Repaint;
      FOnCandleSelect(Self,Candles.Data[Candles.SelectedIndex]);
      Exit;
    end;

    P:=EnsurePoint(PressedPosition,GetVolumesRect,Volumes.Area);
    Candles.SelectedIndex:=Volumes.Get(P);
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
  R,V: TRectF;
  C: TCandle;
begin

  ElapsesMeasure.SetRange(Elapsed,Duration);
  ValueMeasure.SetRange(Self.Candles.Area.ValueMin,Self.Candles.Area.ValueMax);
  Volumes.SetRange(Elapsed,Duration);

  var SaveState:=Canvas.SaveState;

  // draw right measure

  R:=GetCandlesRect;
  R.Right:=LocalRect.Right;

  C:=Default(TCandle);

  if Length(Candles.Data)>0 then
    C:=Candles.Data[High(Candles.Data)];

  ValueMeasure.SetLastValue(Candles.GetCandleRect(C,GetCandlesRect,True),C.Close,FormatFloat('0.########',C.Close));

  ValueMeasure.DrawTo(Canvas,R);

  // draw lower measure

  R:=GetCandlesRect;
  R.Bottom:=LocalRect.Bottom;
  ElapsesMeasure.DrawTo(Canvas,R);

  // draw volumes & candles

  V:=GetVolumesRect;
  R:=GetCandlesRect;

  Canvas.IntersectClipRect(R+V);

  Volumes.DrawTo(Canvas,V);
  Candles.DrawTo(Canvas,R);

  Canvas.RestoreState(SaveState);

  {$IFDEF DEBUG}

  Canvas.Stroke.Thickness:=0.5;
  Canvas.DrawDashRect(R,0,0,AllCorners,1,claBlueviolet);
  Canvas.DrawDashRect(V,0,0,AllCorners,1,claBlueviolet);

  {$ENDIF}

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

procedure TCandleChart.Deselect;
begin
  DoCandleDeselect;
end;

end.
