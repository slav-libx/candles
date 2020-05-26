unit Frame.CandleSummary;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Ani,
  FMX.Effects;

type
  TCandleSummaryFrame = class(TFrame)
    Rectangle1: TRectangle;
    Text1: TText;
    Text8: TText;
    Text2: TText;
    Text5: TText;
    Text3: TText;
    Text6: TText;
    Text4: TText;
    Text7: TText;
    FloatAnimation1: TFloatAnimation;
    Text9: TText;
    Text10: TText;
    Text11: TText;
    procedure FloatAnimation1Finish(Sender: TObject);
  private
  public
    procedure SetValues(const Title: string; DateTime: TDateTime;
      Open,Close,Max,Min,Volume: Extended);
    procedure Show(Parent: TFmxObject; const TargetRect: TRectF);
    procedure Hide;
  end;

implementation

{$R *.fmx}

function AmountToStr(Amount: Extended): string;
begin
  Result:=FormatFloat('0.00######',Amount);
end;

procedure TCandleSummaryFrame.FloatAnimation1Finish(Sender: TObject);
begin
  if Opacity=0 then Parent:=nil;
end;

procedure TCandleSummaryFrame.SetValues(const Title: string; DateTime: TDateTime;
  Open,Close,Max,Min,Volume: Extended);
begin
  if Title='' then
    Text11.Text:=DateTimeToStr(DateTime)
  else
    Text11.Text:=Title;
  Text8.Text:=AmountToStr(Open);
  Text5.Text:=AmountToStr(Close);
  Text6.Text:=AmountToStr(Max);
  Text7.Text:=AmountToStr(Min);
  Text9.Text:=AmountToStr(Volume);
end;

procedure TCandleSummaryFrame.Show(Parent: TFmxObject; const TargetRect: TRectF);
var
  P: TPointF;
  ParentRect: TRectF;
begin

  Opacity:=0;
  Visible:=False;

  if Parent is TControl then
    ParentRect:=TControl(Parent).LocalRect
  else
    ParentRect:=TargetRect;

  if TargetRect.Top-Height>ParentRect.Top then
    P:=PointF(TargetRect.Left,Max(ParentRect.Top,TargetRect.Top-Height))
  else
    P:=PointF(TargetRect.Left,Min(ParentRect.Bottom-Height,TargetRect.Bottom));

  Self.Position.Point:=P;
  Self.Parent:=Parent;

  Visible:=True;

end;

procedure TCandleSummaryFrame.Hide;
begin
  Visible:=False;
end;

end.
