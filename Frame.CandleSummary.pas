unit Frame.CandleSummary;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects,FMX.Ani, FMX.Effects;

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
    procedure FrameClick(Sender: TObject);
    procedure FloatAnimation1Finish(Sender: TObject);
  private
  public
    procedure SetValues(Open,Close,Max,Min,Volume: Extended);
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

procedure TCandleSummaryFrame.FrameClick(Sender: TObject);
begin
  Hide;
end;

procedure TCandleSummaryFrame.SetValues(Open,Close,Max,Min,Volume: Extended);
begin
  Text8.Text:=AmountToStr(Open);
  Text5.Text:=AmountToStr(Close);
  Text6.Text:=AmountToStr(Max);
  Text7.Text:=AmountToStr(Min);
  Text9.Text:=AmountToStr(Volume);
end;

procedure TCandleSummaryFrame.Show(Parent: TFmxObject; const TargetRect: TRectF);
begin
  Opacity:=0;
  Visible:=False;
  Self.Parent:=Parent;
  Visible:=True;
  if TargetRect.Top-Height>0 then
    Self.Position.Point:=TargetRect.TopLeft-PointF(0,Height+0)
  else
    Self.Position.Point:=TargetRect.BottomRight-PointF(TargetRect.Width,-0);
end;

procedure TCandleSummaryFrame.Hide;
begin
  Visible:=False;
end;

end.
