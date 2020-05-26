unit Measures.Types;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Types,
  FMX.Graphics,
  Charts.Math;

type
  TMeasure = record
    CandleRect: TRectF;
    Value: Extended;
    Text: string;
  end;

  TMeasures = array of TMeasure;

  TElapse = record
    Time: Int64;
    DateTime: TDateTime;
    Text: string;
  end;

  TElapses = array of TElapse;

implementation

end.
