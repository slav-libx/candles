unit Candles.Types;

interface

type
  TCandle = record
    Title: string;
    DateTime: TDateTime;
    Min: Extended;
    Max: Extended;
    Open: Extended;
    Close: Extended;
    Volume: Extended;
    Time: Int64;
    Duration: Int64;
  end;

  TCandles = array of TCandle;

implementation

end.
