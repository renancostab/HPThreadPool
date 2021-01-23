unit HPCommons;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TParameters = Pointer;

  TWorkMethod = procedure (AParameters: TParameters) of object;
  TWorkProcedure = procedure (AParameters: TParameters);


implementation

end.

