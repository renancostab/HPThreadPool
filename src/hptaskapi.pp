unit HPTaskApi;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FGL;

type
  TTaskStatus = (tsQueued, tsCompleted, tsRunning);

  IHPTask = interface
    function Wait(ATimeout: Cardinal): Boolean;
    function Wait(): Boolean;

    function GetStatus: TTaskStatus;
  end;

  { IHPInternalTask }

  IHPInternalTask = interface
    procedure Done;
    procedure ChangeStatus(AStatus: TTaskStatus);
  end;

  TSpecializeTaskList = specialize TFPGList<IHPTask>;
  TTaskArray = array of IHPTask;

  TTaskList = class(TSpecializeTaskList)
    function ToArray: TTaskArray;
  end;

implementation

{ TTaskList }

function TTaskList.ToArray: TTaskArray;
var
  I: Integer;
begin
  Result := Default(TTaskArray);
  SetLength(Result, Self.Count);
  for I := 0 to Self.Count - 1 do
    Result[I] := Self[I];
end;

end.

