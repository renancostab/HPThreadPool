unit HPWorkEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  HPWork;

type
  THPWork = HPWork.THPWork;

  { THPWorkEngine }

  THPWorkEngine = class(TObject)
  private
    FEvent: TEvent;
    FWorkList: THPWorkList;
    FShutdown: Boolean;
    FCount: Integer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddWork(AWork: THPWork);
    procedure ShutDown;

    function WaitWork: THPWork;
    property Enqueued: Integer read GetCount;
  end;

implementation

{ THPWorkEngine }

function THPWorkEngine.GetCount: Integer;
begin
  Result := FWorkList.Count;
end;

constructor THPWorkEngine.Create;
begin
  FEvent := TEvent.Create(nil, True, False, EmptyStr);
  FWorkList := THPWorkList.Create;
  FShutdown := False;
  FCount := 0;
end;

destructor THPWorkEngine.Destroy;
begin
  FEvent.Free;
  FWorkList.Free;
  inherited Destroy;
end;

procedure THPWorkEngine.AddWork(AWork: THPWork);
begin
  FWorkList.AddWithLock(AWork);
  FCount := FWorkList.Count;
  FEvent.SetEvent;
end;

procedure THPWorkEngine.ShutDown;
begin
  FShutdown := True;
  FEvent.SetEvent;
end;

function THPWorkEngine.WaitWork: THPWork;
begin
  Result := nil;
  if (FShutdown) or (FEvent.WaitFor(INFINITE) <> wrSignaled) then
    Exit;

  FWorkList.Lock;
  try
    if FWorkList.Count = 0 then
      Exit;

    Result := FWorkList[0];
    FWorkList.Delete(0);
    FCount := FWorkList.Count;

    if FWorkList.Count = 0 then
      FEvent.ResetEvent;
  finally
    FWorkList.Unlock;
  end;
end;

end.

