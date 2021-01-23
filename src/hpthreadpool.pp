unit HPThreadPool;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Fgl,
  SyncObjs,
  HPCommons,
  HPWorkEngine,
  HPThread,
  HPTaskApi;

type
  IHPTask = HPTaskApi.IHPTask;
  TTaskList = HPTaskApi.TTaskList;
  TTaskArray = HPTaskApi.TTaskArray;
  TWorkerList = specialize TFPGList<THPThread>;
  TParameters = HPCommons.TParameters;

  THPTask = class;

  { THPThreadPool }

  THPThreadPool = class(TObject)
  private
    FMaxThreads: Integer;
    FMinThreads: Integer;
    FWaiting: Integer;
    FRunning: Integer;
    FWorkEngine: THPWorkEngine;
    FWorkers: TWorkerList;

    class var FThreadPool: THPThreadPool;
    class function GetDefault: THPThreadPool; static;
    function GetCurrThreads: Integer;
    function GetEnqueued: Integer;
    procedure SetMaxThreads(AValue: Integer);
    procedure SetMinThreads(AValue: Integer);
    procedure TerminateAllWorkers;
    procedure Grow;
  protected
    procedure AddWork(AWork: TWorkProcedure; AParameters: Pointer; ATask: THPTask); overload;
    procedure AddWork(AWork: TWorkMethod; AParameters: Pointer; ATask: THPTask); overload;
  public
    constructor Create;
    destructor Destroy; override;

    property MinThreads: Integer read FMinThreads write SetMinThreads;
    property MaxThreads: Integer read FMaxThreads write SetMaxThreads;
    property CurrThreads: Integer read GetCurrThreads;
    property Enqueued: Integer read GetEnqueued;
    property Running: Integer read FRunning;
    property Waiting: Integer read FWaiting;

    class property DefaultPool: THPThreadPool read GetDefault;
  end;

  { THPTask }

  THPTask = class(TInterfacedObject, IHPTask, IHPInternalTask)
  private
    FEvent: TSimpleEvent;
    FLockGroup: TCriticalSection;
    FGroup: TSimpleEvent;
    FStatus: TTaskStatus;
  public
    constructor Create;
    destructor Destroy; override;

    function Wait(ATimeout: Cardinal): Boolean;
    function Wait(): Boolean;
    function GetStatus: TTaskStatus;

    procedure Done;
    procedure DefineGroupEvent(AGroup: TSimpleEvent);
    procedure ChangeStatus(AStatus: TTaskStatus);

    property Status: TTaskStatus read FStatus;

    class function Run(AWork: TWorkMethod; AParameters: Pointer; APool: THPThreadPool = nil): IHPTask;
    class function Run(AWork: TWorkProcedure; AParameters: Pointer; APool: THPThreadPool = nil): IHPTask;

    class function WaitForAll(ATasks: TTaskArray; ATimeout: Cardinal = INFINITE): Boolean;
    class function WaitForAny(ATasks: TTaskArray; ATimeout: Cardinal = INFINITE): Boolean;
  end;

implementation

{ THPThreadPool }

procedure THPThreadPool.SetMaxThreads(AValue: Integer);
begin
  if FMaxThreads = AValue then Exit;

  if AValue < 1 then
    AValue := 1;

  FMaxThreads := AValue;
end;

function THPThreadPool.GetEnqueued: Integer;
begin
  Result := FWorkEngine.Enqueued;
end;

function THPThreadPool.GetCurrThreads: Integer;
begin
  Result := FWorkers.Count;
end;

class function THPThreadPool.GetDefault: THPThreadPool; static;
begin
  if not Assigned(FThreadPool) then
    FThreadPool := THPThreadPool.Create;

  Result := FThreadPool;
end;

procedure THPThreadPool.SetMinThreads(AValue: Integer);
begin
  if FMinThreads = AValue then Exit;
  FMinThreads := AValue;
end;

procedure THPThreadPool.TerminateAllWorkers;
const
  TIMEOUT = 1000;
var
  Worker: THPThread;
begin
  FWorkEngine.ShutDown;
  for Worker in FWorkers do
    Worker.Terminate;

  for Worker in FWorkers do
  begin
    if WaitForThreadTerminate(Worker.Handle, TIMEOUT) <> 0 then
      KillThread(Worker.ThreadID);

    Worker.Free;
  end;
end;

procedure THPThreadPool.Grow;
begin
  if FWorkers.Count > FMaxThreads then
  begin
    FWorkers[0].FreeOnTerminate := True;
    FWorkers[0].Terminate;
    FWorkers.Delete(0);
    Exit;
  end;

  if (GetEnqueued > 0) and (FWaiting = 0) and (FWorkers.Count < FMaxThreads) then
    FWorkers.Add(THPThread.Create(FWorkEngine, @FRunning, @FWaiting));
end;

constructor THPThreadPool.Create;
begin
  FWorkers := TWorkerList.Create;
  FMinThreads := 1;
  FMaxThreads := TThread.ProcessorCount;
  FWorkEngine := THPWorkEngine.Create;
end;

procedure THPThreadPool.AddWork(AWork: TWorkMethod; AParameters: Pointer; ATask: THPTask);
begin
  FWorkEngine.AddWork(THPWork.Create(AWork, AParameters, ATask));
  Grow;
end;

procedure THPThreadPool.AddWork(AWork: TWorkProcedure; AParameters: Pointer; ATask: THPTask);
begin
  FWorkEngine.AddWork(THPWork.Create(AWork, AParameters, ATask));
  Grow;
end;

destructor THPThreadPool.Destroy;
begin
  TerminateAllWorkers;
  FWorkers.Free;
  FWorkEngine.Free;
  inherited Destroy;
end;

{ THPTask }

constructor THPTask.Create;
begin
  FLockGroup := TCriticalSection.Create;
  FEvent := TSimpleEvent.Create;
  FStatus := tsQueued;
end;

procedure THPTask.ChangeStatus(AStatus: TTaskStatus);
begin
  FStatus := AStatus;
end;

procedure THPTask.DefineGroupEvent(AGroup: TSimpleEvent);
begin
  FLockGroup.Acquire;
  try
    FGroup := AGroup;
    if (FStatus = tsCompleted) and (Assigned(FGroup)) then
      FGroup.SetEvent;
  finally
    FLockGroup.Release;
  end;
end;

destructor THPTask.Destroy;
begin
  FLockGroup.Free;
  FEvent.Free;
  inherited Destroy;
end;

procedure THPTask.Done;
begin
  FEvent.SetEvent;

  FLockGroup.Acquire;
  try
    if Assigned(FGroup) then
      FGroup.SetEvent;
  finally
    FLockGroup.Release;
  end;

  FStatus := tsCompleted;
end;

function THPTask.GetStatus: TTaskStatus;
begin
  Result := FStatus;
end;

class function THPTask.Run(AWork: TWorkMethod; AParameters: Pointer; APool: THPThreadPool): IHPTask;
var
  Task: THPTask;
begin
  if not Assigned(APool) then
    APool := THPThreadPool.GetDefault;

  Task := THPTask.Create;
  APool.AddWork(AWork, AParameters, Task);
  Result := Task;
end;

class function THPTask.Run(AWork: TWorkProcedure; AParameters: Pointer; APool: THPThreadPool): IHPTask;
var
  Task: THPTask;
begin
  if not Assigned(APool) then
    APool := THPThreadPool.GetDefault;

  Task := THPTask.Create;
  APool.AddWork(AWork, AParameters, Task);
  Result := Task;
end;

function THPTask.Wait(ATimeout: Cardinal): Boolean;
begin
  Result := FEvent.WaitFor(ATimeout) = wrSignaled;
end;

function THPTask.Wait(): Boolean;
begin
  Result := Wait(INFINITE);
end;

class function THPTask.WaitForAll(ATasks: TTaskArray; ATimeout: Cardinal): Boolean;
var
  Task: IHPTask;
begin
  Result := True;
  for Task in ATasks do
  begin
    if not Task.Wait(ATimeout) then
      Exit(False);
  end;
end;

class function THPTask.WaitForAny(ATasks: TTaskArray; ATimeout: Cardinal): Boolean;
var
  I: Integer;
  Task: THPTask;
  Event: TSimpleEvent;
begin
  Event := TSimpleEvent.Create;
  try
    for I := Low(ATasks) to High(ATasks) do
    begin
      Task := ATasks[I] as THPTask;
      Task.DefineGroupEvent(Event);
    end;

    Result := Event.WaitFor(ATimeout) = wrSignaled;
  finally
    for I := Low(ATasks) to High(ATasks) do
    begin
      Task := ATasks[I] as THPTask;
      Task.DefineGroupEvent(nil);
    end;

    Event.Free;
  end;
end;

initialization
  THPThreadPool.GetDefault;

finalization
  THPThreadPool.DefaultPool.Free;

end.

