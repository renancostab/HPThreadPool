unit HPWork;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GSyncList,
  HPCommons,
  HPTaskApi;

type

  { THPWork }

  THPWork = class(TObject)
  private
    FProcedure: TWorkProcedure;
    FMethod: TWorkMethod;
    FParameters: TParameters;
    FTask: IHPInternalTask;
  public
    constructor Create(AWork: TWorkProcedure; AParameters: TParameters; ATask: IHPInternalTask); overload;
    constructor Create(AWork: TWorkMethod; AParameters: TParameters; ATask: IHPInternalTask); overload;

    procedure Execute;
  end;

  THPWorkList = specialize TGSyncList<THPWork>;

implementation

{ THPWork }

constructor THPWork.Create(AWork: TWorkProcedure; AParameters: TParameters; ATask: IHPInternalTask);
begin
  FProcedure := AWork;
  FParameters := AParameters;
  FTask := ATask;
end;

constructor THPWork.Create(AWork: TWorkMethod; AParameters: TParameters; ATask: IHPInternalTask);
begin
  FMethod := AWork;
  FParameters := AParameters;
  FTask := ATask;
end;

procedure THPWork.Execute;
begin
  try
    FTask.ChangeStatus(tsRunning);
    if Assigned(FProcedure) then
      FProcedure(FParameters)
    else
      FMethod(FParameters);
  finally
    FTask.Done;
  end;
end;

end.

