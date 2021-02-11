(**************************************************************************************************
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.
Project......: HPThreadPool
Author.......: Renan Bellódi
Original Code: HPWork.pp
***************************************************************************************************)

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

