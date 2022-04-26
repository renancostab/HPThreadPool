(**************************************************************************************************
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.
Project......: HPThreadPool
Author.......: Renan Bell�di
Original Code: HPThread.pp
***************************************************************************************************)

unit HPThread;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  HPWorkEngine;

type
  THPThreadClass = class of THPThread;

  { THPThread }

  THPThread = class(TThread)
  private
    FWorkEngine: THPWorkEngine;
    FRunning: PInteger;
    FWaiting: PInteger;
    FWorking: Boolean;
    FNotify: TNotifyEvent;

    procedure IncRunning; inline;
    procedure DecRunning; inline;
    procedure IncWaiting; inline;
    procedure DecWaiting; inline;
  protected
    procedure Execute; override;
  public
    constructor Create(AWorkEngine: THPWorkEngine; ARunning: PInteger; AWaiting: PInteger; ANotify: TNotifyEvent; AUseSync: Boolean);
    property Running: Boolean read FWorking;
  end;

implementation

{ THPThread }

procedure THPThread.IncRunning;
begin
  InterlockedIncrement(FRunning^);
  FWorking := True;
end;

procedure THPThread.DecRunning;
begin
  InterlockedDecrement(FRunning^);
  FWorking := False;
end;

procedure THPThread.IncWaiting;
begin
  InterlockedIncrement(FWaiting^);
end;

procedure THPThread.DecWaiting;
begin
  InterlockedDecrement(FWaiting^);
end;

procedure THPThread.Execute;
var
  Work: THPWork;
begin
  while not Terminated do
  begin
    IncWaiting;
    Work := FWorkEngine.WaitWork;
    DecWaiting;

    if not Assigned(Work) then
      Continue;

    IncRunning;
    try
      try
        Work.Execute;
      except
      end;
    finally
      Work.Free;
    end;

    DecRunning;
  end;

  if Assigned(FNotify) then
    FNotify(Self);

  ReturnValue := 0;
end;

constructor THPThread.Create(AWorkEngine: THPWorkEngine; ARunning: PInteger; AWaiting: PInteger; ANotify: TNotifyEvent; AUseSync: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FWorkEngine := AWorkEngine;
  FRunning := ARunning;
  FWaiting := AWaiting;
  FWorking := False;

  // OnTerminate by default handles the event using Synchronize
  if Assigned(ANotify) then
  begin
    if AUseSync then
      OnTerminate := ANotify
    else
      FNotify := ANotify;
  end;
end;

end.

