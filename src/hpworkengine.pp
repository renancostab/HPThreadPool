(**************************************************************************************************
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.
Project......: HPThreadPool
Author.......: Renan Bellódi
Original Code: HPWorkEngine.pp
***************************************************************************************************)

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

