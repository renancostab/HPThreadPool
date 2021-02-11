(**************************************************************************************************
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.
Project......: HPThreadPool
Author.......: Renan Bellódi
Original Code: HPTaskApi.pp
***************************************************************************************************)

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

