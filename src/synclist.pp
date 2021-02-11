(**************************************************************************************************
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.
Project......: HPThreadPool
Author.......: Renan Bellódi
Original Code: SyncList.pp
***************************************************************************************************)

unit SyncList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs;

type

  { TSyncList }

  TSyncList = class(TObject)
  private
    FLock: TCriticalSection;
    FList: TFPList;

    procedure Put(AIndex: Integer; AItem: Pointer);

    function GetCount: Integer;
    function Get(AIndex: Integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AItem: Pointer);
    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: Integer; AItem: Pointer);

    procedure AddWithLock(AItem: Pointer);
    procedure DeleteWithLock(AIndex: Integer);
    procedure InsertWithLock(AIndex: Integer; AItem: Pointer);

    procedure Lock;
    procedure Unlock;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: Pointer read Get write Put; default;
  end;

implementation

{ TSyncList }

procedure TSyncList.Put(AIndex: Integer; AItem: Pointer);
begin
  FList[AIndex] := AItem;
end;

function TSyncList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSyncList.Get(AIndex: Integer): Pointer;
begin
  Result := FList[AIndex];
end;

constructor TSyncList.Create;
begin
  FLock := TCriticalSection.Create;
  FList := TFPList.Create;
end;

destructor TSyncList.Destroy;
begin
  FList.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TSyncList.Add(AItem: Pointer);
begin
  FList.Add(AItem);
end;

procedure TSyncList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TSyncList.Insert(AIndex: Integer; AItem: Pointer);
begin
  FList.Insert(AIndex, AITem);
end;

procedure TSyncList.AddWithLock(AItem: Pointer);
begin
  FLock.Acquire;
  try
    FList.Add(AItem);
  finally
    FLock.Release;
  end;
end;

procedure TSyncList.DeleteWithLock(AIndex: Integer);
begin
  FLock.Acquire;
  try
    FList.Delete(AIndex);
  finally
    FLock.Release;
  end;
end;

procedure TSyncList.InsertWithLock(AIndex: Integer; AItem: Pointer);
begin
  FLock.Acquire;
  try
    FList.Insert(AIndex, AItem);
  finally
    FLock.Release;
  end;
end;

procedure TSyncList.Lock;
begin
  FLock.Acquire;
end;

procedure TSyncList.Unlock;
begin
  FLock.Release;
end;

end.

