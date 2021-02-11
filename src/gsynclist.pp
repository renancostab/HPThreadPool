(**************************************************************************************************
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.
Project......: HPThreadPool
Author.......: Renan Bellódi
Original Code: GSyncList.pp
***************************************************************************************************)

unit GSyncList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncList;

type

  { TGSyncList }

  generic TGSyncList<T> = class(TObject)
  private
    FSList: TSyncList;
    procedure Put(AIndex: Integer; AItem: T);

    function GetCount: Integer;
    function Get(AIndex: Integer): T;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AItem: T);
    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: Integer; AItem: Pointer);

    procedure AddWithLock(AItem: T);
    procedure DeleteWithLock(AIndex: Integer);
    procedure InsertWithLock(AIndex: Integer; AItem: Pointer);

    procedure Lock;
    procedure Unlock;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: T read Get write Put; default;
  end;

implementation

{ TGSyncList }

procedure TGSyncList.Put(AIndex: Integer; AItem: T);
begin
  FSList[AIndex] := AItem;
end;

function TGSyncList.GetCount: Integer;
begin
  Result := FSList.Count;
end;

function TGSyncList.Get(AIndex: Integer): T;
begin
  Result := T(FSList[AIndex]);
end;

constructor TGSyncList.Create;
begin
  FSList := TSyncList.Create;
end;

destructor TGSyncList.Destroy;
begin
  FSList.Free;
  inherited Destroy;
end;

procedure TGSyncList.Add(AItem: T);
begin
  FSList.Add(AItem);
end;

procedure TGSyncList.Delete(AIndex: Integer);
begin
  FSList.Delete(AIndex);
end;

procedure TGSyncList.Insert(AIndex: Integer; AItem: Pointer);
begin
  FSList.Insert(AIndex, AItem);
end;

procedure TGSyncList.AddWithLock(AItem: T);
begin
  FSList.AddWithLock(AItem);
end;

procedure TGSyncList.DeleteWithLock(AIndex: Integer);
begin
  FSList.DeleteWithLock(AIndex);
end;

procedure TGSyncList.InsertWithLock(AIndex: Integer; AItem: Pointer);
begin
  FSList.InsertWithLock(AIndex, AItem);
end;

procedure TGSyncList.Lock;
begin
  FSList.Lock;
end;

procedure TGSyncList.Unlock;
begin
  FSList.Unlock;
end;

end.

