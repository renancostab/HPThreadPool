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

