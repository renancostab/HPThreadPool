![Version](https://img.shields.io/badge/version-v1.1-yellow.svg)
![License](https://img.shields.io/github/license/renancostab/HPThreadPool.svg)
![Lang](https://img.shields.io/github/languages/top/renancostab/HPThreadPool.svg)

# HPThreadPool

HPThreadPool is a High Performance Thread Pool design for Lazarus Free pascal

# Design

The goal is to provide a nice and simple way to work with threads without the excessive overhead of runtime thread creation, the major topics of a thread pool is covered, including: 
 
  - Statics
    - Minimum 
    - Maximum 
    - Threads running
    - Work enqueued
  - Resize of thread pool
  - Zero sleep time when a job is available
  - WaitForAll and WaitForAny
  - Worker thread class customization (if needed)
  - OnTerminateWorker callback sync (Synchronized) and async (Thread context)
  
 # Quick Example
 
 ```Pascal
unit Demo;

interface

uses
  HPThreadPool;
  
type
  TMyJobs = class(TObject)
  private
  public
    class procedure MyFiboWork(AParameters: Pointer);
  end;
  
implementation

procedure TMyJobs.MyFiboWork(AParameters: Pointer);
var
  I: Integer;
  Value: Integer; 
  Sum: Integer = 0;
  NumA: Integer = 1;
  NumB: Integer = 0;
begin
  Value := PInteger(AParameters)^;
  
  for I := 1 to Value do
  begin
    Sum := NumA + NumB;
    NumB := NumA;
    NumA := Sum;
  end;
  
  PInteger(AParameters)^ := Sum;
end;
 
// Example working with 100 jobs and a random value of fibonnaci
const
  MAXJOBS = 100;
var
  I: Integer;
  Fibo: array of Integer;  
  Tasks: TTaskList;
begin
  Randomize;
  Tasks := TTaskList.Create;
  SetLength(Fibo, MAXJOBS);
  
  for I := 0 to MAXJOBS - 1 do
  begin
    Fibo[I] := Random(10000);
    Tasks.Add(THPTask.Run(@TMyJobs.MyFiboWork, @Fibo[I]));
  end;
  
  THPTask.WaitForAll(Tasks.ToArray);
  
  for I := 0 to High(Fibo) do
    WriteLn(I, ' - ', Fibo[I]);
  
  Tasks.Free;
end;
