unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  HPThreadPool;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    procedure MyWork({%H-}AParameters: Pointer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Random(30) do
    THPTask.Run(@MyWork, nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  THPThreadPool.DefaultPool.MaxThreads := THPThreadPool.DefaultPool.MaxThreads + 1;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
  Tasks: TTaskList;
begin
  Tasks := TTaskList.Create;
  for I := 0 to Random(30) do
    Tasks.Add(THPTask.Run(@MyWork, nil));

  THPTask.WaitForAny(Tasks.ToArray);

  Tasks.Free;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  THPThreadPool.DefaultPool.MaxThreads := THPThreadPool.DefaultPool.MaxThreads - 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.MyWork(AParameters: Pointer);
var
  I: Integer;
begin
  for I := 0 to 250 do
    Sleep(Random(10));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Memo1.Lines.Clear;

  Memo1.Lines.Add('Enqueued...: ' + THPThreadPool.DefaultPool.Enqueued.ToString());
  Memo1.Lines.Add('Running....: ' + THPThreadPool.DefaultPool.Running.ToString());
  Memo1.Lines.Add('Waiting....: ' + THPThreadPool.DefaultPool.Waiting.ToString());
  Memo1.Lines.Add('CurrThreads: ' + THPThreadPool.DefaultPool.CurrThreads.ToString());
  Memo1.Lines.Add('MaxThreads.: ' + THPThreadPool.DefaultPool.MaxThreads.ToString());
  Memo1.Lines.Add('MinThreads.: ' + THPThreadPool.DefaultPool.MinThreads.ToString());
end;

end.

