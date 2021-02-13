unit main;

{$mode delphi}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ezthreads;

type

  { TMainForm }

  TMainForm = class(TForm)
    btn_edit_doit: TButton;
    edit_test: TEdit;
    pctrl_main: TPageControl;
    ts_edit: TTabSheet;
    procedure btn_edit_doitClick(Sender: TObject);
  private
    procedure UpdateEditInMainThread(Const AThread:IEZThread);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btn_edit_doitClick(Sender: TObject);
var
  LThread:IEZThread;

  procedure WaitSomeTime(Const AThread:IEZThread);
  begin
    //sleep 1 seconds before updating the text
    Sleep(1000);
  end;

begin
  if not btn_edit_doit.Enabled then
    Exit;

  //update the edit control with some different text
  LThread := NewEZThread;

  //configure the thread and start it
  LThread
    .Settings
      .UpdateSynchronizeStopEvents(True) //this will "automatically" sync events
      .Thread
    .Events
      .UpdateOnStop(UpdateEditInMainThread) //pass in the event to update out edit
      .Thread
    .AddArg('text','a message from a thread')
    .Setup(WaitSomeTime)
    .Start;

  //disable control so user cannot click it a lot
  btn_edit_doit.Enabled:=False;
end;

procedure TMainForm.UpdateEditInMainThread(const AThread: IEZThread);
var
  LMsg:String;
begin
  if ThreadID<>MainThreadID then
    raise Exception.Create('needs to be in main thread');

  //set this for debugging, but optional variable, could just assign from thread
  LMsg := AThread['text'];
  edit_test.Text := LMsg;
  btn_edit_doit.Enabled := True;
end;

end.

