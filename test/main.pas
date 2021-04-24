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
    btn_edit_doit_synch: TButton;
    edit_test: TEdit;
    edit_test_synch: TEdit;
    pctrl_main: TPageControl;
    ts_synch: TTabSheet;
    ts_edit: TTabSheet;
    procedure btn_edit_doitClick(Sender: TObject);
    procedure btn_edit_doit_synchClick(Sender: TObject);
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

procedure TMainForm.btn_edit_doit_synchClick(Sender: TObject);
var
  LThread: IEZThread;

  procedure Work(const AThread : IEZThread);

    procedure UpdateUI(const AThread : IEZThread);
    begin
      edit_test_synch.Text := AThread['message'];
    end;

  begin
    AThread.AddArg('message', 'message one');
    AThread.Synchronize(UpdateUI);

    //now wait some time before sending the next message
    Sleep(3000);

    AThread.AddArg('message', 'message two');
    AThread.Synchronize(UpdateUI);
  end;

begin
  if not btn_edit_doit_synch.Enabled then
    Exit;

  //this example starts a method that needs to update the UI from the other thread's
  //context but still needs to "work" in the background (cannot use a single stop event)
  LThread := NewEZThread;
  LThread.Setup(Work).Start;
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

