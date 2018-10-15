{ ezthreads

  Copyright (c) 2018 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

(*
  if you want to use local procedures as variables, this modeswitch needs
  to be included, otherwise you can still use standard callbacks
  or method procedures
*)
{$mode delphi}{$H+}
{$modeswitch nestedprocvars}

program ezthreads_tester;

uses
  {$ifdef unix}
  cthreads,
  cmem,
  {$endif}
  Classes,
  SysUtils,
  ezthreads,
  dateutils;

(*
  a simple test to add an argument and verify the data is
  what was provided
*)
procedure TestAddArg;
var
  Thread : IEZThread;
const
  NAME='TestName';
  DATA='TestData';
begin
  Thread:=TEZThreadImpl.Create;
  Thread.AddArg(NAME,DATA);
  if Thread.Exists[NAME] then
  begin
    if Thread[NAME] = DATA then
      WriteLn('TestAddrArg::success')
    else
      WriteLn('TestAddrArg::failure, data did not match');
  end
  else
    WriteLn('TestAddArg::failed, could not find arg name');
end;

(*
  a "hello world" of sorts. shows a fairly complete demonstration
  of creating a thread to write hello world to the console
*)
procedure TestHelloWorld;
const
  NAME='hi';
  SLEEP_TIME=10;
  MAX_RUN=1000;
var
  Thread : IEZThread;
  Waiting : Boolean;
  LElapsed : Cardinal;

  procedure UpdateWaiting(Const AThread:IEZThread);
  begin
    //let main thread know we are done processing by updating a local var
    //also note that this is method is called in the callers thread
    //so we can
    Waiting:=False;
    WriteLn(AThread[NAME]);
  end;

  procedure Start(Const AThread:IEZThread);
  begin
    //note that we can't use writeln without sync because the start
    //runs in a separate thread than the caller's
    if MainThreadID = TThread.CurrentThread.ThreadID then
    begin
      WriteLn('TestHelloWorld::ThreadStart::failure, this should have been executed in a different thread id');
      Exit;
    end;

    //fetch the argument by name and modify it with info showing we
    //were in a different thread
    AThread.AddArg(
      NAME,
      AThread[NAME] + Format(
        ' MainThreadID:%d EZThreadID:%d',
        [MainThreadID,TThread.CurrentThread.ThreadID]
      )
    );
  end;

begin
  Waiting:=True;
  LElapsed:=0;
  Thread:=TEZThreadImpl.Create;

  //below we show how we can setup an ezthread with a few arguments
  //and an on stop callback
  Thread
    .Settings
      //will terminate if takes longer than this
      .UpdateMaxRuntime(MAX_RUN)
      .Thread
    .Events
      .Thread
    .AddArg(NAME,'hello world from an ezthread!')
    .Setup(Start,UpdateWaiting,UpdateWaiting)
    .Start;

  //simple loop to see when our thread finishes. this would normally not be
  //necessary to wait in the context of this method, but to make sure
  //our tests are run in order this was added. also if the thread was aborted
  //then the OnError method(s) would be called
  while Waiting do
  begin
    Sleep(SLEEP_TIME);
    Inc(LElapsed,SLEEP_TIME);
    if LElapsed >= MAX_RUN then
    begin
      WriteLn('TestHelloWorld::failure, took too long');
      Exit;
    end;
  end;

  WriteLn('TestHelloWorld::success, waiting no longer');
end;

(*
  an example showing how to use dynamic input and perform some
  arbitrary calculation with it in an ezthread
*)
procedure TestDynInput;
var
  LThread:IEZThread;

  (*
    adds two numbers and returns
  *)
  function Add(A,B:Integer):Integer;
  begin
    Result:=A + B;
  end;

  (*
    setup method (runs in separate thread)
  *)
  procedure Setup(Const AThread:IEZThread);
  begin
    AThread.AddArg('result',Add(AThread['A'],AThread['B']));
  end;

  (*
    method to run once thread work has finished
  *)
  procedure Print(Const AThread:IEZThread);
  begin
    WriteLn(MainThreadID,' ',GetThreadID);
    WriteLn('TestDynInput::Result=' + IntToStr(AThread['result']));
  end;

begin
  LThread:=TEZThreadImpl.Create;
  LThread
    .AddArg('A',1)
    .AddArg('B',2)
    .Setup(Setup,nil,Print)
    .Start;

  Await;
end;

procedure TestForceKill;
var
  LThread:IEZThread;

  procedure Setup(Const AThread:IEZThread);
  begin
    Sleep(AThread['sleep']);
  end;

  procedure CheckElapsed(Const AThread:IEZThread);
  begin
    WriteLn(
      'TestForceKill::CheckElapsed::elapsed time - ',
      MilliSecondsBetween(AThread['start'],Now)
    );
  end;

begin
  LThread:=TEZThreadImpl.Create;
  LThread
    .AddArg('sleep',50000)
    .AddArg('start',Now)
    .Settings
      .UpdateMaxRuntime(50)
      .UpdateForceTerminate(True)//non-graceful termination, but guarantees no dangling threads after timeout
      .Thread
    .Events
      .UpdateOnStopNestedCallback(CheckElapsed)
      .Thread
    .Setup(Setup)
    .Start;

  Await;
  WriteLn('TestForceKill::force kill finished');
end;

procedure TestSingleAwait;
var
  LThread:IEZThread;
  LTest:String;

  procedure Setup(Const AThread:IEZThread);
  begin
    Sleep(1000);
    WriteLn('TestSingleAwait::setup finished');
  end;

begin
  LThread:=TEZThreadImpl.Create;
  LThread
    .Setup(Setup)
    .Start;

  //call await with the thread to block until this thread has finished running
  Await(LThread);
  WriteLn('TestSingleAwait::done awaiting');
end;

begin
  //TestAddArg;
  //TestHelloWorld;
  //TestDynInput;
  TestForceKill;
  //TestSingleAwait;
  ReadLn;
end.

