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

  Await;
end;

(*
  a "hello world" of sorts. shows a fairly complete demonstration
  of creating a thread to write hello world to the console
*)
procedure TestHelloWorld;
const
  NAME='hi';
var
  Thread : IEZThread;

  procedure Start(Const AThread:IEZThread);
  begin
    WriteLn(AThread[NAME]);
  end;

begin
  Thread:=TEZThreadImpl.Create;

  Thread
    .AddArg(NAME,'TestHelloWorld::hello world from an ezthread!')
    .Setup(Start)
    .Start;

  Await;

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

(*
  shows that await can be used for blocking for a single thread
*)
procedure TestSingleAwait;
var
  LThread:IEZThread;

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

(*
  this test sets up a situation where one thread (B) depends
  on another thread (A) to finish before it can proceed to process.
  lastly this method blocks until both thread (A) & (B) finish using
  await
*)
procedure TestThreadDependency;
var
  LThreadA,
  LThreadB:IEZThread;

  procedure MethodA(Const AThread:IEZThread);
  begin
    //do some important work
    WriteLn('TestThreadDependency::ThreadA starting');
    Sleep(1000);
    WriteLn('TestThreadDependency::ThreadA finished');
  end;

  procedure MethodB(Const AThread:IEZThread);
  var
    LID:String;
  begin
    LID:=AThread['id'];

    //write that we got to thread (B)
    WriteLn('TestThreadDependency::ThreadB starting');

    //before doing our work, wait until thread (A) has completed
    Await(LID);

    //write to console that we finished
    WriteLn('TestThreadDependency::ThreadB finished');
  end;

begin
  //init both threads
  LThreadA:=TEZThreadImpl.Create;
  LThreadB:=TEZThreadImpl.Create;

  //setup thread (A)
  LThreadA
    .Setup(MethodA)
    .Start;

  //below we add the thread group id of (A) so that thread (B) can
  //"await" until (A) is done to proceed its task
  LThreadB
    .AddArg('id',LThreadA.Settings.Await.GroupID)
    .Setup(MethodB)
    .Start;

  //wait for all threads
  Await;
end;

begin
  TestAddArg;
  TestHelloWorld;
  TestDynInput;
  TestForceKill;
  TestSingleAwait;
  TestThreadDependency;
  ReadLn;
end.

