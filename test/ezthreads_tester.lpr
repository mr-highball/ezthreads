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
{$modeswitch nestedprocvars}

program ezthreads_tester;

uses
  SysUtils,
  ezthreads;

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
    Waiting:=False;
  end;

  procedure Start(Const AThread:IEZThread);
  begin
    //fetch the argument by name and print it to the screen
    WriteLn('TestHelloWorld::ThreadStart::Data::' + AThread[NAME]);
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
      .UpdateOnStopNestedCallback(@UpdateWaiting)
      .Thread
    .AddArg(NAME,'hello world from an ezthread!')
    .Setup(@Start)
    .Start;

  //simple loop to see when our thread finishes
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

begin
  TestAddArg;
  TestHelloWorld;

  //display results to user
  ReadLn;
end.

