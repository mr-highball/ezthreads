program ezthreads_tester;

uses
  SysUtils,
  ezthreads;

var
  Thread : IEZThread;
  Waiting : Boolean;

  procedure UpdateWaiting(Const AThread:IEZThread);
  begin
    Waiting:=False;
  end;

  procedure Start(Const AThread:IEZThread);
  begin
    //todo - sample showing fetching arguments and using them
  end;

begin
  Waiting:=True;
  //todo - init this to a new ezthread
  Thread:=nil;

  //below we show how we can setup an ezthread with a few arguments
  //and an on stop callback
  Thread
    .Settings
      //maximum runtime is 50msecs will terminate if takes longer
      .UpdateMaxRuntime(50)
      .Thread
    .Events
      .UpdateOnStopCallback(@UpdateWaiting)
      .Thread
    .AddArg('test_string','hello world')
    .AddArg('test_int',5)
    .Setup(@Start)
    .Start;

  //simple loop to see when our thread finishes
  while Waiting do
    Sleep(100);

  WriteLn('waiting no longer');
end.

