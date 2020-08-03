{$mode delphi}
{$modeswitch nestedprocvars}

program ezthreads_tester_pool;
uses
  SysUtils,
  ezthreads,
  ezthreads.pool;

(*
  tests showing that the start and stop events work properly
  with a thread pool
*)
procedure TestStartStop;
var
  LPool : IEZThreadPool;

  procedure StartPool(const AThread : IEZThread);
  begin
    AThread.AddArg('started', True);
  end;

  procedure StopPool(const AThread : IEZThread);
  begin
    AThread.AddArg('stopped', True);
  end;
begin
  LPool := NewEZThreadPool(1);

  LPool
    .Events
      .UpdateOnStartNestedCallback(StartPool)
      .UpdateOnStopNestedCallback(StopPool)
      .Thread
    .Start;

  Await(LPool);

  WriteLn(Format('TestStartStop::[success]:%s', [BoolToStr(LPool.Exists['started'] and LPool.Exists['stopped'], True)]));
end;

(*
  test that shows one simple task completing
  by an ezthread pool
*)
procedure TestSingleTask;
var
  LPool : IEZThreadPool;
  LJobOneFinished : Boolean;

  //simple parallel job
  procedure JobOne(const AThread : IEZThread);
  begin
    //do some work
    Sleep(100);
    LJobOneFinished := True;
  end;

begin
  //flags for checking if both jobs finished
  LJobOneFinished := False;

  //init a pool with one worker
  LPool := NewEZThreadPool(1);

  //work a single job
  LPool
    .Queue(JobOne, nil, nil)
    .Start; //start the pool

  //wait until the job finishes
  Await(LPool);

  //write status
  WriteLn(Format('TestSingleTask::[success]:%s', [BoolToStr(LJobOneFinished, True)]));
end;

(*
  test that shows two simple tasks completing in parallel
  by an ezthread pool
*)
procedure TestTwoTasks;
var
  LPool : IEZThreadPool;
  LJobOneFinished,
  LJobTwoFinished: Boolean;

  //simple parallel job
  procedure JobOne(const AThread : IEZThread);
  begin
    //do some work
    Sleep(100);
    LJobOneFinished := True;
  end;

  //simple parallel job
  procedure JobTwo(const AThread : IEZThread);
  begin
    //do some work
    Sleep(100);
    LJobTwoFinished := True;
  end;

begin
  //flags for checking if both jobs finished
  LJobOneFinished := False;
  LJobTwoFinished := False;

  //init a pool with two workers
  LPool := NewEZThreadPool(2);

  //work two jobs
  LPool
    .Queue(JobOne, nil, nil)
    .Queue(JobTwo, nil, nil)
    .Start; //start the pool

  //wait until both jobs finish
  Await(LPool);

  //write status
  WriteLn(Format('TestTwoTasks::[success]:%s', [BoolToStr(LJobOneFinished and LJobTwoFinished, True)]));
end;

(*
  test that demonstrates the sharing of arguments
  between worker threads in a pool.

  ex:
    -pool is provided a "common" argument FOUND
    -argument is "by-value" not by "reference" unless reference type
     so each worker will have their own copy
*)
procedure TestSharedArgs;
var
  LJobOneFound,
  LJobTwoFound: Boolean;
  LPool: IEZThreadPool;
const
  FOUND = 'found';


  //simple parallel job to check if arg exists
  procedure JobOne(const AThread : IEZThread);
  begin
    LJobOneFound := AThread.Exists[FOUND];
  end;

  procedure JobTwo(const AThread : IEZThread);
  begin
    //do some work to check if arg exists
    LJobTwoFound := AThread.Exists[FOUND];
  end;
begin
  //flags for checking if both jobs finding the argument
  LJobOneFound := False;
  LJobTwoFound := False;

  //init a pool with two workers
  LPool := NewEZThreadPool(2);

  //add the "global" argument which will be passed to workers
  LPool.AddArg(FOUND, True);

  //work two jobs
  LPool
    .Queue(JobOne, nil, nil)
    .Queue(JobTwo, nil, nil)
    .Start; //start the pool

  //wait until both jobs finish
  Await(LPool);

  //write status
  WriteLn(Format('TestSharedArgs::[success]:%s', [BoolToStr(LJobOneFound and LJobTwoFound, True)]));
end;

(*
  tests if a pool is freed after work has finished
*)
procedure TestFree;
var
  LPool : IEZThreadPool;
  LJobOneFinished : Boolean;
  LRef: LongInt;

  //simple parallel job
  procedure JobOne(const AThread : IEZThread);
  begin
    //do some work
    Sleep(100);
    LJobOneFinished := True;
  end;

begin
  //flags for checking if both jobs finished
  LJobOneFinished := False;

  //init a pool with one worker
  LPool := NewEZThreadPool(1);

  //should just be one reference
  LPool._AddRef;
  LRef := LPool._Release;

  //work a single job
  LPool
    .Queue(JobOne, nil, nil)
    .Start; //start the pool

  LPool._AddRef;
  LRef := LPool._Release;

  //wait until the job finishes (await in this case will call APool.Stop)
  Await(LPool);

  //now manually increment/dec the reference to make sure we have only one ref
  LPool._AddRef;
  LRef := LPool._Release;

  //write status (I was assuming ref should be 1.. but at this point it's 2 and heaptrc doesn't report any mem leaks)
  WriteLn(Format('TestFree::[success]:%s', [BoolToStr(LRef <= 2, True)]));
end;

(*
  test demonstrating how to use a thread pool with the
  'ForceTerminate' setting to kill worker threads immediately
*)
procedure TestForceStop;
var
  LPool : IEZThreadPool;

  procedure Start(const AThread : IEZThread);
  var
    LPoolRef : IEZThreadPool;
  begin
    //since we expect the pool to go out of scope of the outer method first
    //we capture a reference to avoid a/v's
    LPoolRef := LPool;

    //this should not finish
    sleep(500);

    LPoolRef.AddArg('failed', 'test should not reach this point');
  end;

begin
  LPool := NewEZThreadPool(1);

  LPool
    .Queue(Start, nil, nil)
    .Settings
      .UpdateForceTerminate(True)
      .Thread
    .Start;

  //instead of calling await, call 'stop' explicitly since the behavior of
  //await is to 'await' all workers finishing first, then calling stop
  LPool.Stop;

  WriteLn(Format('TestForceStop::[success]:%s', [BoolToStr(not LPool.Exists['failed'], True)]));
end;

begin
  {$IF DECLARED(GlobalSkipIfNoLeaks)}
  GlobalSkipIfNoLeaks := True;
  setHeapTraceOutput('memory-leak.log');
  {$ENDIF}
  TestStartStop;
  TestSingleTask;
  TestTwoTasks;
  TestSharedArgs;
  TestFree;
  TestForceStop;

  //wait for user input
  ReadLn;
end.

