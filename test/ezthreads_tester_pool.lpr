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

  //work a single job
  LPool
    .Queue(JobOne, nil, nil)
    .Start; //start the pool

  //wait until the job finishes (await in this case will call APool.Stop)
  Await(LPool);

  //now manually decrement the reference count since this simulates going
  //out of scope (we should have free the pool and the worker at this point)
  LRef := LPool._Release;

  //write status
  WriteLn(Format('TestFree::[success]:%s', [BoolToStr(LRef <= 0, True)]));
end;

begin
  //TestStartStop;
  //TestSingleTask;
  //TestTwoTasks;
  //TestSharedArgs;
  TestFree;

  //wait for user input
  ReadLn;
end.

