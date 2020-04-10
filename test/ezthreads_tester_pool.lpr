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

  procedure TempStart(const AThread : IEZThread);
  begin
    WriteLn('Pool=', BoolToStr(AThread is IEZThreadPool, True));
    WriteLn('ThreadID=', AThread.Settings.Await.ThreadID,
      ' GroupID=', AThread.Settings.Await.GroupID);
    WriteLn('--');
  end;

  procedure TempPoolDone(const AThread : IEZThread);
  begin
    WriteLn('Pool Finished');
  end;
begin
  //flags for checking if both jobs finished
  LJobOneFinished := False;
  LJobTwoFinished := False;

  //init a pool with two workers
  LPool := NewEZThreadPool(1);
  LPool
    .Events
      .UpdateOnStartNestedCallback(TempStart)//;
      .UpdateOnStopNestedCallback(TempPoolDone).Thread.Start;

  //work two jobs
  //LPool
  //  .Queue(JobOne, nil, nil);
    //.Queue(JobTwo, nil, nil)
    //.Start; //start the pool

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
  const
    FOUND = 'found';

  //simple parallel job
  procedure JobOne(const AThread : IEZThread);
  begin
    //do some work
    Sleep(100);
  end;

  //simple parallel job
  procedure JobTwo(const AThread : IEZThread);
  begin
    //do some work
    Sleep(100);
  end;
begin
end;



begin
  TestStartStop;
  TestSingleTask;
  //TestTwoTasks;
  //TestSharedArgs;

  //wait for user input
  ReadLn;
end.

