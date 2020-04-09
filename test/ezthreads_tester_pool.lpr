{$mode delphi}
{$modeswitch nestedprocvars}

program ezthreads_tester_pool;
uses
  SysUtils,
  ezthreads,
  ezthreads.pool;

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
    //.Queue(JobTwo, nil, nil)
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
  TestTwoTasks;
  TestSharedArgs;

  //wait for user input
  ReadLn;
end.

