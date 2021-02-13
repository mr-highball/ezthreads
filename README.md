# ezthreads
a simple and safe way to work with threads

To request features or report a bug, open a github issue with details/steps to reproduce

# Features

some features at a glance...

* argument *capturing* by using the `.AddArg()` method
* reference counted threads ensuring memory is cleaned up
* *Await()* support method (similar to c#) can wait for threads, groups, all threads, pools to complete
* ez thread pool for fixed worker size and easy to use fluent methods
* works with nested, callbacks, object methods (or all at the same time)
* events for start, stop, error, success, etc... for flexibility
* did I mention it was easy? :)

# Sample

Below is a sample pulled from the console tester application which shows a possible use for ezthreads:

```pascal
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
    LID := AThread['id'];

    //write that we got to thread (B)
    WriteLn('TestThreadDependency::ThreadB starting');

    //before doing our work, wait until thread (A) has completed
    Await(LID);

    //write to console that we finished
    WriteLn('TestThreadDependency::ThreadB finished');
  end;

begin
  //init both threads
  LThreadA := NewEZThread;
  LThreadB := NewEZThread;

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
```

Here is a sample pulled from the pool tester which shows usage if it:

```pascal
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
```

Even a simple way to synchronize events with the main thread. This is pulled from `.\test\ezthreads_tester_ui.lpr`

```pascal
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
```

![](https://github.com/mr-highball/ezthreads/blob/master/screenshots/synchronize_ui.gif)

# How To Use

1. download and install lazarus if you don't already have it (http://www.lazarus-ide.org)
1. git clone this repo
1. open ezthreads_test.lpr and attempt to compile/run (F9 Key)
    * this project shows some basic usage of the library
    * also, by going to `Toolbar -> Project\Project Options\Paths` you can copy the `other units` text to include in your own project
1. add `.\src` path to your project `other units`


**Tip Jar**
  * :dollar: BTC - bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz
  * :euro: LTC - LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6


Happy Threading :)
