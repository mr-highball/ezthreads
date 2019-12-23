# ezthreads
a simple and safe way to work with threads

To request features or report a bug, open a github issue with details/steps to reproduce

# Sample

Below is a sample pulled from the console tester application which shows a possible use for ezthreads

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
```

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
