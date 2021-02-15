{ ezthreads

  Copyright (c) 2020 mr-highball

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
unit ezthreads.pool;

{$mode delphi}
{$modeswitch nestedprocvars}
{.$define EZTHREAD_TRACE}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  fgl,
  Generics.Collections,
  ezthreads;

type

  { IEZThreadPool }
  (*
    thread pool implementation the EZ way
  *)
  IEZThreadPool = interface(IEZThread)
    ['{3EBD544C-B802-4BA0-B353-876D717142F1}']

    //property methods
    function GetWorkerGroup: String;

    //properties

    (*
      the group id associated with the workers. when awaiting, this is the
      ID that should be used for pools
    *)
    property WorkerGroupID : String read GetWorkerGroup;

    //methods

    (*
      Queue adds work to be performed to the pool. There are overloads for each
      supported type of thread method. the pool *must* be started before
      work is picked up by a worker thread

      @AStart:
        -handler for when the task is started by a worker
      @AError:
        -handler for if a task fails
      @ASuccess:
        -handler on task successfully completing
    *)
    function Queue(Const AStart:TThreadCallback;
      Const AError:TThreadCallback;Const ASuccess:TThreadCallback) : IEZThreadPool; overload;

    function Queue(Const AStart:TThreadNestedCallback;
      Const AError:TThreadNestedCallback;Const ASuccess:TThreadNestedCallback) : IEZThreadPool; overload;

    function Queue(Const AStart:TThreadMethod;
      Const AError:TThreadMethod;Const ASuccess:TThreadMethod) : IEZThreadPool; overload;

    (*
      configures the pool to use x amount of worker threads to process
      tasks. this will wait for all pending tasks to complete before
      adjusting the workers, and will properly start the pool back if the
      pool was previously started

      @AWorkers:
        -number of workers to use
    *)
    function UpdateWorkerCount(const AWorkers : Cardinal) : IEZThreadPool;
  end;


  { TEZThreadPoolImpl }
  (*
    base implementation for an IEZThreadPool
  *)
  TEZThreadPoolImpl = class(TEZThreadImpl, IEZThreadPool)
  public
    function GetWorkerGroup: String;
  public
    type
      TEZThreads = TFPGInterfacedObjectList<IEZThread>;

      { TThreadSetup }

      TThreadSetup<TMethodType> = record
      strict private
        FStart,
        FError,
        FSuccess : TMethodType;
      public
        property Start : TMethodType read FStart write FStart;
        property Error : TMethodType read FError write FError;
        property Success : TMethodType read FSuccess write FSuccess;
      end;

      TCallbackSetup = TThreadSetup<TThreadCallback>;
      TNestedCallbackSetup = TThreadSetup<TThreadNestedCallback>;
      TMethodSetup = TThreadSetup<TThreadMethod>;

      { TQueueItem }

      TQueueItem = record
      private
        FCall: TCallbackSetup;
        FMethod: TMethodSetup;
        FNested: TNestedCallbackSetup;

        procedure NilWork;
      public
        property Callback : TCallbackSetup read FCall write FCall;
        property Nested : TNestedCallbackSetup read FNested write FNested;
        property Method : TMethodSetup read FMethod write FMethod;

        constructor Create(const ACallback : TCallbackSetup); overload;
        constructor Create(const ANested : TNestedCallbackSetup); overload;
        constructor Create(const AMethod : TMethodSetup); overload;
      end;

      (*
        used internally to queue methods of various types
      *)
      TMethodQueue = TQueue<TQueueItem>;

      (*
        placeholder thread used to queue to the await collection
        serves no purpose other than to stand in line... sad life.
      *)
      IPlaceHolderThread = interface(IEZThread)
        ['{29750A1E-4F19-45B4-8692-709921EE913D}']
      end;

      (*
        implementation of a place holder thread
      *)
      TPlaceHolderThreadImpl = class(TEZThreadImpl, IPlaceHolderThread)
      end;
  strict private
    FWorking : TArray<Boolean>;
    FWorkers : TEZThreads;
    FWorkerCount : Integer;
    FWorkerGroup : String;
    FWork : TMethodQueue;
    FCritical : TCriticalSection;
    FWorkerCritical : TCriticalSection;
    FPlaceHolders : TEZThreads;

    procedure AddPlaceHolder;
    procedure RemovePlaceHolder;

    const
      ARGS = 'args';
  public
    type

      { TPoolWorkerThread }
      (*
        override the standard ezthread internal thread to allow
        for task allocation
      *)
      TPoolWorkerThread = class(TEZThreadImpl.TInternalThread)
      private
        FPool: TEZThreadPoolImpl;
      protected
        procedure Execute; override;
      public
        property Pool : TEZThreadPoolImpl read FPool write FPool;
      end;
  strict protected
    (*
      blocks until a free worker is available

      note:
        do not set the "on stop" event from this thread, as it is used
        for internal purposes. Additionally, calling this without starting
        the thread is a no-go and will lock resources
    *)
    function GetFreeWorker : IEZThread;

    (*
      return our worker thread
    *)
    function DoGetThreadClass: TInternalThreadClass; override;

    (*
      handles setting up workers
    *)
    procedure DoBeforeStart(const AThread: IEZThread); override;

    (*
      will clear workers to avoid cyclic dependencies
    *)
    procedure DoAfterStop(const AThread: IEZThread); override;

    procedure DoSetupInternalThread(const AThread: TInternalThread); override;
  public
    property WorkerGroupID : String read GetWorkerGroup;

    function Queue(Const AStart:TThreadCallback;
      Const AError:TThreadCallback;Const ASuccess:TThreadCallback) : IEZThreadPool; overload;

    function Queue(Const AStart:TThreadNestedCallback;
      Const AError:TThreadNestedCallback;Const ASuccess:TThreadNestedCallback) : IEZThreadPool; overload;

    function Queue(Const AStart:TThreadMethod;
      Const AError:TThreadMethod;Const ASuccess:TThreadMethod) : IEZThreadPool; overload;

    function UpdateWorkerCount(const AWorkers : Cardinal) : IEZThreadPool;

    constructor Create; override;
    destructor Destroy; override;
  end;

(*
  helper method to create a new ezthreadpool
*)
function NewEZThreadPool(const AWorkers : Cardinal = 4) : IEZThreadPool;

implementation
uses
  ezthreads.collection;

function NewEZThreadPool(const AWorkers: Cardinal): IEZThreadPool;
begin
  Result := TEZThreadPoolImpl.Create;
  Result.UpdateWorkerCount(AWorkers);
end;

{ TEZThreadPoolImpl.TQueueItem }

procedure TEZThreadPoolImpl.TQueueItem.NilWork;
begin
  FCall.Error := nil;
  FCall.Start := nil;
  FCall.Success := nil;

  FMethod.Error := nil;
  FMethod.Start := nil;
  FMethod.Success := nil;

  FNested.Error := nil;
  FNested.Start := nil;
  FNested.Success := nil;
end;

constructor TEZThreadPoolImpl.TQueueItem.Create(const ACallback: TCallbackSetup);
begin
  NilWork;
  FCall := ACallback;
end;

constructor TEZThreadPoolImpl.TQueueItem.Create(
  const ANested: TNestedCallbackSetup);
begin
  NilWork;
  FNested := ANested;
end;

constructor TEZThreadPoolImpl.TQueueItem.Create(const AMethod: TMethodSetup);
begin
  NilWork;
  FMethod := AMethod;
end;

{ TEZThreadPoolImpl.TPoolWorkerThread }

procedure TEZThreadPoolImpl.TPoolWorkerThread.Execute;
var
  LWork : TEZThreadPoolImpl.TQueueItem;
  LWorker : IEZThread;
  LStillWorking: Boolean;
  I: Integer;
  LArgs: PEZArgs;
  LPool: IEZThreadPool;
begin
  try
    {$IFDEF EZTHREAD_TRACE}WriteLn('PoolExecute::', Self.ClassName);{$ENDIF}
    //capture a reference to ensure the lifetime of this method
    LPool := FPool as IEZThreadPool;

    //run the start methods
    if Assigned(StartCallback) then
      StartCallback(Thread);

    if Assigned(StartNestedCallback) then
      StartNestedCallback(Thread);

    if Assigned(StartMethod) then
      StartMethod(Thread);

    if not Assigned(LPool) then
      Exit;

    //as long as a stop hasn't been requested continue to poll for work
    while not Terminated do
    begin
      //check if we have work
      if FPool.FWork.Count < 1 then
      begin
        Sleep(1);
        Continue;
      end;

      //aquire a lock and work
      FPool.FCritical.Enter;
      try
        //work must've been cleared before we got the lock
        if FPool.FWork.Count < 1 then
          Continue;

        {$IFDEF EZTHREAD_TRACE}WriteLn('PoolExecute::', Self.ClassName, ' dequeued work');{$ENDIF}

        //dequeue some work
        LWork := FPool.FWork.Dequeue;

        //validate we have at least one start method before we pull a worker
        if not (Assigned(LWork.Callback.Start)
          or Assigned(LWork.Nested.Start)
          or Assigned(LWork.Method.Start))
        then
          Continue;

        //get a worker (this blocks until one frees up)
        LWorker := FPool.GetFreeWorker;

        {$IFDEF EZTHREAD_TRACE}WriteLn('PoolExecute::', Self.ClassName, ' found worker ', LWorker.Settings.Await.ThreadID);{$ENDIF}

        //setup and start the work
        LWorker
          .Events
            .UpdateOnStart(FPool.Events.OnStart)
            .UpdateOnStartNestedCallback(FPool.Events.OnStartNestedCallback)
            .UpdateOnStartCallback(FPool.Events.OnStartCallback)
            .Thread
          .Settings
            .UpdateForceTerminate(FPool.Settings.ForceTerminate)
            .Thread
          .Setup(LWork.Callback.Start, LWork.Callback.Error, LWork.Callback.Success)
          .Setup(LWork.Nested.Start, LWork.Nested.Error, LWork.Nested.Success)
          .Setup(LWork.Method.Start, LWork.Method.Error, LWork.Method.Success);

       LArgs := PEZArgs(PtrInt(FPool[ARGS]));
       for I := 0 to High(LArgs^) do
        LWorker.AddArg(LArgs^[I].Name, LArgs^[I].Data);

       LWorker.Start;
       {$IFDEF EZTHREAD_TRACE}WriteLn('PoolExecute::', Self.ClassName, ' started worker ', LWorker.Settings.Await.ThreadID);{$ENDIF}
      finally
        FPool.FCritical.Leave;
      end;
    end;

    //terminate has been requested make sure all workers have a chance
    //to finish
    LStillWorking := False;

    repeat
      for I := 0 to High(FPool.FWorking) do
        if FPool.FWorking[I] then
          LStillWorking := True;
    until not LStillWorking;

    {$IFDEF EZTHREAD_TRACE}WriteLn('PoolExecute::', Self.ClassName, ' finished');{$ENDIF}
  except on E : Exception do
    try
      //run the error methods
      if Assigned(ErrorCallback) then
        ErrorCallback(Thread);

      if Assigned(ErrorNestedCallback) then
        ErrorNestedCallback(Thread);

      if Assigned(ErrorMethod) then
        ErrorMethod(Thread);
    finally
    end;
  end;
end;

{ TEZThreadPoolImpl }

function TEZThreadPoolImpl.GetWorkerGroup: String;
begin
  Result := FWorkerGroup;
end;

procedure TEZThreadPoolImpl.AddPlaceHolder;
var
  LThread : IPlaceHolderThread;
begin
 {$IFDEF EZTHREAD_TRACE}WriteLn('AddPlaceHolder::', Self.ClassName);{$ENDIF}
  LThread := TPlaceHolderThreadImpl.Create;

  //update the group id to that of the workers
  LThread.Settings.Await.UpdateGroupID(FWorkerGroup);

  FCritical.Enter;
  try
    //add the thread id to the place holder list
    FPlaceHolders.Add(LThread);

    //add the thread to the await collection
    AddThreadToAwaitCollection(LThread);
  finally
    FCritical.Leave;
  end;
end;

procedure TEZThreadPoolImpl.RemovePlaceHolder;
var
  LPlaceHolder : IPlaceHolderThread;
begin
 {$IFDEF EZTHREAD_TRACE}WriteLn('RemovePlaceHolder::', Self.ClassName);{$ENDIF}
  FCritical.Enter;
  try
    if FPlaceHolders.Count < 1 then
      Exit;

    LPlaceHolder := FPlaceHolders.Items[0] as IPlaceHolderThread;

    //remove the placeholder from the collection
    RemoveThreadFromAwaitCollection(LPlaceHolder);

    //delete from thread list
    FPlaceHolders.Delete(0);
  finally
    FCritical.Leave;
  end;
end;

function TEZThreadPoolImpl.GetFreeWorker: IEZThread;
var
  I: Integer;
begin
  //allow dirty reads, just find the first available free worker
  while True do
    for I := 0 to High(FWorking) do
    begin
      if not FWorking[I] then
      begin
        //now that we found one, aquire a lock to ensure another thread hasn't '
        //picked this worker up
        FWorkerCritical.Enter;
        try
          if FWorking[I] then
            Continue;

          //mark as working and fetch the free worker
          FWorking[I] := True;
          Result := FWorkers[I];
        finally
          FWorkerCritical.Leave;
        end;

        //add the worker to the group for await support
        AddThreadToAwaitCollection(Result);

        //now that we've taken the place of a placeholder, remove one
        RemovePlaceHolder;

        Exit;
      end;
    end;
end;

function TEZThreadPoolImpl.DoGetThreadClass: TInternalThreadClass;
begin
  Result := TPoolWorkerThread;
end;

procedure TEZThreadPoolImpl.DoBeforeStart(const AThread: IEZThread);
begin
  inherited DoBeforeStart(AThread);

  //if a pool was stopped and didn't get released, this will ensure
  //the proper worker count gets updated before starting the pool
  if FWorkerCount <> FWorkers.Count then
    UpdateWorkerCount(FWorkerCount);
end;

procedure TEZThreadPoolImpl.DoAfterStop(const AThread: IEZThread);
var
  I: Integer;
  LWorker: IEZThread;
begin
  inherited DoAfterStop(AThread);

  if not Assigned(FWorkers) then
    Exit;

  //don't clear the workers until they're finished
  for I := 0 to Pred(FWorkers.Count) do
  begin
    LWorker := FWorkers[I];
    LWorker.Stop;

    while LWorker.State = esStarted do
      Continue;
  end;

  //clear workers
  FWorkers.Clear;
end;

procedure TEZThreadPoolImpl.DoSetupInternalThread(const AThread: TInternalThread);
begin
  inherited DoSetupInternalThread(AThread);
  TPoolWorkerThread(AThread).Pool := Self;
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadCallback;
  const AError: TThreadCallback; const ASuccess: TThreadCallback): IEZThreadPool;
var
  LWork : TQueueItem;
  LCall : TCallbackSetup;
begin
  Result := Self;

  //adds a placeholder thread
  AddPlaceHolder;

  //queue up a callback work item
  FCritical.Enter;
  try
    //initialize work
    LCall.Start := AStart;
    LCall.Error := AError;
    LCall.Success := ASuccess;

    //create
    LWork := TQueueItem.Create(LCall);

    //queue the work
    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadNestedCallback;
  const AError: TThreadNestedCallback; const ASuccess: TThreadNestedCallback): IEZThreadPool;
var
  LWork : TQueueItem;
  LNested : TNestedCallbackSetup;
begin
  Result := Self;

  //adds a placeholder thread
  AddPlaceHolder;

  //queue up a callback work item
  FCritical.Enter;
  try
    //initialize work
    LNested.Start := AStart;
    LNested.Error := AError;
    LNested.Success := ASuccess;

    //create
    LWork := TQueueItem.Create(LNested);

    //queue the work
    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadMethod;
  const AError: TThreadMethod; const ASuccess: TThreadMethod): IEZThreadPool;
var
  LWork : TQueueItem;
  LMethod : TMethodSetup;
begin
  Result := Self;

  //adds a placeholder thread
  AddPlaceHolder;

  //queue up a callback work item
  FCritical.Enter;
  try
    //initialize work
    LMethod.Start := AStart;
    LMethod.Error := AError;
    LMethod.Success := ASuccess;

    //create
    LWork := TQueueItem.Create(LMethod);

    //queue the work
    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TEZThreadPoolImpl.UpdateWorkerCount(const AWorkers: Cardinal): IEZThreadPool;
var
  LStarted : Boolean;
  LCount : Cardinal;
  I: Integer;
  LThread : IEZThread;

const
  INTERNAL_INDEX = 'internal_lock_index';
  POOL = 'pool';
  (*
    when a worker stops, write to the working array
  *)
  procedure WorkerStop(const AThread : IEZThread);
  var
    I : Integer;
    LThread : IEZThread;
    LSelf: TEZThreadPoolImpl;
  begin
    //local ref
    LThread := AThread;

    //self pointer won't work here, so cast the pool arg
    LSelf := TEZThreadPoolImpl(Pointer(PtrInt(LThread[POOL])));

    {$IFDEF EZTHREAD_TRACE}WriteLn('WorkerStop::', LSelf.ClassName, '[id]:', LThread.Settings.Await.ThreadID);{$ENDIF}
    I := LThread[INTERNAL_INDEX];
    LSelf.FWorking[I] := False;
  end;

begin
  Result := Self;
  FWorkerCount := AWorkers;

  //can't have zero workers... I mean you could, that would just be stupid though
  if FWorkerCount < 1 then
    FWorkerCount := 1;

  //await current jobs
  ezthreads.Await(FWorkerGroup);

  //record prior state to see if we need to start back up
  LStarted := State = esStarted;

  //stop monitoring for queued jobs
  if LStarted then
    Stop;

  //set the working array to the worker count and fill as false
  SetLength(FWorking, FWorkerCount);

  //these should be defaulted to false, but just to make sure
  for I := 0 to Pred(FWorkerCount) do
    FWorking[I] := False;

  //clear workers
  FWorkers.Clear;

  //populate workers
  for I := 0 to Pred(FWorkerCount) do
  begin
    //add a new thread, making sure to copy the pool's settings that apply
    LThread := NewEZThread;
    LThread
      .Settings
        .UpdateSynchronizeStopEvents(Settings.SynchronizeStopEvents)
        .UpdateMaxRuntime(Settings.MaxRuntime)
        .Await
          .UpdateGroupID(FWorkerGroup)
          .Thread
        //update the index of the worker and set the stop callback
        .AddArg(INTERNAL_INDEX, I)
        .AddArg(POOL, PtrInt(Pointer(Self)))
        .Events
          .UpdateOnStopNestedCallback(WorkerStop).Thread;

    FWorkers.Add(LThread);
    LThread := nil;//nil local ref since we re-use the variable
  end;

  //start back up if we were already started
  if LStarted then
    Start;
end;

constructor TEZThreadPoolImpl.Create;
begin
  inherited Create;
  FPlaceHolders := TEZThreads.Create;
  FWork := TMethodQueue.Create;
  FCritical := TCriticalSection.Create;
  FWorkerCritical := TCriticalSection.Create;
  FWorkers := TEZThreads.Create;
  FWorkerGroup := TGuid.NewGuid.ToString;
  AddArg(ARGS, PtrInt(GetArgs));
end;

destructor TEZThreadPoolImpl.Destroy;
begin
  {$IFDEF EZTHREAD_TRACE}WriteLn('PoolDestroy::', Self.ClassName);{$ENDIF}
  Stop;
  FreeAndNil(FWork);
  FreeAndNil(FCritical);
  FreeAndNil(FWorkerCritical);
  FreeAndNil(FWorkers);
  FreeAndNil(FPlaceHolders);
  {$IFDEF EZTHREAD_TRACE}WriteLn('PoolDead::', Self.ClassName);{$ENDIF}
  inherited Destroy;
end;

end.

