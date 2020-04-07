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
      public
        property Callback : TCallbackSetup read FCall write FCall;
        property Nested : TNestedCallbackSetup read FNested write FNested;
        property Method : TMethodSetup read FMethod write FMethod;
      end;

      (*
        used internally to queue methods of various types
      *)
      TMethodQueue = TQueue<TQueueItem>;
  strict private
    FWorking : TArray<Boolean>;
    FWorkers : TEZThreads;
    FWorkerGroup : String;
    FWork : TMethodQueue;
    FCritical : TCriticalSection;
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

function NewEZThreadPool(const AWorkers: Cardinal): IEZThreadPool;
begin
  Result := TEZThreadPoolImpl.Create;
  Result.UpdateWorkerCount(AWorkers);
end;

{ TEZThreadPoolImpl.TPoolWorkerThread }

procedure TEZThreadPoolImpl.TPoolWorkerThread.Execute;
var
  LWork : TEZThreadPoolImpl.TQueueItem;
  LWorker : IEZThread;
begin
  try
    try
      //run the start methods
      if Assigned(StartCallback) then
        StartCallback(Thread);

      if Assigned(StartNestedCallback) then
        StartNestedCallback(Thread);

      if Assigned(StartMethod) then
        StartMethod(Thread);

      //check for terminated
      if Terminated then
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

          //setup and start the work
          LWorker
            .Setup(LWork.Callback.Start, LWork.Callback.Error, LWork.Callback.Error)
            .Setup(LWork.Nested.Start, LWork.Nested.Error, LWork.Nested.Error)
            .Setup(LWork.Method.Start, LWork.Method.Error, LWork.Method.Error)
            .Start
        finally
          FPool.FCritical.Leave;
        end;
      end;
    except on E : Exception do
      try
        if Terminated then
          Exit;

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
  finally
    try
      RaiseStopEvents;
    finally
    end;
  end;
end;

{ TEZThreadPoolImpl }

function TEZThreadPoolImpl.GetWorkerGroup: String;
begin
  Result := FWorkerGroup;
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
        FWorking[I] := True;
        Result := FWorkers[I];
        Exit;
      end;
    end;
end;

function TEZThreadPoolImpl.DoGetThreadClass: TInternalThreadClass;
begin
  Result := TPoolWorkerThread;
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
begin
  Result := Self;

  //queue up a callback work item
  FCritical.Enter;
  try
    LWork.Callback.Start := AStart;
    LWork.Callback.Error := AError;
    LWork.Callback.Success := ASuccess;

    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadNestedCallback;
  const AError: TThreadNestedCallback; const ASuccess: TThreadNestedCallback): IEZThreadPool;
var
  LWork : TQueueItem;
begin
  Result := Self;

  //queue up a nested work item
  FCritical.Enter;
  try
    LWork.Nested.Start := AStart;
    LWork.Nested.Error := AError;
    LWork.Nested.Success := ASuccess;

    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadMethod;
  const AError: TThreadMethod; const ASuccess: TThreadMethod): IEZThreadPool;
var
  LWork : TQueueItem;
begin
  Result := Self;

  //queue up a object method work item
  FCritical.Enter;
  try
    LWork.Method.Start := AStart;
    LWork.Method.Error := AError;
    LWork.Method.Success := ASuccess;

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

  (*
    when a worker stops, write to the working array
  *)
  procedure WorkerStop(const AThread : IEZThread);
  begin
    FWorking[AThread[INTERNAL_INDEX]] := False;
  end;

begin
  Result := Self;
  LCount := AWorkers;

  //can't have zero workers... I mean you could, that would just be stupid though
  if LCount < 1 then
    LCount := 1;

  //await current jobs
  ezthreads.Await(FWorkerGroup);

  //record prior state to see if we need to start back up
  LStarted := State = esStarted;

  //stop monitoring for queued jobs
  Stop;

  //set the working array to the worker count and fill as false
  SetLength(FWorking, LCount);

  //these should be defaulted to false, but just to make sure
  for I := 0 to Pred(LCount) do
    FWorking[I] := False;

  //clear workers
  FWorkers.Clear;

  //populate workers
  for I := 0 to Pred(LCount) do
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
        .AddArg(INTERNAL_INDEX, I).Events.UpdateOnStopNestedCallback(WorkerStop);

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
  FWork := TMethodQueue.Create;
  FCritical := TCriticalSection.Create;
  FWorkers := TEZThreads.Create;
  FWorkerGroup := TGuid.NewGuid.ToString;
end;

destructor TEZThreadPoolImpl.Destroy;
begin
  Stop;
  FWork.Free;
  FCritical.Free;
  inherited Destroy;
end;

end.
