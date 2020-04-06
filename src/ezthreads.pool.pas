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
  ezthreads;

type

  { IEZThreadPool }
  (*
    thread pool implementation the EZ way
  *)
  IEZThreadPool = interface(IEZThread)
    ['{3EBD544C-B802-4BA0-B353-876D717142F1}']

    //property methods
    //...

    //properties
    //...

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
  strict private
  public
    type

      { TPoolWorkerThread }
      (*
        override the standard ezthread internal thread to allow
        for task allocation
      *)
      TPoolWorkerThread = class(TEZThreadImpl.TInternalThread)
        protected
          procedure Execute; override;
      end;
  strict protected

    (*
      return our worker thread
    *)
    function DoGetThreadClass: TInternalThreadClass; override;
  public
    function Queue(Const AStart:TThreadCallback;
      Const AError:TThreadCallback;Const ASuccess:TThreadCallback) : IEZThreadPool; overload;

    function Queue(Const AStart:TThreadNestedCallback;
      Const AError:TThreadNestedCallback;Const ASuccess:TThreadNestedCallback) : IEZThreadPool; overload;

    function Queue(Const AStart:TThreadMethod;
      Const AError:TThreadMethod;Const ASuccess:TThreadMethod) : IEZThreadPool; overload;

    function UpdateWorkerCount(const AWorkers : Cardinal) : IEZThreadPool;
  end;

(*
  helper method to create a new ezthreadpool
*)
function NewEZThreadPool(const AWorkers : Cardinal = 4) : IEZThreadPool;

implementation

function NewEZThreadPool(const AWorkers: Cardinal): IEZThreadPool;
begin
  Result := TEZThreadPoolImpl.Create;
  //todo - set workers
end;

{ TEZThreadPoolImpl.TPoolWorkerThread }

procedure TEZThreadPoolImpl.TPoolWorkerThread.Execute;
begin
  //todo - implement a worker
end;

{ TEZThreadPoolImpl }

function TEZThreadPoolImpl.DoGetThreadClass: TInternalThreadClass;
begin
  Result := TPoolWorkerThread;
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadCallback;
  const AError: TThreadCallback; const ASuccess: TThreadCallback): IEZThreadPool;
begin
  //todo - impl
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadNestedCallback;
  const AError: TThreadNestedCallback; const ASuccess: TThreadNestedCallback): IEZThreadPool;
begin
  //todo - impl
end;

function TEZThreadPoolImpl.Queue(const AStart: TThreadMethod;
  const AError: TThreadMethod; const ASuccess: TThreadMethod): IEZThreadPool;
begin
  //todo - impl
end;

function TEZThreadPoolImpl.UpdateWorkerCount(const AWorkers: Cardinal): IEZThreadPool;
begin
  //todo - await current jobs, stop thread, etc...
end;

end.

