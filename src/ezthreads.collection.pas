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
unit ezthreads.collection;

{$mode delphi}
{.$define EZTHREAD_TRACE}

interface

uses
  Classes, SysUtils, fgl, ezthreads;

type

  { IEZCollection }
  (*
    thread safe collection to hold ezthreads in groups
  *)
  IEZCollection = interface
    ['{8C7102EE-F1E2-4380-B057-B52B0844ED9E}']
    //property methods
    function GetCount: Cardinal;
    function GetGroup(const AIndex: Integer): IEZCollection;
    function GetThread(const AThreadID: String): IEZThread;
    function GetThreadGroups: TStringArray;

    //properties
    property ThreadGroups : TStringArray read GetThreadGroups;
    property Count : Cardinal read GetCount;
    (*
      returns a split group provided an index
    *)
    property Groups[Const AIndex:Integer] : IEZCollection read GetGroup;
    property Threads[Const AThreadID:String] : IEZThread read GetThread;

    //methods
    procedure Add(Const AThread:IEZThread);overload;
    procedure Add(Const AThread:IEZThread;Out Index:Integer);overload;
    procedure Remove(Const AThread:IEZThread);
    function Exists(Const AGroupID:String):Boolean;overload;
    function Exists(Const AThread:IEZThread):Boolean;overload;
    function IndexOf(Const AGroupID:String;Out Index:Integer):Boolean;overload;
  end;

  { TEZCollectionImpl }
  (*
    base implementation of an ezthread collection
  *)
  TEZCollectionImpl = class(TInterfacedObject,IEZCollection)
  strict private
    type
      //stores threads of a thread group
      TThreadGroup = TFPGInterfacedObjectList<IEZThread>;

      //stores thread groups, reference by group id
      TGroupMap = TFPGMapObject<String,TThreadGroup>;
  strict private
    FGroups: TGroupMap;
    function GetCount: Cardinal;
    function GetGroup(const AIndex: Integer): IEZCollection;
    function GetThread(const AThreadID: String): IEZThread;
    function GetThreadGroups: TStringArray;
  strict protected
    (*
      adds thread to group without aquiring lock
    *)
    procedure UnsafeAdd(Const AThread:IEZThread;Out Index:Integer);
  public
    property ThreadGroups : TStringArray read GetThreadGroups;
    property Count : Cardinal read GetCount;
    property Groups[Const AIndex:Integer] : IEZCollection read GetGroup;
    property Threads[Const AThreadID:String] : IEZThread read GetThread;

    procedure Add(Const AThread:IEZThread);overload;
    procedure Add(Const AThread:IEZThread;Out Index:Integer);overload;
    procedure Remove(Const AThread:IEZThread);
    function Exists(Const AGroupID:String):Boolean;overload;
    function Exists(Const AThread:IEZThread):Boolean;overload;
    function IndexOf(Const AGroupID:String;Out Index:Integer):Boolean;overload;
    constructor Create;virtual;
    destructor Destroy; override;
  end;

implementation
uses
  syncobjs;
var
  Critical : TCriticalSection;

{ TEZCollectionImpl }

function TEZCollectionImpl.GetCount: Cardinal;
begin
  Critical.Enter;
  try
    Result:=FGroups.Count;
  finally
    Critical.Leave;
  end;
end;

function TEZCollectionImpl.GetGroup(const AIndex: Integer): IEZCollection;
var
  I,J:Integer;
  LGroup:TThreadGroup;
  LResult:TEZCollectionImpl;
begin
  //create a new collection
  LResult:=TEZCollectionImpl.Create;
  Result:=LResult;

  //no need to enter if no groups exist
  if AIndex < 0 then
    Exit;

  Critical.Enter;
  try
    //one more check to make sure we aren't out of bounds after aquiring lock
    if AIndex > Pred(FGroups.Count) then
      Exit;

    LGroup:=FGroups.Data[AIndex];

    //iterate group and add thread to result collection
    for I := 0 to Pred(LGroup.Count) do
      LResult.UnsafeAdd(LGroup[I],J);
  finally
    Critical.Leave;
  end;
end;

function TEZCollectionImpl.GetThread(const AThreadID: String): IEZThread;
var
  I,J:Integer;
begin
  Result:=nil;

  Critical.Enter;
  try
    //iterate all groups, and items in the group until we find a matching id
    for I := 0 to Pred(FGroups.Count) do
      for J := 0 to Pred(FGroups.Data[I].Count) do
        if FGroups.Data[I].Items[J].Settings.Await.ThreadID = AThreadID then
        begin
          Result:=FGroups.Data[I].Items[J];
          Exit;
        end;
  finally
    Critical.Leave;
  end;
end;

function TEZCollectionImpl.GetThreadGroups: TStringArray;
var
  I:Integer;
begin
  Critical.Enter;
  try
    SetLength(Result,FGroups.Count);
    for I := 0 to Pred(FGroups.Count) do
      Result[I]:=FGroups.Keys[I];
  finally
    Critical.Leave;
  end;
end;

procedure TEZCollectionImpl.UnsafeAdd(const AThread: IEZThread;Out Index:Integer);
var
  I:Integer;
  LGroup:TThreadGroup;
  LID:String;
  LThread,
  LGroupThread : IEZThread;
begin
  LThread:=AThread;

  //get the group id and look inside to see if we found it
  LID:=LThread.Settings.Await.GroupID;
  Index:=FGroups.IndexOf(LID);

  //if this thread group already exists, just add to it
  if Index >= 0 then
  begin
    //now get the thread id to search the group
    LID := LThread.Settings.Await.ThreadID;
    LGroup:=FGroups.Data[Index];

    //only add to the group if we haven't already done so
    for I:=0 to Pred(LGroup.Count) do
    begin
      //grab a ref to the group thread
      LGroupThread := LGroup.Items[I];

      if not Assigned(LGroupThread) then
        Continue;

      //check if this item matches the input's thread id, if so we can
      //avoid the add altogether
      if LGroupThread.Settings.Await.ThreadID = LID then
        Exit;
    end;

    //otherwise the thread id did not exist, go ahead and add it
    LGroup.Add(LThread);
  end
  else
  begin
    //if the thread group doesn't exist, create it and
    //add the thread to the group
    LGroup:=TThreadGroup.Create;
    LGroup.Add(LThread);
    Index:=FGroups.Add(
      LID,
      LGroup
    );
  end;
end;

procedure TEZCollectionImpl.Add(const AThread: IEZThread);
var
  I:Integer;
begin
  Add(AThread,I);
end;

procedure TEZCollectionImpl.Add(const AThread: IEZThread; out Index: Integer);
begin
  Critical.Enter;
  try
    UnsafeAdd(AThread,Index);
  finally
    Critical.Leave;
  end;
end;

procedure TEZCollectionImpl.Remove(const AThread: IEZThread);
var
  I,J,K:Integer;
  LGroup:TThreadGroup;
  LThreadID:String;
begin
  LThreadID:=AThread.Settings.Await.ThreadID;
  if IndexOf(AThread.Settings.Await.GroupID,I) then
  begin
    Critical.Enter;
    try
      LGroup:=FGroups.Data[I];

      K:=-1;
      //find the index of the thread in the group
      for J:=0 to Pred(LGroup.Count) do
        if LGroup.Items[J].Settings.Await.ThreadID = LThreadID then
        begin
          K:=J;
          break;
        end;

      //delete the thread from the group
      if K >= 0 then
        LGroup.Delete(K);

      //if there are no more threads in this group, delete the group
      if LGroup.Count < 1 then
        FGroups.Delete(I);
    finally
      Critical.Leave;
    end;
  end;
end;

function TEZCollectionImpl.Exists(const AGroupID: String): Boolean;
var
  I:Integer;
begin
  Result:=IndexOf(AGroupID,I);
end;

function TEZCollectionImpl.Exists(const AThread: IEZThread): Boolean;
var
  I:Integer;
begin
  Result:=IndexOf(AThread.Settings.Await.GroupID,I);
end;

function TEZCollectionImpl.IndexOf(const AGroupID: String;
  out Index: Integer): Boolean;
begin
  Result:=False;

  Critical.Enter;
  try
    Index:=FGroups.IndexOf(AGroupID);
    Result:=Index >= 0;
  finally
    Critical.Leave;
  end;
end;

constructor TEZCollectionImpl.Create;
begin
  FGroups:=TGroupMap.Create(True);
end;

destructor TEZCollectionImpl.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

initialization
  Critical:=TCriticalSection.Create;
finalization
  if Assigned(Critical) then
    Critical.Free;
end.

