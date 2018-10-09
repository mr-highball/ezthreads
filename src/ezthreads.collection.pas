unit ezthreads.collection;

{$mode delphi}

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
    function GetCount: Cardinal;
    function GetGroup(const AIndex: Integer): IEZCollection;
    function GetThread(const AThreadID: String): IEZThread;
    function GetThreadGroups: TStringArray;
  strict protected
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
  end;

implementation
uses
  syncobjs;
var
  Critical : TCriticalSection;

{ TEZCollectionImpl }

function TEZCollectionImpl.GetCount: Cardinal;
begin

end;

function TEZCollectionImpl.GetGroup(const AIndex: Integer): IEZCollection;
begin

end;

function TEZCollectionImpl.GetThread(const AThreadID: String): IEZThread;
begin

end;

function TEZCollectionImpl.GetThreadGroups: TStringArray;
begin

end;

procedure TEZCollectionImpl.Add(const AThread: IEZThread);
begin

end;

procedure TEZCollectionImpl.Add(const AThread: IEZThread; out Index: Integer);
begin

end;

procedure TEZCollectionImpl.Remove(const AThread: IEZThread);
begin

end;

function TEZCollectionImpl.Exists(const AGroupID: String): Boolean;
begin

end;

function TEZCollectionImpl.Exists(const AThread: IEZThread): Boolean;
begin

end;

function TEZCollectionImpl.IndexOf(const AGroupID: String;
  out Index: Integer): Boolean;
begin

end;

initialization
  Critical:=TCriticalSection.Create;
finalization
  if Assigned(Critical) then
    Critical.Free;
end.

