unit ezthreads;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, variants;

type

  //forward
  IEZThread = interface;

  (*
    callback method for ezthreads
  *)
  TThreadCallback = procedure(Const AThread:IEZThread);

  (*
    object method for exthreads
  *)
  TThreadMethod = procedure(Const AThread:IEZThread) of object;

  (*
    callback method for cleaning up arguments
  *)
  TArgCleanupCallback = procedure(Const AArg:Variant);

  (*
    object method for cleaning up arguments
  *)
  TArgCleanupMethod = procedure(Const AArg:Variant) of object;

  TVariantArray = array of Variant;


  { IEZThreadSettings }
  (*
    settings for an ezthread
  *)
  IEZThreadSettings = interface
    ['{1EE5C618-DA56-4101-BE75-1FD25F131ED2}']
    //property methods
    function GetMaxRunTime: Cardinal;
    function GetThread: IEZThread;

    //properties
    (*
      maximum time a method is allowed to run without being aborted
    *)
    property MaxRuntime : Cardinal read GetMaxRunTime;

    (*
      parent thread these settings belong to
    *)
    property Thread : IEZThread read GetThread;

    //methods
    function UpdateMaxRuntime(Const ARuntime:Cardinal):IEZThreadSettings;
  end;

  (*
    IEZThread provides a simple to use and flexible interface
    for performing tasks that require multithreading
  *)
  IEZThread = interface
    ['{998776BC-86C1-432D-B864-BC3A5FF6860A}']
    //property methods
    function GetSettings: IEZThreadSettings;

    //properties
    property Settings:IEZThreadSettings read GetSettings;

    //methods
    (*
      add a single argument to this ezthread and an optional
      cleanup method
    *)
    function AddArg(Const AName:String;Const AArg:Variant;
      OnFinish:TArgCleanupCallback=nil):IEZThread;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      OnFinish:TArgCleanupMethod=nil):IEZThread;overload;

    (*
      add several arguments at one time, note that this doesn't offer
      a cleanup method, so if an argument requires one, utilize the single
      add method
    *)
    function AddArgs(Const ANames:TStringArray;
      Const AArgs:TVariantArray):IEZThread;

    (*
      sets up the thread with methods to run for starting/error/success
    *)
    function Setup(Const AStart:TThreadCallback;
      Const AError:TThreadCallback=nil;Const ASuccess:TThreadCallback=nil):IEZThread;overload;
    function Setup(Const AStart:TThreadMethod;
      Const AError:TThreadMethod=nil;Const ASuccess:TThreadMethod=nil):IEZThread;overload;

    (*
      starts the thread
    *)
    procedure Start;
  end;

  { TEZArg }
  (*
    simple structure to hold argument data and callback methods
  *)
  TEZArg = packed record
  private
    FCallback: TArgCleanupCallback;
    FData: Variant;
    FMethod: TArgCleanupMethod;
    FName: String;
  public
    property Name : String read FName;
    property Data : Variant read FData;
    property Callback : TArgCleanupCallback read FCallback;
    property Method : TArgCleanupMethod read FMethod;
    constructor Create(Const AName:String;Const AData:Variant;
      Const ACallback:TArgCleanupCallback;Const AMethod:TArgCleanupMethod);
  end;

  (*
    array of arguments
  *)
  TEZArgs = array of TEZArg;

implementation

{ TEZArg }

constructor TEZArg.Create(const AName: String; const AData:Variant;
  const ACallback: TArgCleanupCallback; const AMethod: TArgCleanupMethod);
begin
  FName:=AName;
  FData:=AData;
  FMethod:=AMethod;
  FCallback:=ACallback;
end;

end.

