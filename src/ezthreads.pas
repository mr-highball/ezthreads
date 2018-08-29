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
    object method for ezthreads
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
    settings for an ezthread that deal with the operation of the thread
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

  { IEZThreadEvents }
  (*
    all events that can be registered with an ezthread
  *)
  IEZThreadEvents = interface
    ['{7709EF05-BC0F-41A2-AB2B-2FCD8F305D8D}']
    //property methods
    function GetOnStart: TThreadMethod;
    function GetOnStartCall: TThreadCallback;
    function GetOnStop: TThreadMethod;
    function GetOnStopCall: TThreadCallback;
    function GetThread: IEZThread;

    //properties
    (*
      called once the thread successfully starts
    *)
    property OnStart : TThreadMethod read GetOnStart;
    property OnStartCallback : TThreadCallback read GetOnStartCall;

    (*
      called once the thread successfully stops either by force or by
      finishing gracefully
    *)
    property OnStop : TThreadMethod read GetOnStop;
    property OnStopCallback : TThreadCallback read GetOnStopCall;

    (*
      parent thread these settings belong to
    *)
    property Thread : IEZThread read GetThread;

    //methods
    function UpdateOnStart(Const AOnStart:TThreadMethod):IEZThreadEvents;
    function UpdateOnStartCallback(Const AOnStart:TThreadCallback):IEZThreadEvents;
    function UpdateOnStop(Const AOnStop:TThreadMethod):IEZThreadEvents;
    function UpdateOnStopCallback(Const AOnStop:TThreadCallback):IEZThreadEvents;
  end;

  (*
    IEZThread provides a simple to use and flexible interface
    for performing multithreaded tasks
  *)
  IEZThread = interface
    ['{998776BC-86C1-432D-B864-BC3A5FF6860A}']
    //property methods
    function GetSettings: IEZThreadSettings;
    function GetEvents: IEZThreadEvents;

    //properties
    property Settings:IEZThreadSettings read GetSettings;
    property Events:IEZThreadEvents read GetEvents;

    //methods
    (*
      add a single argument to this ezthread and an optional
      cleanup method (will overwrite if name exists)
    *)
    function AddArg(Const AName:String;Const AArg:Variant;
      OnFinish:TArgCleanupCallback):IEZThread;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      OnFinish:TArgCleanupMethod):IEZThread;overload;
    function AddArg(Const AName:String;Const AArg:Variant):IEZThread;overload;
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
      Const AError:TThreadCallback;Const ASuccess:TThreadCallback):IEZThread;overload;
    function Setup(Const AStart:TThreadCallback):IEZThread;overload;

    function Setup(Const AStart:TThreadMethod;
      Const AError:TThreadMethod;Const ASuccess:TThreadMethod):IEZThread;overload;
    function Setup(Const AStart:TThreadMethod):IEZThread;overload;

    (*
      starts the thread
    *)
    procedure Start;

    (*
      stops thread if started
    *)
    procedure Stop;
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

  { TEZThreadImpl }
  (*
    base implementation of an IEZThread, also realizes settings and events
    to keep things together
  *)
  TEZThreadImpl = class(
    TInterfacedObject,
    IEZThreadEvents,
    IEZThreadSettings,
    IEZThread
  )
  strict private
    FMaxRunTime: Cardinal;
    FOnStart,
    FOnStop: TThreadMethod;
    FOnStartCall,
    FOnStopCall: TThreadCallback;
    FArgs: TEZArgs;
    function GetMaxRunTime: Cardinal;
    function GetOnStart: TThreadMethod;
    function GetOnStartCall: TThreadCallback;
    function GetOnStop: TThreadMethod;
    function GetOnStopCall: TThreadCallback;
    function GetThread: IEZThread;
    function GetSettings: IEZThreadSettings;
    function GetEvents: IEZThreadEvents;
  strict protected
  public
    //events
    property OnStart : TThreadMethod read GetOnStart;
    property OnStartCallback : TThreadCallback read GetOnStartCall;
    property OnStop : TThreadMethod read GetOnStop;
    property OnStopCallback : TThreadCallback read GetOnStopCall;
  public
    //properties
    property MaxRuntime : Cardinal read GetMaxRunTime;
    property Thread : IEZThread read GetThread;
    property Settings:IEZThreadSettings read GetSettings;
    property Events:IEZThreadEvents read GetEvents;

    //methods
    function UpdateOnStart(Const AOnStart:TThreadMethod):IEZThreadEvents;
    function UpdateOnStartCallback(Const AOnStart:TThreadCallback):IEZThreadEvents;
    function UpdateOnStop(Const AOnStop:TThreadMethod):IEZThreadEvents;
    function UpdateOnStopCallback(Const AOnStop:TThreadCallback):IEZThreadEvents;
    function UpdateMaxRuntime(Const ARuntime:Cardinal):IEZThreadSettings;
    function AddArg(Const AName:String;Const AArg:Variant;
      OnFinish:TArgCleanupCallback):IEZThread;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      OnFinish:TArgCleanupMethod):IEZThread;overload;
    function AddArg(Const AName:String;Const AArg:Variant):IEZThread;overload;
    function AddArgs(Const ANames:TStringArray;
      Const AArgs:TVariantArray):IEZThread;
    function Setup(Const AStart:TThreadCallback;
      Const AError:TThreadCallback;Const ASuccess:TThreadCallback):IEZThread;overload;
    function Setup(Const AStart:TThreadCallback):IEZThread;overload;
    function Setup(Const AStart:TThreadMethod;
      Const AError:TThreadMethod;Const ASuccess:TThreadMethod):IEZThread;overload;
    function Setup(Const AStart:TThreadMethod):IEZThread;overload;
    procedure Start;
    procedure Stop;
    constructor Create;virtual;
  end;

implementation

{ TEZThreadImpl }

function TEZThreadImpl.GetMaxRunTime: Cardinal;
begin
  Result:=FMaxRunTime;
end;

function TEZThreadImpl.GetOnStart: TThreadMethod;
begin
  Result:=FOnStart;
end;

function TEZThreadImpl.GetOnStartCall: TThreadCallback;
begin
  Result:=FOnStartCall;
end;

function TEZThreadImpl.GetOnStop: TThreadMethod;
begin
  Result:=FOnStop;
end;

function TEZThreadImpl.GetOnStopCall: TThreadCallback;
begin
  Result:=FOnStopCall;
end;

function TEZThreadImpl.GetThread: IEZThread;
begin
  Result:=Self as IEZThread;
end;

function TEZThreadImpl.GetSettings: IEZThreadSettings;
begin
  Result:=Self as IEZThreadSettings;
end;

function TEZThreadImpl.GetEvents: IEZThreadEvents;
begin
  Result:=Self as IEZThreadEvents;
end;

function TEZThreadImpl.UpdateOnStart(const AOnStart: TThreadMethod
  ): IEZThreadEvents;
begin
  FOnStart:=AOnStart;
  Result:=GetEvents;
end;

function TEZThreadImpl.UpdateOnStartCallback(const AOnStart: TThreadCallback
  ): IEZThreadEvents;
begin
  FOnStartCall:=AOnStart;
  Result:=GetEvents;
end;

function TEZThreadImpl.UpdateOnStop(const AOnStop: TThreadMethod
  ): IEZThreadEvents;
begin
  FOnStop:=AOnStop;
  Result:=GetEvents;
end;

function TEZThreadImpl.UpdateOnStopCallback(const AOnStop: TThreadCallback
  ): IEZThreadEvents;
begin
  FOnStopCall:=AOnStop;
  Result:=GetEvents;
end;

function TEZThreadImpl.UpdateMaxRuntime(const ARuntime: Cardinal
  ): IEZThreadSettings;
begin
  FMaxRunTime:=ARunTime;
  Result:=GetSettings;
end;

function TEZThreadImpl.AddArg(const AName: String; const AArg: Variant;
  OnFinish: TArgCleanupCallback): IEZThread;
begin
  //todo
  Result:=GetThread;
end;

function TEZThreadImpl.AddArg(const AName: String; const AArg: Variant;
  OnFinish: TArgCleanupMethod): IEZThread;
begin
  //todo
  Result:=GetThread;
end;

function TEZThreadImpl.AddArg(const AName: String; const AArg: Variant
  ): IEZThread;
begin
  Result:=AddArg(AName,AArg,TArgCleanupCallback(nil));
end;

function TEZThreadImpl.AddArgs(const ANames: TStringArray;
  const AArgs: TVariantArray): IEZThread;
var
  I:Integer;
begin
  //assumes arrays are equal
  for I:=0 to High(AArgs) do
    AddArg(ANames[I],AArgs[I]);
  Result:=GetThread;
end;

function TEZThreadImpl.Setup(const AStart: TThreadCallback;
  const AError: TThreadCallback; const ASuccess: TThreadCallback): IEZThread;
begin
  //todo
  Result:=GetThread;
end;

function TEZThreadImpl.Setup(const AStart: TThreadCallback): IEZThread;
begin
  Result:=Setup(AStart,TThreadCallback(nil),TThreadCallback(nil));
end;

function TEZThreadImpl.Setup(const AStart: TThreadMethod;
  const AError: TThreadMethod; const ASuccess: TThreadMethod): IEZThread;
begin
  //todo
  Result:=GetThread;
end;

function TEZThreadImpl.Setup(const AStart: TThreadMethod): IEZThread;
begin
  Result:=Setup(AStart,TThreadMethod(nil),TThreadMethod(nil));
end;

procedure TEZThreadImpl.Start;
begin
  //todo
end;

procedure TEZThreadImpl.Stop;
begin
  //todo
end;

constructor TEZThreadImpl.Create;
begin
  FMaxRunTime:=0;
  FOnStart:=nil;
  FOnStop:=nil;
  FOnStartCall:=nil;
  FOnStopCall:=nil;
  SetLength(FArgs,0);
end;

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

