unit ezthreads.functional;

{$mode delphi}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, ezthreads, fgl;

type

  { IMapResult }
  (*
    specialized IEZThread that contains a result list
  *)
  IFPGListResult<T> = interface(IEZThread)
    ['{B0984019-8951-4233-A7DB-3499A0AE9A20}']
    //property methods
    function GetResult: TFPGList<T>;

    //properties
    (*
      resulting list result from a functional call
    *)
    property Result : TFPGList<T> read GetResult;
  end;

  { TFPGListResultImpl }
  (*
    base implementation for a functional list result
  *)
  TFPGListResultImpl<T> = class(TEZThreadImpl, IFPGListResult<T>)
  private
    FResult : TFPGList<T>;
    function GetResult: TFPGList<T>;
  public
    property Result : TFPGList<T> read GetResult;
    constructor Create; override;
    destructor Destroy; override;
  end;

  (*
    a method that will be provided to a map call
  *)
  TMapMethod<T> = function (Const AThread : IEZThread; Const AItem : T) : T;

  (*
    a nested method that will be provided to a map call
  *)
  TNestedMapMethod<T> = function (Const AThread : IEZThread; Const AItem : T) : T is nested;

  (*
    will "apply" AMethod to each item of AList
    https://en.wikipedia.org/wiki/Map_(higher-order_function)
  *)
  function Map<T>(Const AList : TFPGList<T>; Const AMethod : TMapMethod<T>; Const AAutoStart : Boolean = True) : IFPGListResult<T>; overload;
  function Map<T>(Const AList : TFPGList<T>; Const AMethod : TNestedMapMethod<T>; Const AAutoStart : Boolean = True) : IFPGListResult<T>; overload;

implementation

function Map<T>(const AList: TFPGList<T>;
  const AMethod: TMapMethod<T>; Const AAutoStart : Boolean): IFPGListResult<T>;
type
  PMapMethod = ^TMapMethod<T>;

  (*
    method responsible for "applying" the mapping method
    to the input list. this assumes that the caller
    does not do any modifications to the input list and remains
    valid for the lifetime of the thread
  *)
  procedure Start(Const AThread : IEZThread);
  var
    LList : TFPGList<T>;
    P, PM : PtrInt;
    LMethod : TMapMethod<T>;
    I : Integer;
    LResult : IFPGListResult<T>;
  begin
    //cast to result type
    LResult := AThread as IFPGListResult<T>;

    if not Assigned(LResult) then
      Exit;

    //cast back to a list
    P := PtrInt(AThread['list']);
    LList := TFPGList<T>(P);

    //after cast check to see if we have a valid list
    if not Assigned(LList) then
      Exit;

    //cast back to a method
    PM := PtrInt(AThread['method']);
    LMethod := TMapMethod<T>({%H-}PMapMethod(PtrInt(PM)));

    //if the method isn't assigned, return the same list contents
    if not Assigned(LMethod) then
    begin
      IFPGListResult<T>(AThread).Result.Assign(LList);
      Exit
    end;

    //iterate the list and output to result
    for I := 0 to Pred(LList.Count) do
      LResult.Result.Add(LMethod(AThread, LList[I]));
  end;

begin
  //create a result thread
  Result := TFPGListResultImpl<T>.Create;

  //add our list arg
  Result.AddArg('list', PtrInt(AList));

  //add our method arg
  Result.AddArg('method', {%H-}PtrInt(@AMethod));

  //setup our start method
  Result.Setup(Start);

  //if the caller requested to start, then go ahead and do so
  if AAutoStart then
    Result.Start;
end;

function Map<T>(const AList: TFPGList<T>; const AMethod: TNestedMapMethod<T>;
  Const AAutoStart : Boolean): IFPGListResult<T>;
type
  PMapMethod = ^TMapMethod<T>;

  (*
    method responsible for "applying" the mapping method
    to the input list. this assumes that the caller
    does not do any modifications to the input list and remains
    valid for the lifetime of the thread
  *)
  procedure Start(Const AThread : IEZThread);
  var
    LList : TFPGList<T>;
    P, PM : PtrInt;
    LMethod : TMapMethod<T>;
    I : Integer;
    LResult : IFPGListResult<T>;
  begin
    //cast to result type
    LResult := AThread as IFPGListResult<T>;

    if not Assigned(LResult) then
      Exit;

    //cast back to a list
    P := PtrInt(AThread['list']);
    LList := TFPGList<T>(P);

    //after cast check to see if we have a valid list
    if not Assigned(LList) then
      Exit;

    //cast back to a method
    PM := PtrInt(AThread['method']);
    LMethod := TMapMethod<T>({%H-}PMapMethod(PtrInt(PM)));

    //if the method isn't assigned, return the same list contents
    if not Assigned(LMethod) then
    begin
      IFPGListResult<T>(AThread).Result.Assign(LList);
      Exit
    end;

    //iterate the list and output to result
    for I := 0 to Pred(LList.Count) do
      LResult.Result.Add(LMethod(AThread, LList[I]));
  end;

begin
  //create a result thread
  Result := TFPGListResultImpl<T>.Create;

  //add our list arg
  Result.AddArg('list', PtrInt(AList));

  //add our method arg
  Result.AddArg('method', {%H-}PtrInt(@AMethod));

  //setup our start method
  Result.Setup(Start);

  //if the caller requested to start, then go ahead and do so
  if AAutoStart then
    Result.Start;
end;


{ TFPGListResultImpl }

function TFPGListResultImpl<T>.GetResult: TFPGList<T>;
begin
  Result:=FResult;
end;

constructor TFPGListResultImpl<T>.Create;
begin
  inherited Create;
  FResult:=TFPGList<T>.Create;
end;

destructor TFPGListResultImpl<T>.Destroy;
begin
  FResult.Free;;
  inherited Destroy;
end;


end.

