{$mode delphi}
{$ModeSwitch nestedprocvars}

program ezthreads_tester_functional;

uses
  ezthreads, ezthreads.functional, fgl;

procedure TestSimpleList;
var
  LList : TFPGList<String>;
  LResult : IFPGListResult<String>;
  I: Integer;

  (*
    method passed into map to perform some simple processing
  *)
  function Start(Const AThread : IEZThread; Const AItem : String) : String;
  begin
    Result := AItem + '_modified';
  end;

begin
  //input list with a few entries
  LList := TFPGList<String>.Create;
  LList.Add('hello');
  LList.Add('World');

  WriteLn('--Before--');
  for I := 0 to Pred(LList.Count) do
    WriteLn(LList[I]);

  //now we should be able to call map with on our list
  LResult := Map<String>(LList, Start);
  
  //await the result before assigning new values to the list
  Await(LResult);

  //assign the modified values
  LList.Assign(LResult.Result);

  WriteLn('--After--');
  for I := 0 to Pred(LList.Count) do
    WriteLn(LList[I]);
  LList.Free;
end;

begin
  TestSimpleList;
end.

