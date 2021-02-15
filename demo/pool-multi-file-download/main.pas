{ ezthreads

  Copyright (c) 2021 mr-highball

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
unit main;

{$mode delphi}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  ezthreads,
  ezthreads.pool;

type

  { TMultiFileDownload }
  (*
    shows a possible use case for ezthread pool
    https://github.com/mr-highball/ezthreads/discussions/13
  *)
  TMultiFileDownload = class(TForm)
    btn_clear: TButton;
    btn_download: TButton;
    edit_workers: TLabeledEdit;
    lbl_instructions: TLabel;
    memo_filelist: TMemo;
    pnl_ctrls: TPanel;
    progress: TProgressBar;
    procedure btn_clearClick(Sender: TObject);
    procedure btn_downloadClick(Sender: TObject);
  public
    type
      TFileArray = array of TBytes;
  private
    FCurrentProgress: Integer;
    FTotalBytes : Integer;
    FIndividualProgress : TArray<Int64>;
    FFiles : TFileArray;

    procedure DataReceived(Sender: TObject; const ContentLength,
      CurrentPos: Int64);
    function GetFiles: TStrings;
    function GetTotalBytes(const ATotalVarName : String) : IEZThread;
    procedure DisableUI;
    procedure EnableUI;
    procedure UpdateProgressBar;
  public
    property Files : TStrings read GetFiles;
    property FileData : TFileArray read FFiles;
    procedure Clear;
    procedure Download;
  end;

var
  MultiFileDownload: TMultiFileDownload;

implementation
uses
  fphttpclient,
  opensslsockets;

{$R *.lfm}

{ TMultiFileDownload }

procedure TMultiFileDownload.btn_clearClick(Sender: TObject);
begin
  Clear;
end;

procedure TMultiFileDownload.btn_downloadClick(Sender: TObject);
begin
  Download;
end;

procedure TMultiFileDownload.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
var
  LTotal : Int64;
  I: Integer;
begin
  //the index into our individual progress array is stored in the "tag" property
  FIndividualProgress[TFPHTTPClient(Sender).Tag] := CurrentPos;

  //sum up the all of the individual progress counts via a "dirty" read
  LTotal := 0;
  for I := 0 to High(FFiles) do
    Inc(LTotal, Length(FFiles[I]));

  //now see if our total is greater than the current progress we've stored
  //if so update this via an interlocked exchange
  if LTotal > FCurrentProgress then
    InterlockedExchange(FCurrentProgress, LTotal);

  //now throw a task on the UI queue to update the progress bar
  TThread.Queue(nil, UpdateProgressBar);
end;

function TMultiFileDownload.GetFiles: TStrings;
begin
  Result := memo_filelist.Lines;
end;

function TMultiFileDownload.GetTotalBytes(const ATotalVarName: String): IEZThread;

  procedure GetBytes(const AThread : IEZThread);
  var
    LTotal, J, I: Integer;
    LHeaders: TStrings;
    LURL : String;
  begin
    LHeaders := TStringList.Create;
    try
      LTotal := 0;

      //for each file make a HEAD call to get the content length
      for I := 0 to Pred(Integer(AThread['count'])) do
      begin
        LURL := AThread[IntToStr(I)];
        TFPHTTPClient.Head(LURL, LHeaders);
        J := TFPHTTPClient.IndexOfHeader(LHeaders, 'Content-Length');

        //if we have the content length header, add it to total
        if J >= 0 then
          Inc(LTotal, StrToIntDef(TFPHTTPClient.GetHeader(LHeaders, 'Content-Length'), 0));
      end;

      //add the total to our thread for our progress bar method to use
      AThread.AddArg(AThread['totalName'], LTotal);
    finally
      LHeaders.Free;
    end;
  end;

var
  I: Integer;
begin
  Result := NewEZThread;
  Result
    .AddArg('count', memo_filelist.Lines.Count) //used to determine file count in thread method
    .AddArg('totalName', ATotalVarName)
    .Setup(GetBytes, nil, nil); //setup the call to get sum of content lengths

  //add all the files to the thread
  for I := 0 to Pred(memo_filelist.Lines.Count) do
    Result.AddArg(IntToStr(I), memo_filelist.Lines[I]);

  Result.Start;
end;

procedure TMultiFileDownload.DisableUI;
begin
  btn_download.Enabled := False;
  btn_clear.Enabled := False;
  memo_filelist.Enabled := False;
end;

procedure TMultiFileDownload.EnableUI;
begin
  btn_download.Enabled := True;
  btn_clear.Enabled := True;
  memo_filelist.Enabled := True;
end;

procedure TMultiFileDownload.UpdateProgressBar;
begin
  progress.Position := FCurrentProgress;
end;

procedure TMultiFileDownload.Clear;
begin
  memo_filelist.Clear;
  FFiles := Default(TFileArray);
  progress.Position := 0;
end;

procedure TMultiFileDownload.Download;
var
  LPool: IEZThreadPool;
  I, LFileCount: Integer;
  LInitThread: IEZThread;
  LFinished: Boolean;

  procedure DownloadFile(const AThread : IEZThread);
  var
    LClient: TFPHTTPClient;
    I : Integer;
    LURL : String;
    LStream: TMemoryStream;
  begin
    //get the index we need to work on, use interlocked dec to ensure "clean" reads
    I := InterlockedDecrement(LFileCount);

    //no more files, this thread is done
    if I < 0 then
      Exit;

    //create an http client
    LClient := TFPHTTPClient.Create(nil);
    LStream := TMemoryStream.Create;
    try
      //setup the client and make a get for the file
      LClient.OnDataReceived := DataReceived; //callback to handle the progress bar
      LClient.Tag := I; //set the tag to the index we're working
      LURL := AThread[IntToStr(I)]; //url stored by the index as the "argument name"
      LClient.Get(LURL, LStream);

      //after the get re-position the stream and copy to our files array
      LStream.Position := 0;
      SetLength(FFiles[I], LStream.Size);
      LStream.ReadBuffer(FFIles[I], LStream.Size);
    finally
      LClient.Free;
      LStream.Free;

      //last file, mark our flag as "finished"
      if I <= 0 then
        LFinished := True;
    end;
  end;

  procedure FinishDownloads;
  begin
    //here's where we could now save our files to disk or do whatever else we want
    //with them. for demo purposes we just make a note here and if a user wants
    //to access via code there is a FileData property
    //....

    //not sure if the data received gets called up to the last byte, but
    //if we make it here, then we're definitely done, so update progress bar to max
    progress.Position := FTotalBytes;

    //re-enable the UI
    EnableUI;
  end;

begin
  FFiles := Default(TFileArray);

  //first disable our UI to avoid people smashing the download button a million times
  DisableUI;

  //get the total bytes of data for all the files. we'll use a method
  //that returns a started ezthread so we can initialize the pool in parallel to data fetching
  LInitThread := GetTotalBytes('total');

  //create a pool with a user defined amount of workers
  LPool := NewEZThreadPool(StrToIntDef(edit_workers.Text, 2));

  //we'll setup a single counter to act like a "queue"
  LFileCount := memo_filelist.Lines.Count;

  //simple bool flag to say when we're "done"
  LFinished := LFileCount < 1;

  //use an array of TBytes to hold the contents
  SetLength(FFiles, LFileCount);

  //initialize the pool with as many lines (files) the user is requesting to download
  //and the task that will use this to download from the internet
  for I := 0 to Pred(memo_filelist.Lines.Count) do
    LPool
      .Queue(DownloadFile, nil, nil)
      .AddArg(IntToStr(I), memo_filelist.Lines[I]); //name is index value is url

  //before we start the pool, ensure we've finished setting up the progress bar
  //but also allow UI to still receive messages (Await here would block UI so we use the state)
  while LInitThread.State = esStarted do
  begin
    Application.ProcessMessages;
    Sleep(50);
  end;

  //reset everything for our progress bar
  progress.Position := 0; //bar back to 0
  FCurrentProgress := 0; //our var to hold the current total on the data received callback
  FTotalBytes := LInitThread['total']; //total, matches the bar max
  SetLength(FIndividualProgress, LFileCount); //for each http client store the current data received
  progress.Max := FTotalBytes;

  //start
  LPool.Start;

  (*
    if we were to "await" the pool it would block the UI, so there's a few
    ways we could handle this. a very simple one is below, use your file count var
    to just call process messages since it is decremented by the threads, once it hits
    zero, then we're done. another way could be to setup another "watcher" ezthread
    which just calls Await(FPool) but would require to set the pool to a private var first
  *)
  while not LFinished do
  begin
    Application.ProcessMessages;
    Sleep(50);
  end;

  //stop the pool
  LPool.Stop;

  //all done here
  FinishDownloads;
end;

end.

