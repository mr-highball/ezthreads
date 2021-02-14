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
    FFiles : TFileArray;

    procedure DataReceived(Sender: TObject; const ContentLength,
      CurrentPos: Int64);
    function GetFiles: TStrings;
    function GetTotalBytes(const ATotalVarName : String) : IEZThread;
    procedure DisableUI;
    procedure EnableUI;
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
begin
  //todo
end;

function TMultiFileDownload.GetFiles: TStrings;
begin
  Result := memo_filelist.Lines;
end;

function TMultiFileDownload.GetTotalBytes(const ATotalVarName: String
  ): IEZThread;

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
      LClient.OnDataReceived := DataReceived;
      LURL := AThread[IntToStr(I)];
      LClient.Get(LURL, LStream);
      LStream.Position := 0;
      SetLength(FFiles[I], LStream.Size);
      LStream.ReadBuffer(FFIles[I], LStream.Size);
    finally
      LClient.Free;
      LStream.Free;
    end;
  end;

  procedure FinishDownloads;
  begin
    //here's where we could now save our files to disk or do whatever else we want
    //with them. for demo purposes we just make a note here and if a user wants
    //to access via code there is a FileData property
    //....

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

  progress.Position := 0;
  progress.Max := LInitThread['total'];

  //start
  LPool.Start;

  (*
    if we were to "await" the pool it would block the UI, so there's a few
    ways we could handle this. a very simple one is below, use your file count var
    to just call process messages since it is decremented by the threads, once it hits
    zero, then we're done. another way could be to setup another "watcher" ezthread
    which just calls Await(FPool) but would require to set the pool to a private var first
  *)
  while LFileCount > 0 do
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

