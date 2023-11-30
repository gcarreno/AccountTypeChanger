unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, Menus
, ActnList
, StdActns
, ExtCtrls
, StdCtrls
, ComCtrls
, Spin
, fphttpclient
, fpjson
, DefaultTranslator
, LJD.Request
, LJD.Response
, LJD.Error
, Data.NodeStatus
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actStepsChangeTwice: TAction;
    actStepsChangeOnce: TAction;
    actStepsSendPascal: TAction;
    actStepsConnect: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    btnStepsConnect: TButton;
    edtWalletPassword: TEdit;
    edtWalletIP: TEdit;
    Label1: TLabel;
    lblWalletPort: TLabel;
    lblWalletIP: TLabel;
    lblBlock: TLabel;
    lblStatus: TLabel;
    lbSteps: TListBox;
    memLog: TMemo;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    Page1: TPage;
    panStepConnectButtons: TPanel;
    panStepConnectTitle: TPanel;
    panStepSendPascalTitle: TPanel;
    panStepChangeOnceTitle: TPanel;
    panStepChangeTwiceTitle: TPanel;
    pcSteps: TPageControl;
    panInfo: TPanel;
    lblHelp: TStaticText;
    edtWalletPort: TSpinEdit;
    splLog: TSplitter;
    tsChangeTwice: TTabSheet;
    tsChangeOnce: TTabSheet;
    tsSendPascal: TTabSheet;
    tsConnect: TTabSheet;
    procedure actStepsConnectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHTTPClient: TFPHTTPClient;

    procedure InitShortcuts;
    procedure UpdateStatus;

    function DoRPCRequest(AJSONRPCRequest: TJSONRPCRequest): TJSONRPCResponse;

    function FetchNodeStatus: TNodeStatus;
    function DoWalletUnlock: Boolean;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
;

type
  TStatus = (
    stDisconnected
  , stConnected
  , stUnlocked
  , stPascalSent
  , stChangedOnce
  , stChangedTwice
  );

const
  cVersion = {$I 'version.inc'};
  cURL = 'http://%s:%d';
  cMethodNodeStatus = 'nodestatus';
  cMethodUnlock = 'unlock';

  cParamPassword = 'pwd';

resourcestring
  rsApplicationTitle = 'Account Type Changer';

  rsError = 'ERROR: %s';
  rsErrorEmptyIP = 'You need to fill the Address/IP for the wallet';
  rsErrorEmptyPort = 'You need to fill the Port for the wallet';
  rsErrorEmptyPassword = 'You need to fill the Password for the wallet';

  rsRPCError = 'RPC Error[%d]; %s';

  rsApplicationStatusCaption = 'Status: %s';
  rsApplicationStatusDisconnected = 'Disconnected';
  rsApplicationStatusConnected = 'Connected';
  rsApplicationStatusUnlocked = 'Unlocked';
  rsApplicationStatusPascalSent = 'Pascal Sent';
  rsApplicationStatusChangedOnce = 'Changed Once';
  rsApplicationStatusChangedTwice = 'Changed Twice';

  rsBlockNumber = 'Block: %d';
  rsBlockUnknown = 'Block: ??';

  rsStepConnect = 'Connect';
  rsStepSendPascal = 'Send Pascal';
  rsStepChangeOnce = 'Change Once';
  rsStepChangeTwice = 'Change Twice';

var
  CurrentStatus: TStatus = stDisconnected;
  LastBlock: Int64 = -1;
  CurrentBlock: Int64 = -1;
  RPCID: Int64 = 1;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= Format('%s v%s', [ rsApplicationTitle, cVersion ]);
  InitShortcuts;
  UpdateStatus;

  lbSteps.Items.Add(rsStepConnect);
  lbSteps.ItemIndex:= 0;

  pcSteps.ShowTabs:= False;
  pcSteps.ActivePageIndex:= 0;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Just in case
end;

procedure TfrmMain.InitShortcuts;
begin
{$IFDEF UNIX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.UpdateStatus;
var
  statusCaption: String = '';
begin
  case CurrentStatus of
    stDisconnected:begin
      statusCaption:= rsApplicationStatusDisconnected;
    end;
    stConnected:begin
      statusCaption:= rsApplicationStatusConnected;
    end;
    stUnlocked:begin
      statusCaption:= rsApplicationStatusUnlocked;
    end;
    stPascalSent:begin
      statusCaption:= rsApplicationStatusPascalSent;
    end;
    stChangedOnce:begin
      statusCaption:= rsApplicationStatusChangedOnce;
    end;
    stChangedTwice:begin
      statusCaption:= rsApplicationStatusChangedTwice;
    end;
  end;
  lblStatus.Caption:= Format(rsApplicationStatusCaption, [statusCaption]);
  if LastBlock < 0 then
  begin
    lblBlock.Caption:= rsBlockUnknown;
  end
  else
  begin
    lblBlock.Caption:= Format(rsBlockNumber, [ LastBlock ]);
  end;
end;

function TfrmMain.DoRPCRequest(
  AJSONRPCRequest: TJSONRPCRequest): TJSONRPCResponse;
var
  jsonRequestBody: TStringStream;
  jsonResponse: String;
begin
  Result:= nil;
  FHTTPClient:= TFPHTTPClient.Create(nil);
  try
    AJSONRPCRequest.CompressedJSON:= True;
    jsonRequestBody:= TStringStream.Create(AJSONRPCRequest.AsJSON);
    FHTTPClient.AllowRedirect:= True;
    FHTTPClient.RequestBody:= jsonRequestBody;
    jsonResponse:= FHTTPClient.Post(Format(
      cURL,
      [ edtWalletIP.Text, edtWalletPort.Value ]
    ));
    Result:= TJSONRPCResponse.Create(jsonResponse);
    if Result.HasError then
    begin
      raise Exception.Create(Format(rsRPCError, [
        Result.Error.Code,
        Result.Error.Message
      ]));
    end;
  finally
    FHTTPClient.Free;
  end;
end;

function TfrmMain.FetchNodeStatus: TNodeStatus;
var
  jsonRPCRequest: TJSONRPCRequest;
  jsonRPCResponse: TJSONRPCResponse;
begin
  Result:= nil;
  jsonRPCRequest:= TJSONRPCRequest.Create;
  try
    jsonRPCRequest.Method:= cMethodNodeStatus;
    jsonRPCRequest.ID:= RPCID;
    Inc(RPCID);
    jsonRPCResponse:= DoRPCRequest(jsonRPCRequest);
    Result:= TNodeStatus.Create(jsonRPCResponse.Result);
    jsonRPCResponse.Free;
  finally
    jsonRPCRequest.Free;
  end;
end;

function TfrmMain.DoWalletUnlock: Boolean;
var
  jsonRPCRequest: TJSONRPCRequest;
  jsonRPCResponse: TJSONRPCResponse;

begin
  Result:= False;
  jsonRPCRequest:= TJSONRPCRequest.Create;
  try
    jsonRPCRequest.Method:= cMethodUnlock;
    jsonRPCRequest.Params:= TJSONObject.Create;
    jsonRPCRequest.ID:= RPCID;
    Inc(RPCID);
    TJSONObject(jsonRPCRequest.Params).Add(cParamPassword, edtWalletPassword.Text);
    jsonRPCResponse:= DoRPCRequest(jsonRPCRequest);

    Result:= jsonRPCResponse.Result.AsBoolean;

    jsonRPCResponse.Free;
  finally
    jsonRPCRequest.Free;
  end;
end;

procedure TfrmMain.actStepsConnectExecute(Sender: TObject);
var
  success: Boolean = False;
  jsonNodeStatus: TNodeStatus;
begin
  actStepsConnect.Enabled:= False;
  Application.ProcessMessages;

  if Length(edtWalletIP.Text) = 0 then
  begin
    Application.ProcessMessages;
    ShowMessage(rsErrorEmptyIP);
    actStepsConnect.Enabled:= True;
    exit;
  end;

  if (edtWalletPort.Value < 1) or (edtWalletPort.Value > 65535) then
  begin
    Application.ProcessMessages;
    ShowMessage(rsErrorEmptyPort);
    actStepsConnect.Enabled:= True;
    exit;
  end;

  if Length(edtWalletPassword.Text) = 0 then
  begin
    Application.ProcessMessages;
    ShowMessage(rsErrorEmptyPassword);
    actStepsConnect.Enabled:= True;
    exit;
  end;

  // Node Status
  try
    jsonNodeStatus:= FetchNodeStatus;
    CurrentStatus:= stConnected;
    LastBlock:= jsonNodeStatus.Blocks;
    CurrentBlock:= LastBlock;
    UpdateStatus;
  except
    on E:Exception do
    begin
      memLog.Append(Format(rsError, [ E.Message ]));
    end;
  end;

  // Unlock
  if jsonNodeStatus.Locked then
  begin
    memLog.Append('Wallet locked, attempting unlock');
    try
      if DoWalletUnlock then
      begin
        memLog.Append('Wallet unlocked');
        success:= True;
      end
      else
      begin
        memLog.Append('Could not unlock wallet');
      end;
    except
      on E:Exception do
      begin
        memLog.Append(Format(rsError, [ E.Message ]));
      end;
    end;
  end
  else
  begin
    memLog.Append('Wallet already unlocked');
  end;

  Application.ProcessMessages;
  if not success then
    actStepsConnect.Enabled:= True;
end;

end.

