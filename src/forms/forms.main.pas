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
, DefaultTranslator
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
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
, LJD.Request
, LJD.Response
, LJD.Error
, Data.NodeStatus
, fpjson
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

resourcestring
  rsApplicationTitle = 'Account Type Changer';

  rsErrorEmptyIP = 'You need to fill the Address/IP for the wallet';
  rsErrorEmptyPort = 'You need to fill the Port for the wallet';
  rsErrorEmptyPassword = 'You need to fill the Password for the wallet';

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

procedure TfrmMain.actStepsConnectExecute(Sender: TObject);
var
  success: Boolean = False;
  jsonRPCRequest: TRequest;
  jsonRequestBody: TStringStream;
  jsonResponse: String;
  jsonRPCResponse: TResponse;
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

  FHTTPClient:= TFPHTTPClient.Create(nil);
  try
    jsonRPCRequest:= TRequest.Create;
    try
      jsonRPCRequest.CompressedJSON:= True;
      jsonRPCRequest.Method:= 'nodestatus';
      jsonRPCRequest.ID:= RPCID;
      Inc(RPCID);

      jsonRequestBody:= TStringStream.Create(jsonRPCRequest.AsJSON);
      try
        FHTTPClient.AllowRedirect:= True;
        FHTTPClient.RequestBody:= jsonRequestBody;
        try
          jsonResponse:= FHTTPClient.Post(Format(
            'http://%s:%d',
            [ edtWalletIP.Text, edtWalletPort.Value ]
          ));
          if Pos('error', jsonResponse) > 0 then
          begin
            // Deal with an RPC error
          end;
          jsonRPCResponse:= TResponse.Create(jsonResponse);
          try
            jsonRPCResponse.CompressedJSON:= False;
            //memLog.Append(jsonRPCResponse.FormatJSON);
            jsonNodeStatus:= TNodeStatus.Create(jsonRPCResponse.Result);
            try
              CurrentStatus:= stConnected;
              LastBlock:= jsonNodeStatus.Blocks;
              UpdateStatus;
              jsonNodeStatus.CompressedJSON:= False;
              memLog.Append(jsonNodeStatus.FormatJSON);
            finally
              jsonNodeStatus.Free;
            end;
          finally
            jsonRPCResponse.Free;
          end;
          success:= True;
        except
          // Deal with errors
        end;
      finally
        jsonRequestBody.Free;
      end;
    finally
      jsonRPCRequest.Free;
    end;
  finally
    FHTTPClient.Free;
  end;

  Application.ProcessMessages;
  if not success then
    actStepsConnect.Enabled:= True;
end;

end.

