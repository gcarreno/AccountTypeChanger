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
, DefaultTranslator
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    alMain: TActionList;
    actFileExit: TFileExit;
    lblBlock: TLabel;
    lblStatus: TLabel;
    lbSteps: TListBox;
    memLog: TMemo;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    Page1: TPage;
    panStepConnectTitle: TPanel;
    panStepSendPascalTitle: TPanel;
    panStepChangeOnceTitle: TPanel;
    panStepChangeTwiceTitle: TPanel;
    pcSteps: TPageControl;
    panInfo: TPanel;
    lblHelp: TStaticText;
    tsChangeTwice: TTabSheet;
    tsChangeOnce: TTabSheet;
    tsSendPascal: TTabSheet;
    tsConnect: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitShortcuts;
    procedure UpdateStatus;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
, Data.NodeStatus
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

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= Format('%s v%s', [ rsApplicationTitle, cVersion ]);
  InitShortcuts;
  UpdateStatus;
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
  lbSteps.Items.Add(rsStepConnect);

  pcSteps.ShowTabs:= False;
  pcSteps.ActivePageIndex:= 0;
  tsConnect.Caption:= rsStepConnect;
  tsSendPascal.Caption:= rsStepSendPascal;
  tsChangeOnce.Caption:= rsStepChangeOnce;
  tsChangeTwice.Caption:= rsStepChangeTwice;

  panStepConnectTitle.Caption:= rsStepConnect;
  panStepSendPascalTitle.Caption:= rsStepSendPascal;
  panStepChangeOnceTitle.Caption:= rsStepChangeOnce;
  panStepChangeOnceTitle.Caption:= rsStepChangeTwice;
end;

end.

