unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs, Menus, ActnList, StdActns
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    alMain: TActionList;
    actFileExit: TFileExit;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitShortcuts;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
;

const
  cVersion = {$I 'version.inc'};

resourcestring
  rsApplicationTitle = 'Account Type Changer';

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= Format('%s v%s', [ rsApplicationTitle, cVersion ]);
  InitShortcuts;
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

end.

