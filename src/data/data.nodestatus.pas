unit Data.NodeStatus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
;

type
{ TNodeStatus }
  TNodeStatus = class(TObject)
  private
    FReady: Boolean;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    property Ready: Boolean
      read FReady
      write FReady;
  published
  end;

implementation

{ TNodeStatus }

constructor TNodeStatus.Create;
begin
  FReady:= False;
end;

destructor TNodeStatus.Destroy;
begin
  inherited Destroy;
end;

end.

