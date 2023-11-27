unit Data.NodeStatus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
, jsonparser
;

type
{ TNodeStatus }
  TNodeStatus = class(TObject)
  private
    FReady: Boolean;
    FReadyString: TJSONStringType;
    FStatusString: TJSONStringType;
    FPort: Integer;
    FLocked: Boolean;
    FTimeStamp: Int64;
    FVersion: TJSONStringType;
    FBlocks: Int64;

    FCompressedJSON: Boolean;

    procedure setFromJSON(const AJSON: TJSONStringType);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
    procedure setFromStream(const AStream: TStream);

    function getAsJSON: TJSONStringType;
    function getAsJSONData: TJSONData;
    function getAsJSONObject: TJSONObject;
    function getAsStream: TStream;
  protected
  public
    constructor Create;
    constructor Create(const AJSON: TJSONStringType);
    constructor Create(const AJSONData: TJSONData);
    constructor Create(const AJSONObject: TJSONObject);
    constructor Create(const AStream: TStream);

    destructor Destroy; override;

    function FormatJSON(AOptions : TFormatOptions = DefaultFormat;
      AIndentsize : Integer = DefaultIndentSize): TJSONStringType;

    property Ready: Boolean
      read FReady
      write FReady;
    property ReadyString: TJSONStringType
      read FReadyString
      write FReadyString;
    property StatusString: TJSONStringType
      read FStatusString
      write FStatusString;
    property Port: Integer
      read FPort
      write FPort;
    property Locked: Boolean
      read FLocked
      write FLocked;
    property TimeStamp: Int64
      read FTimeStamp
      write FTimeStamp;
    property Version: TJSONStringType
      read FVersion
      write FVersion;
    property Blocks: Int64
      read FBlocks
      write FBlocks;

    property CompressedJSON: Boolean
      read FCompressedJSON
      write FCompressedJSON;

    property AsJSON: TJSONStringType
      read getAsJSON;
    property AsJSONData: TJSONData
      read getAsJSONData;
    property AsJSONObject: TJSONObject
      read getAsJSONObject;
    property AsStream: TStream
      read getAsStream;

  published
  end;

implementation

type
{ ENodeStatusNotAJSONObject }
  ENodeStatusNotAJSONObject = Exception;
{ ENodeStatusEmptyString }
  ENodeStatusEmptyString = Exception;
{ ENodeStatusCannotParse }
  ENodeStatusCannotParse = Exception;
{ ENodeStatusMissingMember }
  ENodeStatusMissingMember = Exception;
{ ENodeStatusParamsWrongType }
  ENodeStatusParamsWrongType = Exception;

const
  cJSONReady = 'ready';
  cJSONReadyString = 'ready_s';
  cJSONStatusString = 'status_s';
  cJSONPort = 'port';
  cJSONLocked = 'locked';
  cJSONTimeStamp = 'timestamp';
  cJSONVersion = 'version';
  cJSONBlocks = 'blocks';

resourcestring
  rsExceptionNotAJSONObject = 'JSON Data is not an object';
  rsExceptionEmptyString = 'MUST not be and empty string';
  rsExceptionCannotParse = 'Cannot parse: %s';
  rsExceptionMissingMember = 'Missing member: %s';

{ TNodeStatus }

procedure TNodeStatus.setFromJSON(const AJSON: TJSONStringType);
var
  jData: TJSONData;
begin
  if trim(AJSON) = EmptyStr then
  begin
    raise ENodeStatusEmptyString.Create(rsExceptionEmptyString);
  end;
  try
    jData:= GetJSON(AJSON);
  except
    on E: Exception do
    begin
      raise ENodeStatusCannotParse.Create(Format(rsExceptionCannotParse, [E.Message]));
    end;
  end;
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;

procedure TNodeStatus.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtObject then
  begin
    raise ENodeStatusNotAJSONObject.Create(rsExceptionNotAJSONObject);
  end;
  setFromJSONObject(aJSONData as TJSONObject);
end;

procedure TNodeStatus.setFromJSONObject(const AJSONObject: TJSONObject);
begin
  { #todo -ogcarreno : Need to read the complete object }
  FReady:= AJSONObject.Get(cJSONReady, FReady);
  FReadyString:= AJSONObject.Get(cJSONReadyString, FReadyString);
  FStatusString:= AJSONObject.Get(cJSONStatusString, FStatusString);
  FPort:= AJSONObject.Get(cJSONPort, FPort);
  FLocked:= AJSONObject.Get(cJSONLocked, FLocked);
  FTimeStamp:= AJSONObject.Get(cJSONTimeStamp, FTimeStamp);
  FVersion:= AJSONObject.Get(cJSONVersion, FVersion);
  FBlocks:= AJSONObject.Get(cJSONBlocks, FBlocks);
end;

procedure TNodeStatus.setFromStream(const AStream: TStream);
var
  jData: TJSONData;
begin
  if AStream.Size = 0 then
  begin
    raise ENodeStatusEmptyString.Create(rsExceptionEmptyString);
  end;
  try
    jData:= GetJSON(AStream);
  except
    on E: Exception do
    begin
      raise ENodeStatusCannotParse.Create(Format(rsExceptionCannotParse, [E.Message]));
    end;
  end;
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;

function TNodeStatus.getAsJSON: TJSONStringType;
var
  jObject: TJSONObject;
begin
  Result:= '';
  jObject:= getAsJSONObject;
  jObject.CompressedJSON:= FCompressedJSON;
  Result:= jObject.AsJSON;
  jObject.Free;
end;

function TNodeStatus.getAsJSONData: TJSONData;
begin
  Result:= getAsJSONObject as TJSONData;
end;

function TNodeStatus.getAsJSONObject: TJSONObject;
begin
  { #todo -ogcarreno : Need to return the complete object }
  Result:= TJSONObject.Create;
  Result.Add(cJSONReady, FReady);
  Result.Add(cJSONReadyString, FReadyString);
  Result.Add(cJSONStatusString, FStatusString);
  Result.Add(cJSONPort, FPort);
  Result.Add(cJSONLocked, FLocked);
  Result.Add(cJSONTimeStamp, FTimeStamp);
  Result.Add(cJSONVersion, FVersion);
  Result.Add(cJSONBlocks, FBlocks);
end;

function TNodeStatus.getAsStream: TStream;
begin
  Result:= TStringStream.Create(getAsJSON, TEncoding.UTF8);
end;

constructor TNodeStatus.Create;
begin
  FReady:= False;
end;

constructor TNodeStatus.Create(const AJSON: TJSONStringType);
begin
  Create;
  setFromJSON(AJSON);
end;

constructor TNodeStatus.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

constructor TNodeStatus.Create(const AJSONObject: TJSONObject);
begin
  Create;
  setFromJSONObject(AJSONObject);
end;

constructor TNodeStatus.Create(const AStream: TStream);
begin
  Create;
  setFromStream(AStream);
end;

destructor TNodeStatus.Destroy;
begin
  inherited Destroy;
end;

function TNodeStatus.FormatJSON(AOptions: TFormatOptions;
  AIndentsize: Integer): TJSONStringType;
begin
  Result:= getAsJSONObject.FormatJSON(AOptions, AIndentsize);
end;

end.

