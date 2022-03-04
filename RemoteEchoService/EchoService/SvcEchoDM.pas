unit SvcEchoDM;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os;


type
  TSvcDMExho = class(TAndroidService)
    function AndroidServiceHandleMessage(const Sender: TObject;
      const AMessage: JMessage): Boolean;
    function AndroidServiceStartCommand(const Sender: TObject;
      const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceCreate(Sender: TObject);
    function AndroidServiceBind(const Sender: TObject;
      const AnIntent: JIntent): JIBinder;
  private
    { Private declarations }
    FDebugString:String;
  public
    { Public declarations }
  end;

  TErrorProc = procedure (AError:String) of Object;

  TToastViaEchoObj = class(TObject)
    Private
    FConnected:boolean;
    FErrorText:String;
    FEchoServiceConnection: TRemoteServiceConnection;
    FErrorProc:TErrorProc;
    procedure OnEchoServiceConnected(const ServiceMessenger: JMessenger);
    procedure OnEchoServiceDisconnected;
    procedure OnHandleRemoteMessage(const AMessage: JMessage);
    procedure BindToService;
    Procedure SendTextToEcho(AText:String);
    Public
    Constructor Create(AErrorProc:TErrorProc);
    Destructor Destroy; override;

    Class Function ToastMsgViaEcho(AMessage:String; AErrorProc:TErrorProc=nil):TToastViaEchoObj;
  end;

var
  SvcDMExho: TSvcDMExho;
  SVCEchoPackageName:String='com.embarcadero.RemoteEchoHostApp';

const
  SET_ECHO_STRING = 564;
  SVC_ECHO_STRING = 465;
  SVCEchoSvcName= 'com.embarcadero.services.RemoteEchoService';



implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget;

function TSvcDMExho.AndroidServiceBind(const Sender: TObject;
  const AnIntent: JIntent): JIBinder;
begin
  FDebugString:='AndroidServiceBind';
end;

procedure TSvcDMExho.AndroidServiceCreate(Sender: TObject);
begin
  FDebugString:='AndroidServiceCreate';
end;

function TSvcDMExho.AndroidServiceHandleMessage(const Sender: TObject;
  const AMessage: JMessage): Boolean;
var
  LMessage: JMessage;
  LBundle: JBundle;
  LStr: JString;
  LRxBundle: JBundle;

  LString:String;
begin
 Try
  FDebugString:='AndroidServiceHandleMessage';
  case AMessage.what of
    SET_ECHO_STRING:
    begin
      LRxBundle := TJBundle.Wrap(AMessage.obj);
      LStr := LRxBundle.getString(TAndroidHelper.StringToJString('Key'));
      LString:=TAndroidHelper.JStringToString(Lstr);
      LBundle := TJBundle.Create;  // we can not send String because is not parcelable
      LMessage := TJMessage.Create;
      LMessage.what := SVC_ECHO_STRING;
      LBundle.putString(TAndroidHelper.StringToJString('Key'), LStr);
      LMessage.obj := LBundle;
      AMessage.replyTo.send(LMessage);
      Result := True;
    end;
  else
    Result := False;
  end;
 Except
   On E:Exception do
     LString:=E.Message;
 End;
end;

function TSvcDMExho.AndroidServiceStartCommand(const Sender: TObject;
  const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  FDebugString:='AndroidServiceStartCommand';
end;

{ TToastViaEchoObj }




Var
 SingleTonEchoObj:TToastViaEchoObj=nil;

procedure TToastViaEchoObj.BindToService;
begin
  if FEchoServiceConnection<>nil then
   if not FConnected then
      FEchoServiceConnection.BindService(SVCEchoPackageName,SVCEchoSvcName);
end;

constructor TToastViaEchoObj.Create(AErrorProc:TErrorProc);
begin
  FErrorProc:=AErrorProc;
  if SingleTonEchoObj<>nil then
      raise Exception.Create('Second Example Of TToastViaEchoObj');

  SingleTonEchoObj:=self;

  FEchoServiceConnection:=TRemoteServiceConnection.Create;
  FEchoServiceConnection.OnConnected := OnEchoServiceConnected;
  FEchoServiceConnection.OndIsConnected:=OnEchoServiceDisconnected;
  FEchoServiceConnection.OnHandleMessage := OnHandleRemoteMessage;
  BindToService;
end;

procedure TToastViaEchoObj.OnHandleRemoteMessage(const AMessage: JMessage);
var
  LStr: JString;
  LBundle: JBundle;
begin
  case AMessage.what of
    SVC_ECHO_STRING:
      begin
        LBundle := TJBundle.Wrap(AMessage.obj);
        LStr := LBundle.getString(TAndroidHelper.StringToJString('Key'));
        TJToast.JavaClass.makeText(TAndroidHelper.Context,
          LStr.subSequence(0, LStr.length),
          TJToast.JavaClass.LENGTH_SHORT).show;
      end;
  else
    FEchoServiceConnection.Handler.Super.handleMessage(AMessage);
  end;
end;

procedure TToastViaEchoObj.SendTextToEcho(AText: String);
var
  LBundle: JBundle;
  LStr: JString;
  LMessage: JMessage;

begin
  try
    if not FConnected then
      Exit;


    LStr :=  TAndroidHelper.StringToJString(AText);

    LBundle := TJBundle.Create;
    // we can not send String because is not parcelable
    LBundle.putString(TAndroidHelper.StringToJString('Key'), LStr);

//    LMessage := TJMessage.Create;
//    LMessage.what := SET_ECHO_STRING;
    LMessage := TJMessage.JavaClass.obtain(nil, SET_ECHO_STRING);
    LMessage.replyTo := FEchoServiceConnection.LocalMessenger;

    LMessage.obj := LBundle;
    FEchoServiceConnection.ServiceMessenger.send(LMessage);
    FErrorText := '';
  Except
    On e: Exception do
      FErrorText := e.Message;
  End;
  if FErrorText <> '' then
    if Assigned(FErrorProc) then
      FErrorProc(FErrorText);
end;

destructor TToastViaEchoObj.Destroy;
begin
  if SingleTonEchoObj=self then
     SingleTonEchoObj:=nil;
  FreeAndNil(FEchoServiceConnection);
  inherited;
end;

procedure TToastViaEchoObj.OnEchoServiceConnected(
  const ServiceMessenger: JMessenger);
begin
  FConnected:=True;
end;

procedure TToastViaEchoObj.OnEchoServiceDisconnected;
begin
  FConnected:=false;
end;

class function TToastViaEchoObj.ToastMsgViaEcho(
  AMessage: String; AErrorProc:TErrorProc=nil): TToastViaEchoObj;
Var
   TstCounter:integer;
   ErrorTxt:String;
begin
  Try
  if SingleTonEchoObj=nil then
    Begin
      TToastViaEchoObj.Create(AErrorProc);
      TstCounter:=15;
    End
    Else
      TstCounter:=6;

  if SingleTonEchoObj=nil then
     raise Exception.Create('Unable To Start SingleTonEchoObj'+#13#10+' Is Bundle '+
     SVCEchoPackageName +#13#10+ ' Service '+SVCEchoSvcName+#13#10+ 'Installed');


  While ((TstCounter>0) and Not SingleTonEchoObj.FConnected )do
     Begin
       Dec(TstCounter);
       if TstCounter=5 then
          SingleTonEchoObj.BindToService;
       Sleep(1000);
     End;

  if TstCounter<1 then
      raise Exception.Create('Unable To Send Toast');

//  if Assigned(AErrorProc) then
//          AErrorProc('TstCounter:'+IntTOStr(TstCounter));

  SingleTonEchoObj.SendTextToEcho(AMessage);
  Result:=SingleTonEchoObj;
  Except
    On E:Exception do
     Begin
       ErrorTxt:=e.Message;
       if Assigned(AErrorProc) then
          AErrorProc(ErrorTxt);
       Result:=nil;
     End;
  End;
end;

end.
