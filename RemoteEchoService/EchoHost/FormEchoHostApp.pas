unit FormEchoHostApp;

interface

uses
  System.Android.Service,
  Androidapi.JNI.Os,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;


// System.Classes, System.Messaging,
// {$IFDEF ISD102T_DELPHI}
// System.Permissions,
// {$ENDIF}
// System.Sensors,
// System.Generics.Collections, System.Types, System.UITypes,
// FMX.Controls, FMX.Controls.Presentation, FMX.Types, FMX.Forms, FMX.Dialogs,
// Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
// FMX.Platform, FMX.StdCtrls, // IsNavUtils,
// ServiceUnit, FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TFormEchoApp = class(TForm)
    BtnBindRemote: TButton;
    BtnSendRemote: TButton;
    BtnTestService: TButton;
    Memo1: TMemo;
    procedure BtnSendRemoteClick(Sender: TObject);
    procedure BtnTestServiceClick(Sender: TObject);
    procedure BtnBindRemoteClick(Sender: TObject);
  private
    { Private declarations }
    FRemoteConnected: Boolean;
    FToastText: String;
    FRemoteServiceConnection: TRemoteServiceConnection;
    Procedure ErrorLog(AError:String);
    Procedure MakeServiceConnections;
    procedure OnHandleRemoteMessage(const AMessage: JMessage);
    procedure OnRemoteServiceConnected(const ServiceMessenger: JMessenger);
  public
    { Public declarations }
  end;

var
  FormEchoApp: TFormEchoApp;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget,
  FMX.Platform.Android, // IsFmxGraphics, GpsUserDataAccess,
  FMX.DialogService, SvcEchoDM;

procedure TFormEchoApp.BtnBindRemoteClick(Sender: TObject);
begin
  if FRemoteServiceConnection = nil then
    MakeServiceConnections;
  Try
//    FRemoteServiceConnection.BindService('com.embarcadero.AppRemoteHost',
//      'com.embarcadero.services.RemoteService');
    FRemoteServiceConnection.BindService('com.embarcadero.RemoteEchoHostApp',
      'com.embarcadero.services.RemoteService');
    FToastText := '';
  Except
    On e: Exception do
      FToastText := e.Message;
  End;
  if FToastText <> '' then
    Memo1.lines.Add(FToastText);
end;

procedure TFormEchoApp.BtnSendRemoteClick(Sender: TObject);
var
  LMessage: JMessage;
const
  GET_STRING = 123;
begin
  try
    if FRemoteServiceConnection = nil then
      MakeServiceConnections;
    if not FRemoteConnected then
      Exit;
    LMessage := TJMessage.JavaClass.obtain(nil, GET_STRING);
    LMessage.replyTo := FRemoteServiceConnection.LocalMessenger;
    FRemoteServiceConnection.ServiceMessenger.send(LMessage);
    FToastText := '';
  Except
    On e: Exception do
      FToastText := e.Message;
  End;
  if FToastText <> '' then
    Memo1.lines.Add(FToastText);
end;

procedure TFormEchoApp.BtnTestServiceClick(Sender: TObject);
begin
  TToastViaEchoObj.ToastMsgViaEcho('Test Toast via Echo',ErrorLog);
end;

procedure TFormEchoApp.ErrorLog(AError: String);
begin
   Memo1.lines.Add('ERROR::'+AError);
end;

procedure TFormEchoApp.MakeServiceConnections;
begin
  Try
    FToastText := 'Do Remote';
    if FRemoteServiceConnection = nil then
    begin
      FRemoteServiceConnection := TRemoteServiceConnection.Create;
      FRemoteServiceConnection.OnConnected := OnRemoteServiceConnected;
      FRemoteServiceConnection.OnHandleMessage := OnHandleRemoteMessage;
    end;
    FToastText := '';
  Except
    On e: Exception do
      FToastText := FToastText + e.Message;
  End;
  if FToastText <> '' then
    ErrorLog(FToastText);
end;

procedure TFormEchoApp.OnHandleRemoteMessage(const AMessage: JMessage);
const
  SERVICE_STRING = 321;
var
  LStr: JString;
  LBundle: JBundle;
begin
  case AMessage.what of
    SERVICE_STRING:
      begin
        LBundle := TJBundle.Wrap(AMessage.obj);
        LStr := LBundle.getString(TAndroidHelper.StringToJString('Key'));
        TJToast.JavaClass.makeText(TAndroidHelper.Context,
          LStr.subSequence(0, LStr.length),
          TJToast.JavaClass.LENGTH_SHORT).show;
      end;
  else
    FRemoteServiceConnection.Handler.Super.handleMessage(AMessage);
  end;
end;


procedure TFormEchoApp.OnRemoteServiceConnected(
  const ServiceMessenger: JMessenger);
begin
  FRemoteConnected:=True;
  Memo1.Lines.Add('Remote Service Connected')
end;

end.
