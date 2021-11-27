unit IsPermissions;
// Confirms a permission is supported and interacts with user where required
// Set Uses permissions in Project Options this section manages requesting user approval for those that require it

// Delphi 10.3 Rio Targets API Level Android 26
// Delphi 10.4 Sydney Targets API Level Android 29 (Android 10)
// Data Storage changes and permissions required change between these versions https://developer.android.com/training/data-storage

interface

{$I InnovaMultiPlatLibDefs.inc}
{$IFDEF ANDROID}
{$IFDEF ISD103R_DELPHI}
{$DEFINE UseAndroidPermissions}
{$ENDIF}
{$ENDIF}

Uses
  System.Sysutils, System.classes,
  System.generics.collections, System.DateUtils, System.Types,
{$IFDEF UseAndroidPermissions}
  System.Permissions,
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  Androidapi.Helpers, Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService,
  // System.Math,
  System.Math.Vectors;

Type
  // If you add here you need to add support to PermissionsGranted
  Pmsion = (Gps, Camera, DataAcc, Network, WiFi, BlueTooth);
  PmsmSet = Set of Pmsion;
  TArrayOfPmsion = Array Of Pmsion;

  TPermissionReturnProcRef = reference to Procedure(Const AllGood: Boolean;
    Const AAllGranted, APartialGranted: PmsmSet; Const FailedMsg: String);

  // procedure RequestPermissions(const APermissions: TArray<string>;
  // const AOnRequestPermissionsResult: TRequestPermissionsResultProc; AOnDisplayRationale: TDisplayRationaleProc = nil);
  // overload; virtual;
  // TRequestPermissionsResultProc = reference to procedure(const APermissions: TArray<string>;
  // const AGrantResults: TArray<TPermissionStatus>);
  // TDisplayRationaleProc = reference to procedure(const APermissions: TArray<string>; const APostRationaleProc: TProc);

Procedure PermissionsGranted(AReq: PmsmSet; ADlgPasses, ADlgFails: Boolean;
  ReturnProc: TPermissionReturnProcRef { =nil } );

{$IFDEF UseAndroidPermissions}
Procedure ISRequestPermission(APermArray: TArray<string>;
  ADlgPasses, ADlgFails: Boolean; ReturnProc: TPermissionReturnProcRef;
  AReqArray:TArrayOfPmsion);
{$Endif}

implementation

{$IFDEF UseAndroidPermissions}

Procedure ISRequestPermission(APermArray: TArray<string>;
  ADlgPasses, ADlgFails: Boolean; ReturnProc: TPermissionReturnProcRef;
  AReqArray: TArrayOfPmsion);
Var
  ReqArray: TArrayOfPmsion;
Begin
  ReqArray:=AReqArray;
  { Sydney To Alexandria Change Procedure Definition }
{$IFDEF ISD11A_DELPHI}
  PermissionsService.RequestPermissions(APermArray,
  procedure(const APermissions: TClassicStringDynArray;
    const AGrantResults: TClassicPermissionStatusDynArray)
{$ELSE}
  PermissionsService.RequestPermissions(APermArray,
    procedure(const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>)
{$ENDIF}
    var
      ii,i: integer;
      PermGranted, PerPartial, PerReq: PmsmSet;
      GrantedTxt, GrantFailedTxt: String;
      GoodResult: Boolean;
      DoResultGrant: Boolean;
    begin
      GrantedTxt := '';
      GrantFailedTxt := '';
      PerPartial := [];
      PermGranted:=[];
      DoResultGrant := Length(APermissions) = Length(AReqArray);
      if DoResultGrant then
         for i:=0 to High(AReqArray) do
           PermGranted := PermGranted + [ReqArray[i]];
      PerReq:=PermGranted;
      if (Length(AGrantResults) > 0) then
      Begin
        for ii := 0 to High(AGrantResults) do
        begin
          if (AGrantResults[ii] = TPermissionStatus.Granted) then
          Begin
            GrantedTxt := GrantedTxt + StringReplace(APermArray[ii],
              'android.permission.', #13#10, []);
            if DoResultGrant then
              PerPartial := PerPartial + [AReqArray[ii]];
          End
          else
          begin
            GrantFailedTxt := GrantFailedTxt + StringReplace(APermArray[ii],
              'android.permission.', #13#10, []);
            if DoResultGrant then
              PermGranted := PermGranted - [AReqArray[ii]];
          end;
        end;
        if DoResultGrant then
            GoodResult := PerReq = PerPartial
           Else
            GoodResult := Length(GrantFailedTxt)<2;
        if Assigned(ReturnProc) then
          ReturnProc(GoodResult, PermGranted, PerPartial, GrantFailedTxt);

        if (GrantFailedTxt <> '') and ADlgFails then
          TDialogService.ShowMessage('Permissions not granted:' +
            GrantFailedTxt);

        if ADlgPasses then
          TDialogService.ShowMessage('Granted:' + GrantedTxt);
      End
      else
      begin
        if Assigned(ReturnProc) then
          ReturnProc(false, [], [], 'Permissions not granted');
        if ADlgFails then
          TDialogService.ShowMessage('Permissions not granted');
      end;
    end);
end;

Procedure PermissionsGranted(AReq: PmsmSet; ADlgPasses, ADlgFails: Boolean;
ReturnProc: TPermissionReturnProcRef { =nil } );
// http://docwiki.embarcadero.com/RADStudio/Sydney/en/Android_Permission_Model

// http://docwiki.embarcadero.com/RADStudio/Sydney/en/Uses_Permissions

{ Permission code

  PermissionsService  (from the System.Permissions.pas unit) to

  Thanks to ADUG and Geoffrey Smith

}

(* https://developer.android.com/reference/android/Manifest.permission

  unit Androidapi.JNI.Os;    in version \source\rtl\android\
  ACCESS_CHECKIN_PROPERTIES: JString read _GetACCESS_CHECKIN_PROPERTIES;
  ACCESS_COARSE_LOCATION: JString read _GetACCESS_COARSE_LOCATION;
  ACCESS_FINE_LOCATION: JString read _GetACCESS_FINE_LOCATION;
  ACCESS_LOCATION_EXTRA_COMMANDS: JString read _GetACCESS_LOCATION_EXTRA_COMMANDS;
  ACCESS_NETWORK_STATE: JString read _GetACCESS_NETWORK_STATE;
  ACCESS_NOTIFICATION_POLICY: JString read _GetACCESS_NOTIFICATION_POLICY;
  ACCESS_WIFI_STATE: JString read _GetACCESS_WIFI_STATE;
  ACCOUNT_MANAGER: JString read _GetACCOUNT_MANAGER;
  ADD_VOICEMAIL: JString read _GetADD_VOICEMAIL;
  ANSWER_PHONE_CALLS: JString read _GetANSWER_PHONE_CALLS;
  BATTERY_STATS: JString read _GetBATTERY_STATS;
  BIND_ACCESSIBILITY_SERVICE: JString read _GetBIND_ACCESSIBILITY_SERVICE;
  BIND_APPWIDGET: JString read _GetBIND_APPWIDGET;
  BIND_AUTOFILL_SERVICE: JString read _GetBIND_AUTOFILL_SERVICE;
  BIND_CARRIER_MESSAGING_SERVICE: JString read _GetBIND_CARRIER_MESSAGING_SERVICE;
  BIND_CARRIER_SERVICES: JString read _GetBIND_CARRIER_SERVICES;
  BIND_CHOOSER_TARGET_SERVICE: JString read _GetBIND_CHOOSER_TARGET_SERVICE;
  BIND_CONDITION_PROVIDER_SERVICE: JString read _GetBIND_CONDITION_PROVIDER_SERVICE;
  BIND_DEVICE_ADMIN: JString read _GetBIND_DEVICE_ADMIN;
  BIND_DREAM_SERVICE: JString read _GetBIND_DREAM_SERVICE;
  BIND_INCALL_SERVICE: JString read _GetBIND_INCALL_SERVICE;
  BIND_INPUT_METHOD: JString read _GetBIND_INPUT_METHOD;
  BIND_MIDI_DEVICE_SERVICE: JString read _GetBIND_MIDI_DEVICE_SERVICE;
  BIND_NFC_SERVICE: JString read _GetBIND_NFC_SERVICE;
  BIND_NOTIFICATION_LISTENER_SERVICE: JString read _GetBIND_NOTIFICATION_LISTENER_SERVICE;
  BIND_PRINT_SERVICE: JString read _GetBIND_PRINT_SERVICE;
  BIND_QUICK_SETTINGS_TILE: JString read _GetBIND_QUICK_SETTINGS_TILE;
  BIND_REMOTEVIEWS: JString read _GetBIND_REMOTEVIEWS;
  BIND_SCREENING_SERVICE: JString read _GetBIND_SCREENING_SERVICE;
  BIND_TELECOM_CONNECTION_SERVICE: JString read _GetBIND_TELECOM_CONNECTION_SERVICE;
  BIND_TEXT_SERVICE: JString read _GetBIND_TEXT_SERVICE;
  BIND_TV_INPUT: JString read _GetBIND_TV_INPUT;
  BIND_VISUAL_VOICEMAIL_SERVICE: JString read _GetBIND_VISUAL_VOICEMAIL_SERVICE;
  BIND_VOICE_INTERACTION: JString read _GetBIND_VOICE_INTERACTION;
  BIND_VPN_SERVICE: JString read _GetBIND_VPN_SERVICE;
  BIND_VR_LISTENER_SERVICE: JString read _GetBIND_VR_LISTENER_SERVICE;
  BIND_WALLPAPER: JString read _GetBIND_WALLPAPER;
  BLUETOOTH: JString read _GetBLUETOOTH;
  BLUETOOTH_ADMIN: JString read _GetBLUETOOTH_ADMIN;
  BLUETOOTH_PRIVILEGED: JString read _GetBLUETOOTH_PRIVILEGED;
  BODY_SENSORS: JString read _GetBODY_SENSORS;
  BROADCAST_PACKAGE_REMOVED: JString read _GetBROADCAST_PACKAGE_REMOVED;
  BROADCAST_SMS: JString read _GetBROADCAST_SMS;
  BROADCAST_STICKY: JString read _GetBROADCAST_STICKY;
  BROADCAST_WAP_PUSH: JString read _GetBROADCAST_WAP_PUSH;
  CALL_PHONE: JString read _GetCALL_PHONE;
  CALL_PRIVILEGED: JString read _GetCALL_PRIVILEGED;
  CAMERA: JString read _GetCAMERA;
  CAPTURE_AUDIO_OUTPUT: JString read _GetCAPTURE_AUDIO_OUTPUT;
  CAPTURE_SECURE_VIDEO_OUTPUT: JString read _GetCAPTURE_SECURE_VIDEO_OUTPUT;
  CAPTURE_VIDEO_OUTPUT: JString read _GetCAPTURE_VIDEO_OUTPUT;
  CHANGE_COMPONENT_ENABLED_STATE: JString read _GetCHANGE_COMPONENT_ENABLED_STATE;
  CHANGE_CONFIGURATION: JString read _GetCHANGE_CONFIGURATION;
  CHANGE_NETWORK_STATE: JString read _GetCHANGE_NETWORK_STATE;
  CHANGE_WIFI_MULTICAST_STATE: JString read _GetCHANGE_WIFI_MULTICAST_STATE;
  CHANGE_WIFI_STATE: JString read _GetCHANGE_WIFI_STATE;
  CLEAR_APP_CACHE: JString read _GetCLEAR_APP_CACHE;
  CONTROL_LOCATION_UPDATES: JString read _GetCONTROL_LOCATION_UPDATES;
  DELETE_CACHE_FILES: JString read _GetDELETE_CACHE_FILES;
  DELETE_PACKAGES: JString read _GetDELETE_PACKAGES;
  DIAGNOSTIC: JString read _GetDIAGNOSTIC;
  DISABLE_KEYGUARD: JString read _GetDISABLE_KEYGUARD;
  DUMP: JString read _GetDUMP;
  EXPAND_STATUS_BAR: JString read _GetEXPAND_STATUS_BAR;
  FACTORY_TEST: JString read _GetFACTORY_TEST;
  GET_ACCOUNTS: JString read _GetGET_ACCOUNTS;
  GET_ACCOUNTS_PRIVILEGED: JString read _GetGET_ACCOUNTS_PRIVILEGED;
  GET_PACKAGE_SIZE: JString read _GetGET_PACKAGE_SIZE;
  GET_TASKS: JString read _GetGET_TASKS;
  GLOBAL_SEARCH: JString read _GetGLOBAL_SEARCH;
  INSTALL_LOCATION_PROVIDER: JString read _GetINSTALL_LOCATION_PROVIDER;
  INSTALL_PACKAGES: JString read _GetINSTALL_PACKAGES;
  INSTALL_SHORTCUT: JString read _GetINSTALL_SHORTCUT;
  INSTANT_APP_FOREGROUND_SERVICE: JString read _GetINSTANT_APP_FOREGROUND_SERVICE;
  INTERNET: JString read _GetINTERNET;
  KILL_BACKGROUND_PROCESSES: JString read _GetKILL_BACKGROUND_PROCESSES;
  LOCATION_HARDWARE: JString read _GetLOCATION_HARDWARE;
  MANAGE_DOCUMENTS: JString read _GetMANAGE_DOCUMENTS;
  MANAGE_OWN_CALLS: JString read _GetMANAGE_OWN_CALLS;
  MASTER_CLEAR: JString read _GetMASTER_CLEAR;
  MEDIA_CONTENT_CONTROL: JString read _GetMEDIA_CONTENT_CONTROL;
  MODIFY_AUDIO_SETTINGS: JString read _GetMODIFY_AUDIO_SETTINGS;
  MODIFY_PHONE_STATE: JString read _GetMODIFY_PHONE_STATE;
  MOUNT_FORMAT_FILESYSTEMS: JString read _GetMOUNT_FORMAT_FILESYSTEMS;
  MOUNT_UNMOUNT_FILESYSTEMS: JString read _GetMOUNT_UNMOUNT_FILESYSTEMS;
  NFC: JString read _GetNFC;
  PACKAGE_USAGE_STATS: JString read _GetPACKAGE_USAGE_STATS;
  PERSISTENT_ACTIVITY: JString read _GetPERSISTENT_ACTIVITY;
  PROCESS_OUTGOING_CALLS: JString read _GetPROCESS_OUTGOING_CALLS;
  READ_CALENDAR: JString read _GetREAD_CALENDAR;
  READ_CALL_LOG: JString read _GetREAD_CALL_LOG;
  READ_CONTACTS: JString read _GetREAD_CONTACTS;
  READ_EXTERNAL_STORAGE: JString read _GetREAD_EXTERNAL_STORAGE;
  READ_FRAME_BUFFER: JString read _GetREAD_FRAME_BUFFER;
  READ_INPUT_STATE: JString read _GetREAD_INPUT_STATE;
  READ_LOGS: JString read _GetREAD_LOGS;
  READ_PHONE_NUMBERS: JString read _GetREAD_PHONE_NUMBERS;
  READ_PHONE_STATE: JString read _GetREAD_PHONE_STATE;
  READ_SMS: JString read _GetREAD_SMS;
  READ_SYNC_SETTINGS: JString read _GetREAD_SYNC_SETTINGS;
  READ_SYNC_STATS: JString read _GetREAD_SYNC_STATS;
  READ_VOICEMAIL: JString read _GetREAD_VOICEMAIL;
  REBOOT: JString read _GetREBOOT;
  RECEIVE_BOOT_COMPLETED: JString read _GetRECEIVE_BOOT_COMPLETED;
  RECEIVE_MMS: JString read _GetRECEIVE_MMS;
  RECEIVE_SMS: JString read _GetRECEIVE_SMS;
  RECEIVE_WAP_PUSH: JString read _GetRECEIVE_WAP_PUSH;
  RECORD_AUDIO: JString read _GetRECORD_AUDIO;
  REORDER_TASKS: JString read _GetREORDER_TASKS;
  REQUEST_COMPANION_RUN_IN_BACKGROUND: JString read _GetREQUEST_COMPANION_RUN_IN_BACKGROUND;
  REQUEST_COMPANION_USE_DATA_IN_BACKGROUND: JString read _GetREQUEST_COMPANION_USE_DATA_IN_BACKGROUND;
  REQUEST_DELETE_PACKAGES: JString read _GetREQUEST_DELETE_PACKAGES;
  REQUEST_IGNORE_BATTERY_OPTIMIZATIONS: JString read _GetREQUEST_IGNORE_BATTERY_OPTIMIZATIONS;
  REQUEST_INSTALL_PACKAGES: JString read _GetREQUEST_INSTALL_PACKAGES;
  RESTART_PACKAGES: JString read _GetRESTART_PACKAGES;
  SEND_RESPOND_VIA_MESSAGE: JString read _GetSEND_RESPOND_VIA_MESSAGE;
  SEND_SMS: JString read _GetSEND_SMS;
  SET_ALARM: JString read _GetSET_ALARM;
  SET_ALWAYS_FINISH: JString read _GetSET_ALWAYS_FINISH;
  SET_ANIMATION_SCALE: JString read _GetSET_ANIMATION_SCALE;
  SET_DEBUG_APP: JString read _GetSET_DEBUG_APP;
  SET_PREFERRED_APPLICATIONS: JString read _GetSET_PREFERRED_APPLICATIONS;
  SET_PROCESS_LIMIT: JString read _GetSET_PROCESS_LIMIT;
  SET_TIME: JString read _GetSET_TIME;
  SET_TIME_ZONE: JString read _GetSET_TIME_ZONE;
  SET_WALLPAPER: JString read _GetSET_WALLPAPER;
  SET_WALLPAPER_HINTS: JString read _GetSET_WALLPAPER_HINTS;
  SIGNAL_PERSISTENT_PROCESSES: JString read _GetSIGNAL_PERSISTENT_PROCESSES;
  STATUS_BAR: JString read _GetSTATUS_BAR;
  SYSTEM_ALERT_WINDOW: JString read _GetSYSTEM_ALERT_WINDOW;
  TRANSMIT_IR: JString read _GetTRANSMIT_IR;
  UNINSTALL_SHORTCUT: JString read _GetUNINSTALL_SHORTCUT;
  UPDATE_DEVICE_STATS: JString read _GetUPDATE_DEVICE_STATS;
  USE_FINGERPRINT: JString read _GetUSE_FINGERPRINT;
  USE_SIP: JString read _GetUSE_SIP;
  VIBRATE: JString read _GetVIBRATE;
  WAKE_LOCK: JString read _GetWAKE_LOCK;
  WRITE_APN_SETTINGS: JString read _GetWRITE_APN_SETTINGS;
  WRITE_CALENDAR: JString read _GetWRITE_CALENDAR;
  WRITE_CALL_LOG: JString read _GetWRITE_CALL_LOG;
  WRITE_CONTACTS: JString read _GetWRITE_CONTACTS;
  WRITE_EXTERNAL_STORAGE: JString read _GetWRITE_EXTERNAL_STORAGE;
  WRITE_GSERVICES: JString read _GetWRITE_GSERVICES;
  WRITE_SECURE_SETTINGS: JString read _GetWRITE_SECURE_SETTINGS;
  WRITE_SETTINGS: JString read _GetWRITE_SETTINGS;
  WRITE_SYNC_SETTINGS: JString read _GetWRITE_SYNC_SETTINGS;
  WRITE_VOICEMAIL: JString read _GetWRITE_VOICEMAIL;
  end; *)
Var
  NxtP: Pmsion;
  NoOfPermissions, Nxt: integer;
  PermArray: TArray<string>;
  ReqArray: TArrayOfPmsion;
  // Permissions: TJavaObjectArray<JString>;
  // GrantResults: TJavaArray<Integer>;
begin
  // if True then {test}
  // Begin
  // if Assigned(ReturnProc) then
  // ReturnProc(True, [], [], 'Permissions not granted');
  // End
  // Else
  Begin
    NoOfPermissions := 0;
    for NxtP := Pmsion.Gps to Pmsion.BlueTooth do
      if NxtP in AReq then
        case NxtP of
          Gps:
            Inc(NoOfPermissions, 2);
            // was 3 dropped  ACCESS_LOCATION_EXTRA_COMMANDS
          Camera:
            Inc(NoOfPermissions, 1);
          DataAcc:
            Inc(NoOfPermissions, 2);
          Network:
            Inc(NoOfPermissions, 2);
          WiFi:
            Inc(NoOfPermissions, 3);
          BlueTooth:
            Inc(NoOfPermissions, 2);
        end;
    SetLength(PermArray, NoOfPermissions);
    SetLength(ReqArray, NoOfPermissions);
    Nxt := 0;
    for NxtP := Pmsion.Gps to Pmsion.BlueTooth do
      if NxtP in AReq then
        case NxtP of
          Gps:
            Begin
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION);
              ReqArray[Nxt] := Gps;
              Inc(Nxt);
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION);
              ReqArray[Nxt] := Gps;
              Inc(Nxt);
              // PermArray[Nxt] :=
              // JStringToString
              // (TJManifest_permission.JavaClass.
              // ACCESS_LOCATION_EXTRA_COMMANDS);
              // ReqArray[Nxt] := Gps;
              // Inc(Nxt);
            end;
          Camera:
            Begin
              PermArray[Nxt] :=
                JStringToString(TJManifest_permission.JavaClass.Camera);
              ReqArray[Nxt] := Camera;
              Inc(Nxt);
            end;
          DataAcc:
            Begin
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);
              ReqArray[Nxt] := DataAcc;
              Inc(Nxt);
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);
              ReqArray[Nxt] := DataAcc;
              Inc(Nxt);
            end;
          Network:
            Begin
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.CHANGE_NETWORK_STATE);
              ReqArray[Nxt] := Network;
              Inc(Nxt);
              PermArray[Nxt] :=
                JStringToString(TJManifest_permission.JavaClass.INTERNET);
              ReqArray[Nxt] := Network;
              Inc(Nxt);
            end;
          WiFi:
            Begin
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.CHANGE_WIFI_STATE);
              ReqArray[Nxt] := WiFi;
              Inc(Nxt);
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.CHANGE_WIFI_MULTICAST_STATE);
              ReqArray[Nxt] := WiFi;
              Inc(Nxt);
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.ACCESS_WIFI_STATE);
              ReqArray[Nxt] := WiFi;
              Inc(Nxt);
            end;
          BlueTooth:
            Begin
              PermArray[Nxt] :=
                JStringToString(TJManifest_permission.JavaClass.BlueTooth);
              ReqArray[Nxt] := BlueTooth;
              Inc(Nxt);
              PermArray[Nxt] :=
                JStringToString
                (TJManifest_permission.JavaClass.BLUETOOTH_ADMIN);
              ReqArray[Nxt] := BlueTooth;
              Inc(Nxt);
            end;
        end;
    if Nxt = NoOfPermissions then
    Begin
      Nxt := NoOfPermissions;
      // All Good
    End
    else
    Begin
      Nxt := NoOfPermissions;
      // Something wrong
    End;
    ISRequestPermission(PermArray, ADlgPasses, ADlgFails, ReturnProc, ReqArray);
  end;
end;

{$ELSE}

Procedure PermissionsGranted(AReq: PmsmSet; ADlgPasses, ADlgFails: Boolean;
ReturnProc: TPermissionReturnProcRef { =nil } );
Begin
  if Assigned(ReturnProc) then
    ReturnProc(True, AReq, AReq, 'Dummy Permissions');

  if ADlgFails then
    TDialogService.ShowMessage('Permissions not granted:' +
      'Dummy Permissions');

  if ADlgPasses then
    TDialogService.ShowMessage('Granted:' + 'Dummy Permissions');
end;

Procedure ISRequestPermission(APermArray: TArray<string>;
ADlgPasses, ADlgFails: Boolean; ReturnProc: TPermissionReturnProcRef);
Begin
  PermissionsGranted([Gps], ADlgPasses, ADlgFails, ReturnProc);
End;

{$ENDIF}


end.
