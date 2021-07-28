unit PlayGoogleTabbedForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, IsNavUtils, FMX.Edit;

type
  TTabbedwithNavigationForm = class(TForm)
    TabControl1: TTabControl;
    TbItmPageOne: TTabItem;
    TbCtrlPgOne: TTabControl;
    TbItmAllWork: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnNext: TSpeedButton;
    TabItem6: TTabItem;
    ToolBar2: TToolBar;
    lblTitle2: TLabel;
    btnBack: TSpeedButton;
    TabItem2: TTabItem;
    ToolBar3: TToolBar;
    lblTitle3: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    BtnGoGoogle: TButton;
    EdtLong: TEdit;
    EdtLat: TEdit;
    lbllong: TLabel;
    LblLat: TLabel;
    LblLocation: TLabel;
    EdtScale: TEdit;
    LblScale: TLabel;
    BtnYMMB_NDB: TButton;
    BtnPointCook_NDB: TButton;
    BtnCowes: TButton;
    BtnWonthaggi: TButton;
    BtnSetLocation2: TButton;
    BtnGoogleDir: TButton;
    LblLocationTwo: TLabel;
    LblDistance: TLabel;
    LblOneToTwoBearing: TLabel;
    LblSource: TLabel;
    LblDestination: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    StyleBook1: TStyleBook;
    EdtNewName: TEdit;
    BtnMyLocation: TButton;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure BtnGoGoogleClick(Sender: TObject);
    procedure EdtBoxExit(Sender: TObject);
    procedure BtnYMMB_NDBClick(Sender: TObject);
    procedure BtnPointCook_NDBClick(Sender: TObject);
    procedure BtnCowesClick(Sender: TObject);
    procedure BtnWonthaggiClick(Sender: TObject);
    procedure BtnSetLocation2Click(Sender: TObject);
    procedure BtnGoogleDirClick(Sender: TObject);
    procedure EdtNewNameExit(Sender: TObject);
    procedure BtnMyLocationClick(Sender: TObject);
  private
    { Private declarations }
    FLocationTwo:RNavigateLongLat;
    Procedure DoLocationTwo;
  public
    { Public declarations }
  end;

var
  TabbedwithNavigationForm: TTabbedwithNavigationForm;

implementation

{$R *.fmx}

procedure TTabbedwithNavigationForm.BtnCowesClick(Sender: TObject);
begin
  EdtLong.Text:=FloatToStr(145 + 12.8 / 60);
  EdtLat.Text:=FloatToStr(-38 - 30.5 / 60);
  LblSource.Text:='Old Cowes NDB';
  DoLocationTwo;
end;

procedure TTabbedwithNavigationForm.BtnGoGoogleClick(Sender: TObject);
Var
  Loc:RNavigateLongLat;
  Long,Lat:Double;
begin
 Long:=RealFrmDegreeText(EdtLong.Text);
 Lat:= RealFrmDegreeText(EdtLat.Text);
  Loc.CreateDec(Long,Lat);
  Loc.GoogleScale:=StrToInt(EdtScale.Text);
  EdtScale.Text:=IntToStr(Loc.GoogleScale);
  LblLocation.Text:=Loc.LocatationText(3);
  Loc.GoGoogle;
end;

procedure TTabbedwithNavigationForm.BtnGoogleDirClick(Sender: TObject);
Var
   Loc:RNavigateLongLat;
  Long,Lat:Double;
begin
  EdtBoxExit(nil);
  Long:=RealFrmDegreeText(EdtLong.Text);
  Lat:= RealFrmDegreeText(EdtLat.Text);
  if FLocationTwo.NotNull then
   Begin
   Loc.CreateDec(Long,Lat);
   if (Loc.Longitude<>FLocationTwo.Longitude)Or(Loc.Latitude<>FLocationTwo.Latitude) then
       Loc.GoGoogleDirectionsTo(FLocationTwo,2);
  end;
end;

procedure TTabbedwithNavigationForm.BtnMyLocationClick(Sender: TObject);
begin
  EdtNewName.Visible:=True;
  EdtNewName.Text:=LblSource.text;
  EdtNewName.Visible:=true;
end;

procedure TTabbedwithNavigationForm.BtnPointCook_NDBClick(Sender: TObject);
begin
  EdtLong.Text:=FloatToStr(144 + 45.3 / 60);
  EdtLat.Text:=FloatToStr(-37 - 55.7 / 60);
  LblSource.Text:='Point Cook Airport';
  DoLocationTwo;
end;

procedure TTabbedwithNavigationForm.BtnSetLocation2Click(Sender: TObject);
Var
  Long,Lat:Double;
begin
  Long:=RealFrmDegreeText(EdtLong.Text);
  Lat:= RealFrmDegreeText(EdtLat.Text);
  EdtBoxExit(Sender);
  FLocationTwo.CreateDec(Long,Lat);
  LblDestination.Text:=LblSource.Text;
  DoLocationTwo;
end;

procedure TTabbedwithNavigationForm.BtnWonthaggiClick(Sender: TObject);
begin
  EdtLong.Text:=FloatToStr(145 + 37.4 / 60);
  EdtLat.Text:=FloatToStr(-38 - 28.3 / 60);
  LblSource.Text:='Old Wonthaggi NDB';
  DoLocationTwo;
end;

procedure TTabbedwithNavigationForm.BtnYMMB_NDBClick(Sender: TObject);
begin
  EdtLong.Text:=FloatToStr(145 + 5.4 / 60);
  EdtLat.Text:=FloatToStr(-37 - 58.6 / 60);
  LblSource.Text:='Moorabin Airport';
  DoLocationTwo;
end;

procedure TTabbedwithNavigationForm.DoLocationTwo;

Var
  Loc:RNavigateLongLat;
  Meters,Bearing,BBearing:Double;
  Long,Lat:Double;
begin
 Long:=RealFrmDegreeText(EdtLong.Text);
 Lat:= RealFrmDegreeText(EdtLat.Text);
 LblLocationTwo.Text:='';
 LblDistance.Text:='';
 LblOneToTwoBearing.Text:='';
 if FLocationTwo.NotNull then
  begin
   LblLocationTwo.Text:=FLocationTwo.LocatationText(3);
   Loc.CreateDec(Long,Lat);
   if (Loc.Longitude<>FLocationTwo.Longitude)Or(Loc.Latitude<>FLocationTwo.Latitude) then
      Begin
       Meters:=FLocationTwo.MetresFromPrecision(Loc, Bearing,BBearing);
       LblDistance.Text:=FormatFloat('00.',Meters)+' Meters From '+LblSource.Text;
       LblOneToTwoBearing.Text:='At '+TextDegreeMinuteSecond(Bearing,3);
      End;
  end;

end;

procedure TTabbedwithNavigationForm.EdtBoxExit(Sender: TObject);
Var
  Loc:RNavigateLongLat;
  Long,Lat:Double;
begin
  Long:=RealFrmDegreeText(EdtLong.Text);
  Lat:= RealFrmDegreeText(EdtLat.Text);
  Loc.CreateDec(Long,Lat);
  Loc.GoogleScale:=StrToInt(EdtScale.Text);
  EdtScale.Text:=IntToStr(Loc.GoogleScale);
  LblLocation.Text:=Loc.LocatationText(3);
  DoLocationTwo;
end;

procedure TTabbedwithNavigationForm.EdtNewNameExit(Sender: TObject);
begin
   EdtNewName.Visible:=false;
   LblSource.Text:=EdtNewName.Text;
   LblSource.Visible:=true;
end;

procedure TTabbedwithNavigationForm.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TbItmPageOne;

  BtnYMMB_NDBClick(nil);
end;

procedure TTabbedwithNavigationForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (TabControl1.ActiveTab = TbItmPageOne) and (TbCtrlPgOne.ActiveTab = TabItem6) then
    begin
      TbCtrlPgOne.Previous;
      Key := 0;
    end;
  end;
end;

procedure TTabbedwithNavigationForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount - 1] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

end.

