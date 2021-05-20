unit GoogleDirForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation;

type
  TDirections = class(TForm)
    LblFrom: TLabel;
    lbllong: TLabel;
    EdtLong: TEdit;
    LblLat: TLabel;
    EdtLat: TEdit;
    BtnGoGoogle: TButton;
    LblLocation: TLabel;
    LblTo: TLabel;
    LblLong2: TLabel;
    EdtLong2: TEdit;
    LblLat2: TLabel;
    EdtLat2: TEdit;
    LblLocation2: TLabel;
    EdtGoogleLink: TEdit;
    CBxGoNow: TCheckBox;
    LBlCrowFlies: TLabel;
    procedure EdtFromChange(Sender: TObject);
    procedure EdtToChange(Sender: TObject);
    procedure BtnGoGoogleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Procedure EditChangeData(AEdtLong,AEdtLat:TEdit;ALabel:TLabel);
  public
    { Public declarations }
  end;

var
  Directions: TDirections;

implementation

{$R *.fmx}

uses IsNavUtils;

{ TDirections }

procedure TDirections.BtnGoGoogleClick(Sender: TObject);
Var
  LocFrom,LocTo:RNavigateLongLat;
begin
  LocFrom.CreateDec(StrToFloat(EdtLong.Text),StrToFloat(EdtLat.Text));
  LocTo.CreateDec(StrToFloat(EdtLong2.Text),StrToFloat(EdtLat2.Text));
  EdtGoogleLink.Text:=LocFrom.GoogleLinkDirectionsTo(LocTo);
  LBlCrowFlies.Text:='Distance as Crow Flys ='+FormatFloat('0.0km',LocTo.MetresFrom(LocFrom)/1000);
  if CBxGoNow.IsChecked then
     LocFrom.GoGoogleDirectionsTo(LocTo);
end;

procedure TDirections.EditChangeData(AEdtLong, AEdtLat: TEdit; ALabel: TLabel);
Var
  Loc:RNavigateLongLat;
begin
  Loc.CreateDec(StrToFloat(AEdtLong.Text),StrToFloat(AEdtLat.Text));
  ALabel.Text:=Loc.LocatationText(3);
end;

procedure TDirections.EdtFromChange(Sender: TObject);
begin
  EditChangeData(EdtLong,EdtLat,LblLocation);
end;

procedure TDirections.EdtToChange(Sender: TObject);
begin
  EditChangeData(EdtLong2,EdtLat2,LblLocation2);
end;

procedure TDirections.FormCreate(Sender: TObject);
begin
   EdtFromChange(nil);
   EdtToChange(nil);
   LBlCrowFlies.Text:='Distance as Crow Flys';
end;

end.
