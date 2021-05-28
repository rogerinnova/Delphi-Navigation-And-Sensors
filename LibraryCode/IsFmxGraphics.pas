unit IsFmxGraphics;

interface

{$I InnovaMultiPlatLibDefs.inc}

uses System.Types, IsNavUtils, Fmx.Graphics, System.UITypes, System.math,
  Fmx.Types, Fmx.Controls,
  Fmx.Objects, System.math.Vectors,
  System.Generics.Collections;

Type

  TIsGraphics = Class(Tobject)
  Private
    class Function SizeCanvasFont(ACanvas: TCanvas;
      ALengthLimit, AHeightLimit: Single; ATxt: String): TRectf;
  Protected
    Class Procedure ClearCanvas(ACanvas: TCanvas);
    Class Function PointOutsideBounds(ACanvas: TCanvas;
      APoint: TPointf): Boolean;
    Class Function RectOutsideBounds(ACanvas: TCanvas;
      ARect: TRectf): Boolean;
    Class Procedure DrawHorizontalScaleBar(ACanvas: TCanvas; ABar: Single;
      AOrigin: TPointf; Const ATxt: String);
    Class Procedure DrawVerticalScaleBar(ACanvas: TCanvas; ABar: Single;
      AOrigin: TPointf; Const ATxt: String);
    Class Procedure RectangleToPath(APath: TPathData; const ARect: TRectf);
    Class Procedure AddText(ACanvas: TCanvas; ATxt: String;
      Const AFractionFromTop, AFractionPerLine: Single;
      AAlignment: TTextAlign = TTextAlign.Center; ADoFill: Boolean = true;
      ADoStroke: Boolean = False);
  Public
    Class Procedure DrawDot(ALocation: TPointf; ACanvas: TCanvas;
      ASolid: Boolean = true);
    Class Function LocationAsDraw(AMouseLocation: TPointf;
      ACanvas: TCanvas): TPointf;
    Class Function LocationAsMouse(ADrawLocation: TPointf;
      ACanvas: TCanvas): TPointf;
    Class Function SetNewImageBitMap(AImage: TImage;
      ABackGround: TAlphaColor = $FFFFFF): Boolean;
  End;

  TNavGraphics = Class(TIsGraphics)
    // Does not handle International dateline
  Private
    Class Function NavAsPoint(ALocation: RNavigateLongLat; AScale: TPointf;
      AOrigin: RNavigateLongLat; AOffset: TPointf): TPointf;
    Class Procedure AddScale(ACanvas: TCanvas; AScale: TPointf;
      AHorizontal: Boolean; AAltPos: Boolean = False);
  Public

    Class Function GetScale(ACanvas: TCanvas;
      ALocationList: TList<RNavigateLongLat>; Var Origin: RNavigateLongLat;
      Out CenterOffset: TPointf): TPointf;
    Class Procedure RelocateNavPointFromMapRef(Var ALatLongLocationRef
      : RNavigateLongLat; AScaleOfMap, ANewPosCanvas, AOrigRefOffset: TPointf);
    Class Procedure DrawLocationsOnCanvas(ACanvas: TCanvas;
      ALocationList: TList<RNavigateLongLat>; AOrigin: RNavigateLongLat;
      AScale, ACenterOffset: TPointf; AAddScale: Integer = 1 { Horizontal };
      Const AHdr: String = ''; AIgnorJitterMeters: Single = 25.0);
  End;

implementation

{ TNavGrahics }

class procedure TNavGraphics.AddScale(ACanvas: TCanvas; AScale: TPointf;
  AHorizontal: Boolean; AAltPos: Boolean = False);
Var
  s, TxtLead: String;
  Bar, MaxBar, CanvasScale: Single;
  Origin: TPointf;
  Pwr: Integer;
begin
  ACanvas.Stroke.Color := TAlphaColorRec.Crimson;
  ACanvas.Stroke.Thickness := 1;
  CanvasScale := ACanvas.Scale;
  if CanvasScale < 1 then
    CanvasScale := 1;
  if AHorizontal then
    MaxBar := 2 * ACanvas.width / CanvasScale / 3
  Else
    MaxBar := 2 * ACanvas.height / CanvasScale / 3;
  Pwr := -1;
  Bar := 0;
  while Bar < MaxBar do
  Begin
    inc(Pwr);
    Bar := OneMeterAsDegrees * IntPower(10, Pwr) * AScale.X;
  End;
  if Bar < 1 then
    Exit;
  Dec(Pwr);
  Bar := OneMeterAsDegrees * IntPower(10, Pwr) * AScale.X;
  if Bar < MaxBar / 5 then
  Begin
    Bar := Bar * 5;
    TxtLead := '5'
  End
  Else if Bar < MaxBar / 2 then
  Begin
    Bar := Bar * 2;
    TxtLead := '2'
  End
  Else
    TxtLead := '1';

  case Pwr of
    - 1:
      s := '0.' + TxtLead + '00 mm';
    0:
      s := TxtLead + ' m';
    1:
      s := TxtLead + '0 m';
    2:
      s := TxtLead + '00 m';
    3:
      s := TxtLead + ' km';
    4:
      s := TxtLead + '0 km';
    5:
      s := TxtLead + '00 km';
    6:
      s := TxtLead + '000 km';
  else
    s := '';
  end;
  // Origin:=TPointf.Create(ACanvas.width ,ACanvas.height);
  if AAltPos then
  Begin
    if AHorizontal then
      Origin := TPointf.Create(ACanvas.width / CanvasScale / 2,
        1 * ACanvas.height / CanvasScale / 10)
    else
      Origin := TPointf.Create(1 * ACanvas.width / CanvasScale / 10,
        ACanvas.height / CanvasScale / 2);
  End
  Else if AHorizontal then
    Origin := TPointf.Create(ACanvas.width / CanvasScale / 2,
      9 * ACanvas.height / CanvasScale / 10)
  else
    Origin := TPointf.Create(9 * ACanvas.width / CanvasScale / 10,
      ACanvas.height / CanvasScale / 2);
  Origin.Offset(ACanvas.Offset);
  if AHorizontal then
    DrawHorizontalScaleBar(ACanvas, Bar * AScale.y / AScale.X, Origin, s)
  Else
    DrawVerticalScaleBar(ACanvas, Bar, Origin, s);
end;

class procedure TNavGraphics.DrawLocationsOnCanvas(ACanvas: TCanvas;
  ALocationList: TList<RNavigateLongLat>; AOrigin: RNavigateLongLat;
  AScale, ACenterOffset: TPointf; AAddScale: Integer = 1 { Horizontal };
  Const AHdr: String = ''; AIgnorJitterMeters: Single = 25.0);
// Takes a list of Lat/Long Records and plots the values on a canvas.
// Accepts a Header Text, A scale bar V/H option.
// Can be made to attempt to smooth out jitter. Sett the Jitter parameter

Var
  i: Integer;
  Path: TPathData;
  NodeRect, ThisNodeRect: TRectf;
  Nxt: TPointf;
  SaveStrokeBrush: TStrokeBrush;
  SavePaintBrush: TBrush;
  PrevJitterLoc: RNavigateLongLat;
  Dots: TList<RNavigateLongLat>;
  ApplyJitterSupression: Boolean;
begin
  if ALocationList.Count < 0 then
    Exit;
  if ALocationList.Count < 5 then
    ApplyJitterSupression := False
  else
    ApplyJitterSupression := AIgnorJitterMeters > 0.001;
  ClearCanvas(ACanvas);
  SaveStrokeBrush := TStrokeBrush.Create(TBrushKind.Solid,
    TAlphaColorRec.Black);
  SavePaintBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
  Try
    SaveStrokeBrush.Assign(ACanvas.Stroke);
    SavePaintBrush.Assign(ACanvas.Fill);
    if ACanvas.Stroke.Kind = TBrushKind.None then
    Begin
      ACanvas.Stroke.Thickness := 5;
      ACanvas.Stroke.Color := TAlphaColorRec.Black;
      ACanvas.Stroke.Kind := TBrushKind.Solid;
    End;

    Path := TPathData.Create;
    Dots := TList<RNavigateLongLat>.Create;
    Try
      if ApplyJitterSupression then
      Begin
        // To show centers
        // {$IFNDEF ISD103R_DELPHI}
        // ThisNodeRect.Create(0.0, 0.0, 100, 100);
        // AOrigin := ALocationList[0];
        // ThisNodeRect.Offset(-ThisNodeRect.width / 2, -ThisNodeRect.height / 2);
        // ThisNodeRect.Offset(NavAsPoint(AOrigin, AScale, AOrigin,
        // ACenterOffset));
        // Path.AddEllipse(ThisNodeRect);
        // {$ENDIF}
        NodeRect.Create(0.0, 0.0, OneMeterAsDegrees * AScale.y,
          OneMeterAsDegrees * AScale.y);
        NodeRect.Offset(-NodeRect.width / 2, -NodeRect.height / 2);
        ThisNodeRect := NodeRect;
        PrevJitterLoc := ALocationList[0];
        ThisNodeRect.Offset(NavAsPoint(ALocationList[0], AScale, AOrigin,
          ACenterOffset));
        Path.AddEllipse(ThisNodeRect);
      End;
      Path.MoveTo(NavAsPoint(ALocationList[0], AScale, AOrigin, ACenterOffset));
      Dots.Add(ALocationList[0]);
      for i := 1 to ALocationList.Count - 1 do
      Begin
        if ApplyJitterSupression then
        Begin
          If (i = ALocationList.Count - 1) Or
            (PrevJitterLoc.MetresFrom(ALocationList[i]) > AIgnorJitterMeters)
          then
          Begin
            PrevJitterLoc := ALocationList[i];
            Nxt := NavAsPoint(PrevJitterLoc, AScale, AOrigin, ACenterOffset);
            if not PointOutsideBounds(ACanvas,Nxt) then
                                           Path.LineTo(Nxt);
            Dots.Add(PrevJitterLoc);
          End;
        End
        else
        Begin
          Dots.Add(ALocationList[i]);
          Nxt := NavAsPoint(ALocationList[i], AScale, AOrigin, ACenterOffset);
          // if PointOutsideBounds(ACanvas, Nxt) then
          // Path.LineTo(CenterOffset)
          // Else
          Path.LineTo(Nxt);
        End;
      End;
      ACanvas.BeginScene;
      ACanvas.DrawPath(Path, 0.3);
      ACanvas.EndScene;
      Path.Clear;

      NodeRect.Create(0.0, 0.0, OneMeterAsDegrees / 5 { *Scale.X/Scale.X } *
        AScale.y, OneMeterAsDegrees / 5 * AScale.y); // Dot size is 20 cm
      NodeRect.Offset(-NodeRect.width / 2, -NodeRect.height / 2);
      if Dots.Count > 0 then
        for i := 0 to Dots.Count - 1 do
        begin
          ThisNodeRect := NodeRect;
          ThisNodeRect.Offset(NavAsPoint(Dots[i], AScale, AOrigin,
            ACenterOffset));
            if not RectOutsideBounds(ACanvas,ThisNodeRect) then
              Path.AddEllipse(ThisNodeRect);
        end;
      ACanvas.Stroke.Thickness := 1;
      ACanvas.Fill.Color := TAlphaColorRec.Crimson;
      ACanvas.BeginScene;
      ACanvas.FillPath(Path, 0.3);
      ACanvas.EndScene;
    Finally
      Path.Free;
      Dots.Free;
    End;
    ACanvas.Stroke.Thickness := 1;
    ACanvas.Fill.Color := ACanvas.Stroke.Color;
    if AHdr <> '' then
      AddText(ACanvas, AHdr, 0.1, 0.05);
    case AAddScale of
      0:
        ;
      1:
        AddScale(ACanvas, AScale, true);
      2:
        AddScale(ACanvas, AScale, False);
      3:
        AddScale(ACanvas, AScale, true, true);
      4:
        AddScale(ACanvas, AScale, False, true);
    end;
  Finally
    ACanvas.Stroke.Assign(SaveStrokeBrush);
    ACanvas.Fill.Assign(SavePaintBrush);
    SaveStrokeBrush.Free;
    SavePaintBrush.Free;
  End;
end;

class function TNavGraphics.GetScale(ACanvas: TCanvas;
  ALocationList: TList<RNavigateLongLat>; Var Origin: RNavigateLongLat;
  Out CenterOffset: TPointf): TPointf;
// Takes a list of Lat/Long Records and a canvas and returns the degrees to pixel scale factors required to plot the points. It also returns the center of the points provided as Pixels (CenterOffset) and Lat/Long (Origin)

Var
  MinX, MinY, MaxX, MaxY: Double;
  Rslt2, AvailX, AvailY: Single;
  i: Integer;
  LOffset: TPointf;
  LatitudeFactor: Single;
  Tst: RNavigateLongLat;
begin
  if ALocationList = nil then
  begin
    Result := TPointf.Create(-0.02, -0.02);
    Exit;
  end;

  if ALocationList.Count < 1 then
  begin
    Result := TPointf.Create(-0.02, -0.02);
    Exit;
  end;

  Result := TPointf.Create(-0.01, -0.01);
  if ACanvas.Scale < 1 then
  Begin
    AvailX := ACanvas.width;
    AvailY := ACanvas.height;
  End
  Else
  begin
    AvailX := ACanvas.width / ACanvas.Scale;
    AvailY := ACanvas.height / ACanvas.Scale;
  end;
  LOffset := ACanvas.Offset;
  CenterOffset.X := LOffset.X + AvailX / 2;
  CenterOffset.y := LOffset.y + AvailY / 2;

  if ALocationList.Count = 1 then
  Begin
    Origin := ALocationList[0];
    // LatitudeFactor := Cos(ALocationList[0].Latitude / 2 / 180 * Pi);
    LatitudeFactor := Cos(ALocationList[0].LatAsRad);
    Result.X := AvailX / (10 * OneMeterAsDegrees * LatitudeFactor);
    Result.y := AvailY / (10 * OneMeterAsDegrees);
    if Result.y < Result.X then
      Result.X := Result.y / LatitudeFactor
    else
      Result.y := Result.X * LatitudeFactor;
    Exit;
  End;

  MinX := ALocationList[0].Longitude;
  MaxX := MinX;
  MinY := ALocationList[0].Latitude;
  MaxY := MinY;

  for i := 1 to ALocationList.Count - 1 do
  Begin
    Tst := ALocationList[i];
    if MinX > Tst.Longitude then
      MinX := Tst.Longitude;
    if MaxX < Tst.Longitude then
      MaxX := Tst.Longitude;
    if MinY > Tst.Latitude then
      MinY := Tst.Latitude;
    if MaxY < Tst.Latitude then
      MaxY := Tst.Latitude;
  End;

  LatitudeFactor := Cos((MinY + MaxY) / 2 / 180 * Pi);

  if (MaxX <> MinX) then
  Begin
    Result.X := AvailX / ((MaxX - MinX) / LatitudeFactor);
    if Result.X < 0.0 then
      Result.X := -Result.X;
  End;
  if (MaxY <> MinY) then
  Begin
    Rslt2 := AvailY / (MaxY - MinY);
    if Rslt2 < 0.0 then
      Rslt2 := -Rslt2;
    if (Result.X < 0.0) or (Rslt2 < Result.X) then
    Begin
      Result.y := Rslt2;
      Result.X := Result.y * LatitudeFactor;
    End
    Else
      Result.y := Result.X / LatitudeFactor;
  End;
  If Origin.NotNull Then
  Else
  Begin
    Origin.Longitude := (MaxX + MinX) / 2;
    Origin.Latitude := (MaxY + MinY) / 2;
  End;
  Result := Result * 0.9;
end;

class function TNavGraphics.NavAsPoint(ALocation: RNavigateLongLat;
  AScale: TPointf; AOrigin: RNavigateLongLat; AOffset: TPointf): TPointf;
// Returns a scaled plot point TPointf from a Navigation Reference
begin
  // Result.X := (ALocation.Longitude - AOrigin.Longitude) * AScale.X*Abs(Sin(AOrigin.LatAsRad));
  Result.X := (ALocation.Longitude - AOrigin.Longitude) * AScale.X;
  // Scale X is adjusted for Latitude
  Result.y := (AOrigin.Latitude - ALocation.Latitude) * AScale.y;
  // y Positive is down
  Result.Offset(AOffset);
end;

class procedure TNavGraphics.RelocateNavPointFromMapRef(var ALatLongLocationRef
  : RNavigateLongLat; AScaleOfMap, ANewPosCanvas, AOrigRefOffset: TPointf);
// Takes a RNavigateLongLat Record and calculates a new value based on the original map x,y pixel reference, the map scale and a desired position given in Map x,y pixel coordinates

Var
  Shift: TPointf;
begin
  Shift.X := ANewPosCanvas.X;
  Shift.y := ANewPosCanvas.y;
  Shift := -Shift;
  Shift.Offset(AOrigRefOffset);
  ALatLongLocationRef.Longitude := ALatLongLocationRef.Longitude - Shift.X /
    AScaleOfMap.X; // * Cos(ALatLongLocationRef.LatAsRad);
  // Because AScaleOfMap.X incorporates adjustment
  ALatLongLocationRef.Latitude := ALatLongLocationRef.Latitude + Shift.y /
    AScaleOfMap.y;
end;

{ TIsGraphics }

class procedure TIsGraphics.AddText(ACanvas: TCanvas; ATxt: String;
  const AFractionFromTop, AFractionPerLine: Single;
  AAlignment: TTextAlign = TTextAlign.Center; ADoFill: Boolean = true;
  ADoStroke: Boolean = False);
Var
  PathD: TPathData;
  TextRect, PathRect: TRectf;
  AlignOffset, CanvasScale: Single;
  TextHeightLimit, ScaledWidth, ScaledHeight: Single;
begin
  PathD := TPathData.Create;
  try
    CanvasScale := ACanvas.Scale;
    ScaledWidth := ACanvas.width;
    ScaledHeight := ACanvas.height;
    if CanvasScale > 1 then
    Begin // no idea why but seems to be required Scale < 1 on Pocket Neo
      ScaledHeight := ScaledHeight / CanvasScale;
      ScaledWidth := ScaledWidth / CanvasScale;
    End;
    TextRect.Create(ACanvas.Offset, ScaledWidth,
      ScaledHeight * AFractionPerLine);
    TextHeightLimit := ScaledHeight * AFractionFromTop;
    TextRect.Offset(0.0, TextHeightLimit);
    TextRect.Left := TextRect.Left + TextRect.width * 0.05;
    TextRect.width := TextRect.width * 0.9;

    PathRect := SizeCanvasFont(ACanvas, TextRect.width, TextRect.height, ATxt);
    Case AAlignment of
      TTextAlign.Center:
        AlignOffset := ScaledWidth / 2 - PathRect.width / 2;
      TTextAlign.Leading:
        AlignOffset := TextRect.Left;
      TTextAlign.Trailing:
        AlignOffset := TextRect.Right - PathRect.width;
    else
      AlignOffset := 0;
    End;
    PathRect.Offset(AlignOffset, TextRect.Top);
    ACanvas.TextToPath(PathD, PathRect, ATxt, False, AAlignment);
    PathD.FitToRect(PathRect);
    ACanvas.BeginScene;
    if ADoStroke then
      ACanvas.DrawPath(PathD, 1.0);
    if ADoFill then
      ACanvas.FillPath(PathD, 1.0);
    ACanvas.EndScene;

  finally
    PathD.Free;
  end;
end;

class procedure TIsGraphics.ClearCanvas(ACanvas: TCanvas);
Var
  Rect: TRectf;
  PathD: TPathData;

begin
  Rect.Create(ACanvas.Offset, ACanvas.width / ACanvas.Scale,
    ACanvas.height / ACanvas.Scale);
  ACanvas.BeginScene;
  ACanvas.FillRect(Rect, 0.0, 0.0, AllCorners, 1.0);
  ACanvas.EndScene;

  PathD := TPathData.Create;
  Try
    PathD.AddEllipse(Rect);
    // Remove Comments to show extent of canvas for debugging
    // ACanvas.BeginScene;
    // ACanvas.DrawPath(PathD, 1.0);
    // ACanvas.EndScene;
  Finally
    PathD.Free;
  End;
end;

class procedure TIsGraphics.DrawDot(ALocation: TPointf; ACanvas: TCanvas;
  ASolid: Boolean = true);
Var
  Path: TPathData;
  NodeRect: TRectf;
  Brush: TBrush;
  Stroke: TStrokeBrush;
begin
  if ACanvas = nil then
    Exit;

  NodeRect.Create(-5, -5, 5, 5);
  NodeRect.Offset(ALocation);
  Path := TPathData.Create;
  Try
    if Not RectOutsideBounds(ACanvas,NodeRect) then
      Begin
       Path.AddEllipse(NodeRect);
       if ASolid then
          Brush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black)
        else
          Brush:=nil;
       Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
       try
         Stroke.Thickness := 2;
         ACanvas.BeginScene;
         if ASolid then
           ACanvas.FillPath(Path, 0.5, Brush);
         ACanvas.DrawPath(Path, 0.5,Stroke);
         ACanvas.EndScene;
       finally
         Brush.Free;
         Stroke.Free;
       end;
      End;
  Finally
    Path.Free;
  End;
end;

class procedure TIsGraphics.DrawHorizontalScaleBar(ACanvas: TCanvas;
  ABar: Single; AOrigin: TPointf; const ATxt: String);
Var
  PathD: TPathData;
  // PathO: TPath;
  BarTck: Single;
  SvFontSz: Single;
  PathRect: TRectf;
  Nxt: TPointf;
begin
  BarTck := ABar / 20;
  SvFontSz := ACanvas.Font.Size;
  PathD := TPathData.Create;
  try
    Nxt := TPointf.Create(AOrigin.X - ABar / 2, AOrigin.y - BarTck);
    PathD.MoveTo(Nxt);
    Nxt.Offset(0.0, 2 * BarTck);
    PathD.LineTo(Nxt);
    Nxt.Offset(0.0, -BarTck);
    PathD.MoveTo(Nxt);
    if ATxt <> '' then
    Begin
      PathRect := SizeCanvasFont(ACanvas, ABar * 0.9, 2 * BarTck, ATxt);
      PathRect.Offset(AOrigin.X - PathRect.width / 2,
        AOrigin.y - PathRect.height / 2);
      Nxt.Offset(ABar / 2 - PathRect.width / 2 - PathRect.height / 4, 0.0);
      // To Start of text
      PathD.LineTo(Nxt);
      Nxt.Offset(PathRect.width + PathRect.height / 2, 0.0); // To End of Text
      PathD.MoveTo(Nxt);
      Nxt.Offset(ABar / 2 - (PathRect.width + PathRect.height) / 2, 0.0);
      // To End of text
    end
    Else
      Nxt.Offset(ABar, 0.0);
    PathD.LineTo(Nxt);
    Nxt.Offset(0.0, -BarTck);
    PathD.MoveTo(Nxt);
    Nxt.Offset(0.0, 2 * BarTck);
    PathD.LineTo(Nxt);
    ACanvas.BeginScene;
    ACanvas.DrawPath(PathD, 1.0);
    ACanvas.EndScene;
    if ATxt <> '' then
    Begin
      PathD.Clear;
      ACanvas.TextToPath(PathD, PathRect, ATxt, False, TTextAlign.Center,
        TTextAlign.Center);
      PathD.FitToRect(PathRect);
      ACanvas.Fill.Color := TAlphaColorRec.Red;
      ACanvas.BeginScene;
      ACanvas.FillPath(PathD, 1.0);
      ACanvas.EndScene;
    End;
  finally
    PathD.Free;
    ACanvas.Font.Size := SvFontSz;
  end;
end;

Class Procedure TIsGraphics.DrawVerticalScaleBar(ACanvas: TCanvas; ABar: Single;
  AOrigin: TPointf; Const ATxt: String);
Var
  PathD: TPathData;
  // PathO: TPath;
  BarTck: Single;
  SvFontSz: Single;
  PathRect: TRectf;
  Nxt: TPointf;

begin
  BarTck := ABar / 20;
  SvFontSz := ACanvas.Font.Size;
  PathD := TPathData.Create;
  try
    Nxt := TPointf.Create(AOrigin.X - BarTck, AOrigin.y - ABar / 2);
    PathD.MoveTo(Nxt);
    Nxt.Offset(2 * BarTck, 0.0);
    PathD.LineTo(Nxt);
    Nxt.Offset(-BarTck, 0.0);
    PathD.MoveTo(Nxt);
    if ATxt <> '' then
    Begin
      PathRect := SizeCanvasFont(ACanvas, ABar * 0.9, 2 * BarTck, ATxt);
      Nxt.Offset(0.0, ABar / 2 - PathRect.width / 2 - PathRect.height / 4);
      // To Start of text
      PathD.LineTo(Nxt);
      Nxt.Offset(0.0, PathRect.width + PathRect.height / 2); // To End of Text
      PathD.MoveTo(Nxt);
      Nxt.Offset(0.0, ABar / 2 - (PathRect.width + PathRect.height) / 2);
      // To End of text
    end
    Else
      Nxt.Offset(0.0, ABar);
    PathD.LineTo(Nxt);
    Nxt.Offset(-BarTck, 0.0);
    PathD.MoveTo(Nxt);
    Nxt.Offset(2 * BarTck, 0.0);
    PathD.LineTo(Nxt);
    ACanvas.BeginScene;
    ACanvas.DrawPath(PathD, 1.0);
    ACanvas.EndScene;
    if ATxt <> '' then
    Begin
      PathD.Clear;
      ACanvas.TextToPath(PathD, PathRect, ATxt, False, TTextAlign.Center,
        TTextAlign.Center);
      if AOrigin.X > (ACanvas.width / 2) then
        PathD.ApplyMatrix(TMatrix.CreateRotation(-Pi / 2))
      Else
        PathD.ApplyMatrix(TMatrix.CreateRotation(Pi / 2));

      PathRect.Create(0, 0, PathRect.height, PathRect.width); // Rotate 90

      PathRect.Offset(AOrigin.X - PathRect.width / 2,
        AOrigin.y - PathRect.height / 2);

      // rotate

      PathD.FitToRect(PathRect);
      ACanvas.Fill.Color := TAlphaColorRec.Red;
      ACanvas.BeginScene;
      ACanvas.FillPath(PathD, 1.0);
      ACanvas.EndScene;
    End;
  finally
    PathD.Free;
    ACanvas.Font.Size := SvFontSz;
  end;
end;

class function TIsGraphics.LocationAsDraw(AMouseLocation: TPointf;
  ACanvas: TCanvas): TPointf;
begin
  Result := AMouseLocation;
  if ACanvas <> nil then
    if ACanvas.Scale > 1 then
      Result := AMouseLocation / ACanvas.Scale;
end;

class function TIsGraphics.LocationAsMouse(ADrawLocation: TPointf;
  ACanvas: TCanvas): TPointf;
begin
  Result := ADrawLocation;
  if ACanvas <> nil then
    if ACanvas.Scale > 1 then
      Result := ADrawLocation * ACanvas.Scale;
end;

class function TIsGraphics.PointOutsideBounds(ACanvas: TCanvas;
  APoint: TPointf): Boolean;
Var
  Scale:single;
begin
  Result := False;
  Scale:=ACanvas.Scale;
  if Scale<1 then
     Scale:=1;
  if APoint.X < ACanvas.Offset.X then
    Result := true
  Else if APoint.y < ACanvas.Offset.y then
    Result := true
  Else if APoint.X > (ACanvas.Offset.X + ACanvas.width / Scale) then
    Result := true
  Else if APoint.y > (ACanvas.Offset.y + ACanvas.height / Scale) then
    Result := true;
end;

class procedure TIsGraphics.RectangleToPath(APath: TPathData;
  const ARect: TRectf);
Var
  Nxt: TPointf;
begin
  Nxt := ARect.TopLeft;
  APath.MoveTo(Nxt);
  Nxt.Offset(0.0, ARect.height);
  APath.LineTo(Nxt);
  Nxt.Offset(ARect.width, 0.0);
  APath.LineTo(Nxt);
  Nxt.Offset(0.0, -ARect.height);
  APath.LineTo(Nxt);
  Nxt.Offset(-ARect.width, 0.0);
  APath.LineTo(Nxt);
end;

class function TIsGraphics.RectOutsideBounds(ACanvas: TCanvas;
  ARect: TRectf): Boolean;
Var
  Scale:single;
begin
  Result := False;
  Scale:=ACanvas.Scale;
  if Scale<1 then
     Scale:=1;
  if ARect.Left < ACanvas.Offset.X then
    Result := true
  Else if ARect.top < ACanvas.Offset.y then
    Result := true
  Else if ARect.Left > (ACanvas.Offset.X + ACanvas.width / Scale) then
    Result := true
  Else if ARect.Bottom > (ACanvas.Offset.y + ACanvas.height / Scale) then
    Result := true;
end;

class function TIsGraphics.SetNewImageBitMap(AImage: TImage;
  ABackGround: TAlphaColor): Boolean;

Var
  BMap: TBitMap;
  X, y: Integer;
begin
  Result := False;
  BMap := AImage.Bitmap;
  X := Round(AImage.width);
  y := Round(AImage.height);
  if Not((BMap.width = X) and (BMap.height = y)) then
  begin
    Result := true;
    BMap.width := X;
    BMap.height := y;
    BMap.Clear(ABackGround);
  end;
end;

class Function TIsGraphics.SizeCanvasFont(ACanvas: TCanvas;
  ALengthLimit, AHeightLimit: Single; ATxt: String): TRectf;
// Adjusts Font size of given text to fit given limits
{ Sub } procedure Resize(Var AHght, ALenth: Single);
  Begin
    AHght := ACanvas.TextHeight(ATxt);
    ALenth := ACanvas.TextWidth(ATxt);
  End;

Var
  Hght, Lenth: Single;

begin
  Resize(Hght, Lenth);
  while (Hght < AHeightLimit * 0.99) and (Lenth < ALengthLimit * 0.99) do
  Begin
    ACanvas.Font.Size := ACanvas.Font.Size * 1.1;
    Resize(Hght, Lenth);
  End;
  while (Hght > AHeightLimit * 0.999) or (Lenth > ALengthLimit * 0.999) do
  Begin
    ACanvas.Font.Size := ACanvas.Font.Size * 0.95;
    Resize(Hght, Lenth);
  End;
  Result.TopLeft := TPointf.Create(0.0, 0.0);
  Result.height := ACanvas.TextHeight(ATxt);
  Result.width := ACanvas.TextWidth(ATxt);
end;

end.
