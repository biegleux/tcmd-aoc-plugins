(*
 * $Id$
 * This file is part of the A2LView project.
 *
 * Copyright (c) 2009-2010 biegleux <biegleux[at]gmail[dot]com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses>.
 *)
unit TeamsVCL;

interface

uses
  Windows, Controls, Classes, Messages, ExtCtrls, StdCtrls, Contnrs, Graphics,
  RecAnalyst, PaintBoxEx, uHintForm;

type
  TTeamsVCL = class;

  TPlayerVCL = class
  private
    FHintText: String;
    FOwner: TTeamsVCL;
    CivPaintBox: TPaintBoxEx;
    AgesPaintBox: TPaintBox;
    NameLabel: TLabel;
    CivLabel: TLabel;
    FeudalTimeLabel: TLabel;
    CastleTimeLabel: TLabel;
    ImperialTimeLabel: TLabel;
    procedure Initialize;
  public
    constructor Create(const AOwner: TTeamsVCL);
    destructor Destroy; override;
    procedure SetProperties(const Player: TPlayer; const ALeft: Integer;
        const ATop: Integer; const ATag: Integer);
    procedure CivPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CivPaintBoxMouseLeave(Sender: TObject);
  end;

  TTeamsVCL = class
  private
    FOwner: TComponent;
    FParent: TWinControl;
    FList: TList;
    FBitmaps: TObjectList;
    FAgesBitmap: TBitmap;
    FHintWin: THintForm;
    procedure AgesPaintBoxPaint(Sender: TObject);
    procedure CivPaintBoxPaint(Sender: TObject);
  public
    constructor Create(const AOwner: TComponent; const AParent: TWinControl);
    destructor Destroy; override;
    procedure Build(const RecAnalyst: TRecAnalyst; const Left: Integer; const Top: Integer);
    procedure Clear;
    property HintWin: THintForm read FHintWin write FHintWin;
  end;

implementation

uses
  Forms, SysUtils, Context, RecAnalystConsts, uLogger;

{ TPlayerVCL }
constructor TPlayerVCL.Create(const AOwner: TTeamsVCL);
begin
  FOwner := AOwner;

  if not Assigned (FOwner) or not Assigned (FOwner.FOwner) then
    raise Exception.Create ('Owner has not beed defined.');

  CivPaintBox       := TPaintBoxEx.Create (FOwner.FOwner);
  AgesPaintBox      := TPaintBox.Create (FOwner.FOwner);
  NameLabel         := TLabel.Create (FOwner.FOwner);
  CivLabel          := TLabel.Create (FOwner.FOwner);
  FeudalTimeLabel   := TLabel.Create (FOwner.FOwner);
  CastleTimeLabel   := TLabel.Create (FOwner.FOwner);
  ImperialTimeLabel := TLabel.Create (FOwner.FOwner);

  FHintText := '';

  Initialize;
end;

destructor TPlayerVCL.Destroy;
begin
  CivPaintBox.OnPaint := nil;
  AgesPaintBox.OnPaint := nil;
end;

procedure TPlayerVCL.Initialize;
begin
  with CivPaintBox do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 36;
    Height := 36;
    OnPaint := nil;
    OnMouseMove := nil;
    OnMouseLeave := nil;
    Visible := False;
  end;

  with AgesPaintBox do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 12;
    Height := 36;
    OnPaint := nil;
    Visible := False;
  end;

  with NameLabel do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 103;
    Height := 13;
    Caption := '';
    ParentFont := False;
    Font.Name := 'Georgia';
    Font.Style := [fsBold];
    Transparent := True;
    ShowAccelChar := False;
    Visible := False;
  end;

  with CivLabel do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 37;
    Height := 13;
    Caption := '';
    ParentFont := False;
    Font.Name := 'Verdana';
    Transparent := True;
    Visible := False;
  end;

  with FeudalTimeLabel do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 50;
    Height := 12;
    Caption := '';
    ParentFont := False;
    Font.Name := 'Verdana';
    Font.Size := 7;
    Transparent := True;
    Visible := False;
  end;

  with CastleTimeLabel do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 50;
    Height := 12;
    Caption := '';
    ParentFont := False;
    Font.Name := 'Verdana';
    Font.Size := 7;
    Transparent := True;
    Visible := False;
  end;

  with ImperialTimeLabel do
  begin
    Parent := FOwner.FParent;
    Left := 0;
    Top := 0;
    Width := 50;
    Height := 12;
    Caption := '';
    ParentFont := False;
    Font.Name := 'Verdana';
    Font.Size := 7;
    Transparent := True;
    Visible := False;
  end;
end;

procedure TPlayerVCL.SetProperties(const Player: TPlayer; const ALeft: Integer;
    const ATop: Integer; const ATag: Integer);
var
  SL: TStringList;
begin
  if not Assigned (Player) then
    Exit;

  if not Player.IsCooping then
  begin
    SL := TStringList.Create;
    try
      with Player.InitialState do
      begin
        SL.Add ('Initial resources:');
        SL.Add (Format ('F: %d, W: %d, S: %d, G: %d', [Food, Wood, Stone, Gold]));
        SL.Add ('Starting Age: ' + sStartingAge);
        if (ExtraPop <> 0) then
          SL.Add (Format ('Population: %d (%d/%d/%d)', [Population, CivilianPop, MilitaryPop, ExtraPop]))
        else
          SL.Add (Format ('Population: %d (%d/%d)', [Population, CivilianPop, MilitaryPop]));
        if (Player.ResignTime <> 0) then
        begin
          SL.Add ('');
          SL.Add (Format ('Resigned in %s', [TRecAnalyst.GameTimeToString (Player.ResignTime)]));
        end;
      end;
      FHintText := SL.Text;
    finally
      SL.Free;
    end;
  end;
  
  with CivPaintBox do
  begin
    Left := ALeft;
    Top := ATop + 24;
    Tag := ATag;
    OnPaint := FOwner.CivPaintBoxPaint;
    OnMouseMove := CivPaintBoxMouseMove;
    OnMouseLeave := CivPaintBoxMouseLeave;
    Visible := True;
  end;

  with AgesPaintBox do
  begin
    Left := ALeft + 40;
    Top := ATop + 28;
    OnPaint := FOwner.AgesPaintBoxPaint;
    Visible := True;
  end;

  with NameLabel do
  begin
    Left := ALeft + 32;
    Top := ATop;
    Caption := Player.Name;
    if not Player.Human then
      Caption := Caption + ' /PC/';
    if (Player.ResignTime <> 0) then
      Font.Style := Font.Style + [fsStrikeOut];
    if (Player.ResignTime <> 0) then
    begin
      ShowHint := True;
      Hint := Format ('Resigned in %s', [TRecAnalyst.GameTimeToString (Player.ResignTime)]);
    end;
    Visible := True;
  end;

  with CivLabel do
  begin
    Left := ALeft + 40;
    Top := ATop + 12;
    Caption := Player.Civ;
    ParentFont := False;
    if Player.ColorId in [Low (COLORS)..High (COLORS)] then
      Font.Color := COLORS[Player.ColorId];
    Visible := True;
  end;

  with FeudalTimeLabel do
  begin
    Left := ALeft + 53;
    Top := ATop + 28;
    Caption := TRecAnalyst.GameTimeToString (Player.FeudalTime);
    if (Caption = '') then
      Caption := '-';
    Visible := True;
  end;

  with CastleTimeLabel do
  begin
    Left := ALeft + 53;
    Top := ATop + 40;
    Caption := TRecAnalyst.GameTimeToString (Player.CastleTime);
    if (Caption = '') then
      Caption := '-';
    Visible := True;
  end;

  with ImperialTimeLabel do
  begin
    Left := ALeft + 53;
    Top := ATop + 52;
    Caption := TRecAnalyst.GameTimeToString (Player.ImperialTime);
    if (Caption = '') then
      Caption := '-';
    Visible := True;
  end;
end;

procedure TPlayerVCL.CivPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  HintForm: THintForm;
begin
  if not (Sender is TPaintBoxEx) or not Assigned (FOwner.FHintWin) then
    Exit;

  HintForm := FOwner.FHintWin;
  
  GetCursorPos (p);
  if (p.X + 8 + HintForm.Width > Screen.Width) then
    HintForm.Left := p.X + 8 - HintForm.Width
  else
    HintForm.Left := p.X + 8;

  if (p.Y + 8 + HintForm.Height > Screen.Height) then
    HintForm.Top := p.Y + 8 - HintForm.Height
  else
    HintForm.Top := p.Y + 8;

  if not HintForm.IsVisible then
  begin
    HintForm.Text := FHintText;
    HintForm.IsVisible := True;
  end;
end;

procedure TPlayerVCL.CivPaintBoxMouseLeave(Sender: TObject);
begin
  if not Assigned (FOwner.FHintWin) then
    Exit;
  
  if FOwner.FHintWin.IsVisible then
    FOwner.FHintWin.IsVisible := False;
end;

{ TTeamsVCL }
constructor TTeamsVCL.Create(const AOwner: TComponent; const AParent: TWinControl);
var
  PlayerVCL: TPlayerVCL;
  i: Integer;
const
  MaxTeamCount = 8;
begin
  FOwner := AOwner;
  FParent := AParent;
  FList := TList.Create;
  FBitmaps := TObjectList.Create;
  FAgesBitmap := TBitmap.Create;
  FHintWin := nil;

  try
    TResourceDll.LoadBitmap (AGES_RESNAME, FAgesBitmap);
  except
    on E: Exception do
      Logger.SendException ('Resource Name: %s', [AGES_RESNAME], E);
  end;

  for i := 0 to MaxTeamCount - 1 do
  begin
    PlayerVCL := TPlayerVCL.Create (Self);
    FList.Add (PlayerVCL);
  end;
end;

destructor TTeamsVCL.Destroy;
var
  i: Integer;
  PlayerVCL: TPlayerVCL;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    PlayerVCL := FList[i];
    FList.Delete (i);
    PlayerVCL.Free;
  end;
  FList.Free;
  FBitmaps.Free;
  FAgesBitmap.Free;
  FOwner := nil;
  FParent := nil;
end;

procedure TTeamsVCL.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TPlayerVCL(FList[i]).Initialize;
  FBitmaps.Clear;
end;

procedure TTeamsVCL.AgesPaintBoxPaint(Sender: TObject);
begin
  if not (Sender is TPaintBox) then
    Exit;

  if not Assigned (FAgesBitmap) then
    Exit;

  (Sender as TPaintBox).Canvas.Draw (0, 0, FAgesBitmap);
end;

procedure TTeamsVCL.CivPaintBoxPaint(Sender: TObject);
var
  PB: TPaintBoxEx;
begin
  if not (Sender is TPaintBoxEx) then
    Exit;

  PB := Sender as TPaintBoxEx;
  if not PB.Tag in [0..FBitmaps.Count - 1] then
    Exit;

  if not Assigned (FBitmaps[PB.Tag]) then
    Exit;

  PB.Canvas.Draw (0, 0, TBitmap (FBitmaps[PB.Tag]));
end;

procedure TTeamsVCL.Build(const RecAnalyst: TRecAnalyst; const Left: Integer; const Top: Integer);
var
  Team: TTeam;
  Player: TPlayer;
  PlayerVCL: TPlayerVCL;
  Bitmap: TBitmap;
  sResName: String;
  i, j, idx, Tag: Integer;
  max_width: array[0..8] of Integer;
  width, cleft: Integer;
begin
  if not Assigned (RecAnalyst) then
    Exit;

  Clear;

  { calculate max text-width of an player in a team }
  FillChar (max_width, SizeOf(max_width), 0);
  PlayerVCL := FList[0];
  for i := 0 to RecAnalyst.Teams.Count - 1 do
  begin
    Team := RecAnalyst.Teams[i];
    for j := 0 to Team.Count - 1 do
    begin
      Player := Team[j];
      if not Player.Human then
        width := PlayerVCL.NameLabel.Canvas.TextWidth(Format ('%s /PC/', [Player.Name]))
      else
        width := PlayerVCL.NameLabel.Canvas.TextWidth(Player.Name);
      if (max_width[i + 1] < width) then
        max_width[i + 1] := width;
    end;
    if (max_width[i + 1] < 90) then
      max_width[i + 1] := 90;
// Max (FeudalTimeLabel.Canvas.TextWidth(FeudalTimeLabel.Caption))), max_width);      
  end;

  idx := 0;
  cleft := Left;
  for i := 0 to RecAnalyst.Teams.Count - 1 do
  begin
    Team := RecAnalyst.Teams[i];
    cleft := cleft + max_width[i] + 20;
    for j := 0 to Team.Count - 1 do
    begin
      Player := Team[j];
      PlayerVCL := FList[idx];

      if not (Player.CivId in [Low (CIVS)..High (CIVS)]) then
        Continue;

      Tag := idx;
      Bitmap := TBitmap.Create;
      { load civ image from resources }
      try
        sResName := Format ('%d_%s', [Player.ColorId, CIVS[Player.CivId].ResName]);
        TResourceDll.LoadBitmap (sResName, Bitmap);
        FBitmaps.Add (Bitmap);
      except
        Tag := -1;
      end;
      PlayerVCL.SetProperties (Player, cleft, Top + 72 * j, Tag);
      Inc (idx);
    end;
  end;
end;

end.
