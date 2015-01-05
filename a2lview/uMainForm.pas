{*
 * This file is part of the A2LView project.
 *
 * Copyright (c) 2009-2013 biegleux <biegleux[at]gmail[dot]com>
 *
 * This plugin is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This plugin is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this plugin; if not, see <http://www.gnu.org/licenses>.
 *}
unit uMainForm;

interface

uses
  Winapi.Windows, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Controls, System.Classes,
  Vcl.Forms, System.SysUtils, Winapi.Messages, System.Contnrs,
  RecAnalystWrap;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    PageControl: TPageControl;
    GeneralTabSheet: TTabSheet;
    GameSettingsGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    GameTypeLabel: TLabel;
    MapLabel: TLabel;
    PlayersLabel: TLabel;
    DurationLabel: TLabel;
    POVLabel: TLabel;
    VersionLabel: TLabel;
    DetailsLabel: TLabel;
    DetailsPanel: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    MapStyleLabel: TLabel;
    MapSizeLabel: TLabel;
    RevealMapLabel: TLabel;
    VictoryLabel: TLabel;
    SpeedLabel: TLabel;
    PopulationLabel: TLabel;
    DifficultyLabel: TLabel;
    TeamsGroupBox: TGroupBox;
    Panel1: TPanel;
    Image1: TImage;
    Label14: TLabel;
    Label15: TLabel;
    Panel2: TPanel;
    Image2: TImage;
    Label16: TLabel;
    Label17: TLabel;
    Panel3: TPanel;
    Image3: TImage;
    Label18: TLabel;
    Label19: TLabel;
    Panel4: TPanel;
    Image4: TImage;
    Label20: TLabel;
    Label21: TLabel;
    Panel5: TPanel;
    Image5: TImage;
    Label22: TLabel;
    Label23: TLabel;
    Panel6: TPanel;
    Image6: TImage;
    Label24: TLabel;
    Label25: TLabel;
    Panel7: TPanel;
    Image7: TImage;
    Label26: TLabel;
    Label27: TLabel;
    Panel8: TPanel;
    Image8: TImage;
    Label28: TLabel;
    Label29: TLabel;
    Panel9: TPanel;
    Image9: TImage;
    Label30: TLabel;
    Label31: TLabel;
    Panel10: TPanel;
    Image10: TImage;
    Label32: TLabel;
    Label33: TLabel;
    Panel11: TPanel;
    Image11: TImage;
    Label34: TLabel;
    Label35: TLabel;
    Panel12: TPanel;
    Image12: TImage;
    Label36: TLabel;
    Label37: TLabel;
    Panel13: TPanel;
    Image13: TImage;
    Label38: TLabel;
    Label39: TLabel;
    Panel14: TPanel;
    Image14: TImage;
    Label40: TLabel;
    Label41: TLabel;
    Panel15: TPanel;
    Image15: TImage;
    Label42: TLabel;
    Label43: TLabel;
    Panel16: TPanel;
    Image16: TImage;
    Label44: TLabel;
    Label45: TLabel;
    ChatTabSheet: TTabSheet;
    ChatRichEdit: TRichEdit;
    ResearchesTabSheet: TTabSheet;
    MapImage: TImage;
    ResearchesGroupBox: TGroupBox;
    ScrollBox: TScrollBox;
    ResearchesImage: TImage;
    PlayersPanel: TPanel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    ViewResultsLabel: TLabel;
    procedure DetailsLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure ResearchesImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ResearchesTabSheetResize(Sender: TObject);
    procedure ViewResultsLabelClick(Sender: TObject);
  private
    TotCmdWin: HWND;     { handle of TC window }
    ParentWin: HWND;     { handle of Lister window }
    QuickView: Boolean;  { Ctrl+Q panel }
    FileName: String;
    RecAnalyst: TRecAnalyst;
    ImageMap: TObjectList;
    procedure LoadFile();
    procedure AppException(Sender: TObject; E: Exception);

    procedure CollapseDetails();
    procedure ExpandDetails();
    procedure ToggleDetails();
    procedure FillGameSettings();
    procedure FillPlayers();
    procedure FillChat();
    procedure FillResearches();
    procedure FillPanel(Panel: TPanel);
    function GetSortedMessages(): TObjectList;
    class function HintString(Player: TPlayer): String;
    procedure GenerateResearches();
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor CreateParented(ParentWindow: HWND; const FileToView: String); reintroduce;
  end;

function ShowAoE2(ListerWin: HWND; const FileToLoad: String; ShowFlags: Integer): HWND;
procedure HideAoE2(PluginWin: HWND);

implementation

{$R *.dfm}

uses
  Vcl.Graphics, Vcl.Imaging.pngimage, Context, Types, recanalystd,
  GDIPAPI, GDIPOBJ, GDIPUTIL, ActiveX, Math;

type
  TPlugInfo = record
    PlugWinProc: Pointer; { callback function of our form }
    PlugForm: TMainForm;  { our form }
  end;

  TImageMapItem = class(TObject)
    Coordinates: TRect;
    Hint: String;
  end;

const
  sExceptionMsg = 'Plugin Error:'#13'%s';
  sExceptionDestroyMsg = 'DestroyWindow Error:'#13'%s';
  sCaption = 'AOC Recorded Games Viewer';

procedure wMsgBox(hWnd: HWND; Msg: String);
begin
  MessageBox(hWnd, PChar(Msg), sCaption, MB_OK + MB_ICONINFORMATION);
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
  wMsgBox(Handle, Format(sExceptionMsg, [E.Message]));
end;

procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (WS_CHILD or WS_MAXIMIZE) and not WS_CAPTION and not WS_BORDER;
  Params.WindowClass.cbWndExtra := SizeOf(Pointer); // 4 bytes for address of form
end;

constructor TMainForm.CreateParented(ParentWindow: HWND; const FileToView: String);
const
  WinCmdClassName = 'TTOTAL_CMD';
begin
  inherited CreateParented(ParentWindow);
  TotCmdWin := FindWindow(WinCmdClassName, nil);
  ParentWin := ParentWindow;
  QuickView := GetParent(ParentWin) <> 0;
  FileName := FileToView;
end;

function HookDestroy(PluginWin: HWND; Msg, wParam, lParam: LongInt): LongInt; stdcall;
var
  p: ^TPlugInfo;
begin
  { hook destroy our window }
  p := Pointer(GetWindowLong(PluginWin, GWL_USERDATA));
  if (Msg <> WM_DESTROY) then
    Result := CallWindowProc(p^.PlugWinProc, PluginWin, Msg, wParam, lParam)
  else begin
    { plugin close }
    HideAoE2(PluginWin);
    Result := 0;
  end;
end;

procedure HideAoE2(PluginWin: HWND);
var
  p: ^TPlugInfo;
begin
  p := Pointer(GetWindowLong(PluginWin, GWL_USERDATA));
  with p^.PlugForm do
    try
      Application.ShowHint := False;
      Application.HintHidePause := 2500;
      Application.RemoveComponent(p^.PlugForm);
      Application.Handle := 0;
      { restore callback function }
      SetWindowLong(Handle, GWL_WNDPROC, Integer(p^.PlugWinProc));
      Free();
    except
      on E: Exception do
        wMsgBox(Handle, Format(sExceptionDestroyMsg, [E.Message]));
    end;
  Dispose(p);
end;

function ShowAoE2(ListerWin: HWND; const FileToLoad: String; ShowFlags: Integer): HWND;
var
  MainForm: TMainForm;
  s: String;
  p: ^TPlugInfo;
begin
  try
    s := ExtractFilePath(FileToLoad);
    if not SetCurrentDir(s) then
      raise Exception.Create(Format('Error of SetCurrentDir() for Folder: %s', [s]));

    MainForm := TMainForm.CreateParented(ListerWin, FileToLoad);
    MainForm.Show;
    { synchronize our form and Lister }
    Application.Handle := ListerWin;
    Application.ShowHint := True;
    Application.HintHidePause := 5000;

    Application.OnException := MainForm.AppException;
    Application.InsertComponent(MainForm);

    { substitution callback function }
    New(p);
    SetWindowLong(MainForm.Handle, GWL_USERDATA, Integer(p));
    p^.PlugForm := MainForm;
    p^.PlugWinProc := Pointer(SetWindowLong(MainForm.Handle, GWL_WNDPROC, Integer(@HookDestroy)));

    { set focus to our window }
    if not MainForm.QuickView then
      PostMessage(MainForm.Handle, WM_SETFOCUS, 0, 0);
    Result := MainForm.Handle;
  except
    on E: Exception do
    begin
      wMsgBox(ListerWin, Format(sExceptionMsg, [E.Message]));
      Result := 0;
    end;
  end;
end;

procedure TMainForm.ViewResultsLabelClick(Sender: TObject);
var
  Panel: TPanel;
  P: TPlayer;
  L: TLabel;
  i: Integer;
begin
  for i := 0 to 15 do
  begin
    Panel := FindComponent('Panel' + IntToStr(i + 1)) as TPanel;
    if (Panel.Tag = 0) then Continue;
    P := TPlayer(Panel.Tag);
    if (P.ResignTime > 0) then
    begin
      L := TLabel(Panel.Controls[1]);
      L.Font.Style := [fsBold, fsStrikeOut];
      L.Hint := Format('Resigned in %s', [TRecAnalyst.GameTimeToString(P.ResignTime)]);
      L.ShowHint := True;
    end;
  end;
end;

procedure TMainForm.LoadFile();
begin
  RecAnalyst.Analyze(FileName);

  FillGameSettings();
  FillPlayers();

  try
    FillChat();
  except
    on E: Exception do
    begin
      ChatTabSheet.TabVisible := False;
      AppException(Self, E);
    end;
  end;

  try
    FillResearches();
  except
    on E: Exception do
    begin
      ResearchesTabSheet.TabVisible := False;
      AppException(Self, E);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CollapseDetails();
  TeamsGroupBox.Top := GameSettingsGroupBox.Top + GameSettingsGroupBox.Height
    + TeamsGroupBox.Left;
  ChatRichEdit.Width := ChatTabSheet.ClientWidth - 2 * ChatRichEdit.Left;
  ChatRichEdit.Height := ChatTabSheet.ClientHeight - 2 * ChatRichEdit.Top;
  ChatRichEdit.Anchors := [akTop, akLeft, akRight, akBottom];

  ImageMap := TObjectList.Create();

  try
    RecAnalyst := TRecAnalyst.Create();
    LoadFile();
  except
    PageControl.Visible := False;
    raise;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(RecAnalyst) then
    RecAnalyst.Free();
end;

function ListCompare(Item1: Pointer; Item2: Pointer): Integer;
var
  Time1, Time2: Integer;
begin
  Time1 := 0; Time2 := 0;
  if (TObject(Item1) is TChatMessage) then
    Time1 := TChatMessage(Item1).Time
  else if (TObject(Item1) is TTribute) then
    Time1 := TTribute(Item1).Time;

  if (TObject(Item2) is TChatMessage) then
    Time2 := TChatMessage(Item2).Time
  else if (TObject(Item2) is TTribute) then
    Time2 := TTribute(Item2).Time;

  if (Time1 < Time2) then Result := -1
  else if (Time1 > Time2) then Result := 1
  else Result := 0;
end;

function TMainForm.GetSortedMessages(): TObjectList;
var
  i: Integer;
begin
  Result := TObjectList.Create(False);
  for i := 0 to RecAnalyst.InGameChatMessages.Count - 1 do
    Result.Add(RecAnalyst.InGameChatMessages[i]);
  for i := 0 to RecAnalyst.Tributes.Count - 1 do
    Result.Add(RecAnalyst.Tributes[i]);
  Result.Sort(@ListCompare);
end;

procedure TMainForm.FillChat();
var
  Messages: TObjectList;
  M: TChatMessage;
  T: TTribute;
  i: Integer;
begin
  if (RecAnalyst.PreGameChatMessages.Count = 0)
    and (RecAnalyst.InGameChatMessages.Count = 0)
    and (RecAnalyst.Tributes.Count = 0) then
  begin
    ChatTabSheet.TabVisible := False;
    Exit;
  end;

  ChatRichEdit.SelStart := 0;
  if (RecAnalyst.PreGameChatMessages.Count > 0) then
  begin
    ChatRichEdit.SelAttributes.Style := [fsBold];
    ChatRichEdit.SelText := 'pre-game chat:' + sLineBreak;
    for i := 0 to RecAnalyst.PreGameChatMessages.Count - 1 do
    begin
      M := RecAnalyst.PreGameChatMessages[i] as TChatMessage;
      if Assigned(M.Player) then
        ChatRichEdit.SelAttributes.Color := M.Player.Color
      else
        ChatRichEdit.SelAttributes.Color := clBlack;
      ChatRichEdit.SelText := M.Msg + sLineBreak;
    end;
  end;

  Messages := GetSortedMessages();
  try
    if (Messages.Count = 0) then Exit;
    ChatRichEdit.SelAttributes.Style := [fsBold];
    ChatRichEdit.SelText := 'in-game chat:' + sLineBreak;
    for i := 0 to Messages.Count - 1 do
    begin
      if (Messages[i] is TChatMessage) then
      begin
        M := Messages[i] as TChatMessage;
        ChatRichEdit.SelText := Format('(%s) ', [TRecAnalyst.GameTimeToString(M.Time)]);
        if Assigned(M.Player) then
          ChatRichEdit.SelAttributes.Color := M.Player.Color
        else begin
          ChatRichEdit.SelAttributes.Color := clBlack;
          ChatRichEdit.SelAttributes.Style := [fsItalic];
        end;
        ChatRichEdit.SelText := M.Msg + sLineBreak;
      end else if (Messages[i] is TTribute) then
      begin
        T := TTribute(Messages[i]);
        ChatRichEdit.SelText := Format('(%s) ', [TRecAnalyst.GameTimeToString(T.Time)]);
        ChatRichEdit.SelAttributes.Style := [fsItalic];
        ChatRichEdit.SelAttributes.Color := T.PlayerFrom.Color;
        ChatRichEdit.SelText := T.PlayerFrom.Name;
        ChatRichEdit.SelAttributes.Color := clBlack;
        ChatRichEdit.SelText := Format(' has sent %d (-%d) %s to ' ,
            [T.Amount, Round(T.Amount + (T.Amount * T.Fee)), T.Resource]);
        ChatRichEdit.SelAttributes.Color := T.PlayerTo.Color;
        ChatRichEdit.SelText := T.PlayerTo.Name + sLineBreak;
      end;
    end;
  finally
    Messages.Free();
  end;
end;

procedure TMainForm.FillGameSettings();
var
  Stream: TMemoryStream;
  Png: TPngImage;
begin
  with RecAnalyst.GameSettings do
  begin
    GameTypeLabel.Caption := GameType;
    if RecAnalyst.GameSettings.IsScenario then
      MapLabel.Caption := Trim(ChangeFileExt(RecAnalyst.GameSettings.ScFileName, ''))
    else
      MapLabel.Caption := Trim(Map);
    PlayersLabel.Caption := PlayersType;
    DurationLabel.Caption := RecAnalyst.GameTimeToString(PlayTime);
    POVLabel.Caption := POV;
    VersionLabel.Caption := Version;
    MapStyleLabel.Caption := MapStyle;
    MapSizeLabel.Caption := MapSize;
    RevealMapLabel.Caption := RevealMap;
    VictoryLabel.Caption := Victory;
    SpeedLabel.Caption := GameSpeed;
    PopulationLabel.Caption := IntToStr(PopLimit);
    DifficultyLabel.Caption := DifficultyLevel;
  end;

  if RecAnalyst.GameSettings.IsScenario then
  begin
    Label13.Top := Label10.Top;
    Label11.Top := Label9.Top;
    Label10.Top := Label8.Top;
    Label9.Top := Label7.Top;
    DifficultyLabel.Top := Label13.Top;
    SpeedLabel.Top := Label11.Top;
    VictoryLabel.Top := Label10.Top;
    RevealMapLabel.Top := Label9.Top;
    Label7.Visible := False;
    Label8.Visible := False;
    Label12.Visible := False;
    MapStyleLabel.Visible := False;
    MapSizeLabel.Visible := False;
    PopulationLabel.Visible := False;
    ViewResultsLabel.Top := DetailsPanel.Top + DifficultyLabel.Top;
    DetailsPanel.Height := DifficultyLabel.Top + DifficultyLabel.Height;
  end;

  Stream := RecAnalyst.GenerateMap(246, 123);
  if Assigned(Stream) then
  try
    Png := TPngImage.Create();
    try
      Png.LoadFromStream(Stream);
      MapImage.Picture.Assign(Png);
    finally
      Png.Free();
    end;
  finally
    Stream.Free();
  end;
end;

class function TMainForm.HintString(Player: TPlayer): String;
begin
  Result := '';
  if Player.IsCooping then Exit;
  if (Player.FeudalTime > 0) then
    Result := Format('Feudal: %s', [TRecAnalyst.GameTimeToString(Player.FeudalTime)]);
  if (Player.CastleTime > 0) then
    Result := Result + Format('%sCastle: %s', [sLineBreak, TRecAnalyst.GameTimeToString(
        Player.CastleTime)]);
  if (Player.ImperialTime > 0) then
    Result := Result + Format('%sImperial: %s', [sLineBreak, TRecAnalyst.GameTimeToString(
        Player.ImperialTime)]);
end;

procedure TMainForm.FillPanel(Panel: TPanel);
var
  Player: TPlayer;
  Image: TImage;
  PngImage: TPngImage;
  ResName: String;
begin
  if (Panel.Tag = 0) then
    Panel.Visible := False
  else begin
    Player := TPlayer(Panel.Tag);
    TLabel(Panel.Controls[1]).Caption := Player.Name;
    TLabel(Panel.Controls[2]).Font.Color := Player.Color;
    TLabel(Panel.Controls[2]).Caption := Player.Civ;

    PngImage := TPngImage.Create();
    try
      try
        ResName := Format('%d_%s', [Player.ColorId, Player.Civ]);
        PngImage.LoadFromResourceName(DllHandle, ResName);
        Image := Panel.Controls[0] as TImage;
        Image.Picture.Assign(PngImage);
        Image.Hint := HintString(Player);
      except
        //
      end;
    finally
      PngImage.Free();
    end;
  end;
end;

procedure TMainForm.FillPlayers();
var
  i, split_idx, idx, max_height: Integer;
  Panels: array[0..15] of TPanel;
  PP, NP: TPlayer;
  Panel: TPanel;
begin
  for i := Low(Panels) to High(Panels) do
    Panels[i] := FindComponent('Panel' + IntToStr(i + 1)) as TPanel;

  PP := nil; split_idx := -1;
  if RecAnalyst.GameSettings.IsFFA then
  begin
    split_idx := (RecAnalyst.Players.Count div 2) + (RecAnalyst.Players.Count mod 2);
  end else
  begin
    for i := 0 to RecAnalyst.Players.Count - 1 do
    begin
      NP := RecAnalyst.Players[i] as TPlayer;
      if not Assigned(PP) then
      begin
        PP := NP; Continue;
      end;
      if (NP.Team <> PP.Team) or ((NP.Team = 0) and (NP.Team = PP.Team) and (NP.Index <> PP.Index)) then
      begin
        split_idx := i; Break;
      end;
    end;
  end;

  for i := 0 to RecAnalyst.Players.Count - 1 do
  begin
    if (i < split_idx) then idx := i else idx := 8 + (i - split_idx);
    Panels[idx].Tag := NativeInt(RecAnalyst.Players[i]);
  end;

  max_height := 0;
  for i := Low(Panels) to High(Panels) do
  begin
    Panel := Panels[i];
    FillPanel(Panel);
    if Panel.Visible and (Panel.Top + Panel.Height > max_height) then
      max_height := Panel.Top + Panel.Height;
  end;
  TeamsGroupBox.Height := max_height + Panel1.Top;
end;

procedure TMainForm.FillResearches();
var
  i, idx: Integer;
  P: TPlayer;
  L: TLabel;
begin
  if (RecAnalyst.Researches.Count = 0) then
  begin
    ResearchesTabSheet.TabVisible := False;
    Exit;
  end;

  idx := 0;
  for i := 0 to RecAnalyst.Players.Count - 1 do
  begin
    P := RecAnalyst.Players[i] as TPlayer;
    if P.IsCooping then Continue;
    L := TLabel(PlayersPanel.Controls[idx]);
    L.Caption := P.Name;
    L.Font.Color := P.Color;
    L.Hint := P.Civ;
    Inc(idx);
  end;
  L := TLabel(PlayersPanel.Controls[idx]);
  L.Caption := 'minute';
  L.Font.Style := [];
  L.Top := L.Top - 6;
  L.Hint := 'Minute, in which player began to research the technology.';

  for i := idx + 1 to PlayersPanel.ControlCount - 1 do
  begin
    if (PlayersPanel.Controls[i] is TLabel) then
      PlayersPanel.Controls[i].Visible := False;
  end;

  GenerateResearches();

  ScrollBox.Width := ResearchesGroupBox.Width - ScrollBox.Left - PlayersPanel.Left;
  ScrollBox.Anchors := [akLeft, akTop, akRight];
end;

procedure TMainForm.CollapseDetails();
begin
  GameSettingsGroupBox.Height := MapImage.Top + MapImage.Height + MapImage.Top;
  DetailsPanel.Visible := False;
  DetailsLabel.Caption := '↓ more...';
end;

procedure TMainForm.DetailsLabelClick(Sender: TObject);
begin
  ToggleDetails();
end;

procedure TMainForm.ExpandDetails();
begin
  GameSettingsGroupBox.Height := DetailsPanel.Top + DetailsPanel.Height + GameTypeLabel.Top;
  DetailsPanel.Visible := True;
  DetailsLabel.Caption := '↑ less...';
end;

procedure TMainForm.ToggleDetails();
begin
  if DetailsPanel.Visible then
    CollapseDetails()
  else
    ExpandDetails();

  TeamsGroupBox.Top := GameSettingsGroupBox.Top + GameSettingsGroupBox.Height
    + TeamsGroupBox.Left;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if (PageControl.ActivePage = ChatTabSheet) then
  begin
    ChatRichEdit.CaretPos := Point(0, 0);
    ChatRichEdit.SetFocus;
  end;
end;

procedure TMainForm.ResearchesImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i, idx: Integer;
  Item: TImageMapItem;
begin
  if not Assigned(ResearchesImage.Picture) then Exit;

  idx := -1;
  Item := nil;
  for i := 0 to ImageMap.Count - 1 do
  begin
    Item := ImageMap[i] as TImageMapItem;
    if (X >= Item.Coordinates.Left) and (X <= Item.Coordinates.Right) and
       (Y >= Item.Coordinates.Top) and (Y <= Item.Coordinates.Bottom) then
    begin
      idx := i;
      Break;
    end;
  end;

  if (idx <> -1) and Assigned(Item) then
  begin
    ResearchesImage.ShowHint := False;
    ResearchesImage.ShowHint := True;
    ResearchesImage.Hint := Item.Hint;
  end else
  begin
    ResearchesImage.Hint := '';
    ResearchesImage.ShowHint := False;
  end;
end;

procedure TMainForm.ResearchesTabSheetResize(Sender: TObject);
var
  Width: Integer;
begin
  Width := ResearchesGroupBox.Left + ScrollBox.Left + ResearchesImage.Width;
  if (Width + 32 > ResearchesTabSheet.ClientWidth) then
    ResearchesGroupBox.Width := ResearchesTabSheet.ClientWidth - 2 * ResearchesGroupBox.Left
  else
    ResearchesGroupBox.Width := Width;
end;

procedure TMainForm.ScrollBoxResize(Sender: TObject);
begin
  if ScrollBox.HorzScrollBar.IsScrollBarVisible then
    ScrollBox.Height := ResearchesImage.Height + 20
  else
    ScrollBox.Height := ResearchesImage.Height;
  ResearchesGroupBox.ClientHeight := ScrollBox.Height + 2 * ScrollBox.Top;
end;

procedure TMainForm.GenerateResearches();
type
  TImageData = record
    Points: TPointDynArray;
    Size: TSize;
  end;

function Calculate(RecAnalyst: TRecAnalyst): TImageData;
var
  R: TResearch;
  P: TPlayer;
  prev_minute, minute, max: Integer;
  res_cnt: array[1..8] of Integer; // player_id in <1, 8>
  line: array[1..8] of Integer; // player_id in <1, 8>
  dst_x, dst_y, delta, count, i, idx: Integer;
const
  RTW = 28; { resource tile width }
  RTH = 28; { resource tile height }
  HSPACING = 3; { horizontal spacing }
  VSPACING = 3; { vertical spacing }
begin
  Result.Points := nil;
  Result.Size.cx := 0;
  Result.Size.cy := 0;

  if (RecAnalyst.Researches.Count = 0) then Exit;

  SetLength(Result.Points, RecAnalyst.Researches.Count);
  FillChar(res_cnt, SizeOf(res_cnt), 0);
  count := 0;
  for i := 0 to RecAnalyst.Players.Count - 1 do
  begin
    if not TPlayer(RecAnalyst.Players[i]).IsCooping then
      Inc(count);
  end;

  Result.Size.cy := count * (RTH + VSPACING) + 20; { height for minutes }

  idx := 0;
  for i := 0 to RecAnalyst.Players.Count - 1 do
  begin
    P := RecAnalyst.Players[i] as TPlayer;
    if P.IsCooping then Continue;
    line[P.Index] := idx;
    Inc(idx);
  end;

  max := 0;  { max = res_cnt[i]; forall j<>i: res_cnt[j] <= res_cnt[i] }
  dst_x := 0;
  FillChar(res_cnt, SizeOf(res_cnt), 0);  { holds # of resources for each player in current minute }

  prev_minute := -1;
  for i := 0 to RecAnalyst.Researches.Count - 1 do
  begin
    R := RecAnalyst.Researches[i] as TResearch;
    { minute = current minute in timeline (minute-based sampling) }
    minute := Floor(R.Time / 1000 / 60);
    { new minute has just started, rember researches are time-sorted }
    if (prev_minute <> minute) then
    begin
      FillChar(res_cnt, SizeOf(res_cnt), 0); { zero }
      Inc(dst_x, max * (RTW + HSPACING));  { shift position on maximum }
      max := 0;  { zero }
    end;
    prev_minute := minute;

    delta := res_cnt[R.Player.Index] * (RTW + HSPACING);
    dst_y := line[R.Player.Index] * (RTH + VSPACING);

    { increase # of resources for its 'owner' }
    Inc(res_cnt[R.Player.Index]);
    { set maximum if we have new one }
    if (max < res_cnt[R.Player.Index]) then
      max := res_cnt[R.Player.Index];

    Result.Points[i].X := dst_x + delta;
    Result.Points[i].Y := dst_y;
  end;

  Inc(dst_x, max * (RTW + HSPACING));
  Result.Size.cx := dst_x;
end;

var
  R: TResearch;
  prev_minute, minute: Integer;
  i, y: Integer;

  Bitmap: TGPBitmap;
  Bmp: TGPBitmap;
  Graphics: TGPGraphics;
  encoderClsid: TGUID;

  Item: TImageMapItem;

  ImageData: TImageData;

  Res : TResourceStream;
  StreamInf: IStream;
  MemoryStream: TMemoryStream;

  FontFamily: TGPFontFamily;
  Font: TGPFont;
  Brush: TGPBrush;
  Pen: TGPPen;
  Origin: TGPPointF;

  Png: TPngImage;
const
  RTW = 28; { resource tile width }
  RTH = 28; { resource tile height }
  HSPACING = 3; { horizontal spacing }
  VSPACING = 3; { vertical spacing }
  c_min = '%d.';
begin
  ImageData := Calculate(RecAnalyst);
  if not Assigned(ImageData.Points) then Exit;

  InitializeGdiplus();

  Bitmap := TGPBitmap.Create(ImageData.Size.cx, ImageData.Size.cy);
  Graphics := TGPGraphics.Create(Bitmap);
  try
    Graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);

    Brush:= TGPSolidBrush.Create(MakeColor(0, 0, 0));
    Pen := TGPPen.Create(MakeColor(213, 223, 229));
    FontFamily := TGPFontFamily.Create(WideString('Tahoma'));
    Font := TGPFont.Create(FontFamily, 12, FontStyleRegular, UnitPixel);
    try
      prev_minute := -1;
      for i := 0 to RecAnalyst.Researches.Count - 1 do
      begin
        R := RecAnalyst.Researches[i] as TResearch;
        minute := Floor(R.Time / 1000 / 60);
        try
          try
            Res := TResourceStream.Create(DllHandle, R.ResourceName, RT_RCDATA);
            StreamInf := TStreamAdapter.Create(Res, soOwned);
            Bmp := TGPBitmap.Create(StreamInf);
            Graphics.DrawImage(Bmp, ImageData.Points[i]);
          except
            //
          end;
        finally
          StreamInf := nil;
          if Assigned(Bmp) then FreeAndNil(Bmp);
        end;

        if (minute <> prev_minute) then
        begin
          Origin.X := ImageData.Points[i].X;
          Origin.Y := ImageData.Size.cy - 20;
          Graphics.DrawString(WideString(Format(c_min, [minute + 1])), -1, Font, Origin, Brush);
          Graphics.DrawLine(Pen, Origin.X - 2, 0, Origin.X - 2, ImageData.Size.cy);
          prev_minute := minute;
        end;
        Item := TImageMapItem.Create();
        Item.Coordinates := Rect(ImageData.Points[i].X, ImageData.Points[i].Y,
          ImageData.Points[i].X + RTW, ImageData.Points[i].Y + RTH);
        Item.Hint := Format('(%s) %s', [TRecAnalyst.GameTimeToString(R.Time), R.Name]);
        ImageMap.Add(Item);
      end;
      //Graphics.DrawLine(Pen, ImageData.Size.cx - 2, 0, ImageData.Size.cx - 2, ImageData.Size.cy);
      y := RTH + 1;
      while (y < ImageData.Size.cy) do
      begin
        Graphics.DrawLine(Pen, 0, y, ImageData.Size.cx - 2, y);
        Inc(y, RTH + VSPACING);
      end;
    finally
      Brush.Free();
      Pen.Free();
      FontFamily.Free();
      Font.Free();
    end;

    GetEncoderClsid('image/png', encoderClsid);

    try
      MemoryStream := TMemoryStream.Create();
      StreamInf := TStreamAdapter.Create(MemoryStream, soOwned);
      Bitmap.Save(StreamInf, encoderClsid);
      MemoryStream.Position := 0;

      Png := TPngImage.Create();
      Png.LoadFromStream(MemoryStream);
      ResearchesImage.Picture.Assign(Png);
    finally
      StreamInf := nil;
      if Assigned(Png) then FreeAndNil(Png);
    end;
  finally
    SetLength(ImageData.Points, 0);
    Bitmap.Free();
    Graphics.Free();
    FinalizeGdiplus();
  end;
end;

end.

