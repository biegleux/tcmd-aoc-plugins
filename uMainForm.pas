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
unit uMainForm;

interface

uses
  Windows, ImgList, Controls, Menus, StdCtrls, ExtCtrls, OleCtrls,
  SHDocVw_EWB, EwbCore, EmbeddedWB, JvExControls, JvLabel, Forms, ComCtrls,
  Classes, Messages, Contnrs, SysUtils, Graphics,
  TeamsVCL, RecAnalyst, ScAnalyst;

type
  TStaticText = class(StdCtrls.TStaticText)
  private
    FOnMouseLeave: TNotifyEvent;
    TrackingMouse: Boolean;
    procedure TrackMouse;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  public
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TMainForm = class(TForm)
    MapPopupMenu: TPopupMenu;
    SaveMapAs: TMenuItem;
    ChatPopupMenu: TPopupMenu;
    SaveasHTML: TMenuItem;
    ImageList: TImageList;
    MainPanel: TPanel;
    PageControl: TPageControl;
    GeneralTabSheet: TTabSheet;
    GeneralScrollBox: TScrollBox;
    GeneralPaintBox: TPaintBox;
    GSLabel: TJvLabel;
    JvLabel1: TJvLabel;
    JvLabel2: TJvLabel;
    JvLabel3: TJvLabel;
    JvLabel4: TJvLabel;
    JvLabel5: TJvLabel;
    JvLabel6: TJvLabel;
    JvLabel7: TJvLabel;
    JvLabel8: TJvLabel;
    JvLabel9: TJvLabel;
    JvLabel10: TJvLabel;
    JvLabel11: TJvLabel;
    POVLabel: TLabel;
    TeamsLabel: TJvLabel;
    MapPaintBox: TPaintBox;
    PlayersLabel: TLabel;
    DurationLabel: TLabel;
    DifficultyLabel: TLabel;
    PopulationLabel: TLabel;
    MapSizeLabel: TLabel;
    SpeedLabel: TLabel;
    LockDiplomacyLabel: TLabel;
    LocationLabel: TLabel;
    MapStyleLabel: TLabel;
    GameTypeLabel: TLabel;
    JvLabel13: TJvLabel;
    ScenarioFileLabel: TLabel;
    JvLabel14: TJvLabel;
    JvLabel15: TJvLabel;
    RevealMapLabel: TLabel;
    GameVersionLabel: TLabel;
    JvLabel12: TJvLabel;
    VictoryLabel: TLabel;
    ChatTabSheet: TTabSheet;
    ChatScrollBox: TScrollBox;
    ChatPaintBox: TPaintBox;
    ChatLabel: TJvLabel;
    ChatWB: TEmbeddedWB;
    TributesTabSheet: TTabSheet;
    TributesScrollBox: TScrollBox;
    TributingPaintBox: TPaintBox;
    TributingLabel: TJvLabel;
    TributesWB: TEmbeddedWB;
    ResearchesTabSheet: TTabSheet;
    ResearchesScrollBox: TScrollBox;
    ResearchesPaintBox: TPaintBox;
    ResearchesLabel: TJvLabel;
    ResPaintBox: TPaintBox;
    ExtraStatsTabSheet: TTabSheet;
    ExtraStatsScrollBox: TScrollBox;
    ExtraStatsPaintBox: TPaintBox;
    ESLabel: TJvLabel;
    UnitsPaintBox: TPaintBox;
    BuildingsPaintBox: TPaintBox;
    ScenarioInfoTabSheet: TTabSheet;
    ScenarioPaintBox: TPaintBox;
    SILabel: TJvLabel;
    ScBgPaintBox: TPaintBox;
    BtnPaintBox1: TPaintBox;
    BtnPaintBox2: TPaintBox;
    BtnPaintBox3: TPaintBox;
    BtnPaintBox4: TPaintBox;
    JvLabel17: TJvLabel;
    JvLabel18: TJvLabel;
    JvLabel20: TJvLabel;
    JvLabel19: TJvLabel;
    HeaderLabel: TJvLabel;
    ScenarioMapPaintBox: TPaintBox;
    ScContentLabel: TLabel;
    AboutTabSheet: TTabSheet;
    AboutScrollBox: TScrollBox;
    AboutPaintBox: TPaintBox;
    APaintBox: TPaintBox;
    JvLabel16: TJvLabel;
    AboutLabel: TJvLabel;
    AboutContentLabel: TLabel;
    CopyrightLabel: TLabel;
    Label3: TLabel;
    PluginURLLabel: TLabel;
    PluginInfoLabel: TLabel;
    OSPaintBox: TPaintBox;
    CommentTabSheet: TTabSheet;
    CommentPaintBox: TPaintBox;
    CommentMemo: TMemo;
    CheckBox1PaintBox: TPaintBox;
    CheckBox2PaintBox: TPaintBox;
    UpdateBtnPaintBox: TPaintBox;
    UpdateLabel: TLabel;
    CBLabel: TLabel;
    ACLabel: TLabel;
    CLabel: TJvLabel;
    MemoShape: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GeneralPaintBoxPaint(Sender: TObject);
    procedure MapPaintBoxPaint(Sender: TObject);
    procedure SaveMapAsClick(Sender: TObject);
    procedure BuildingsPaintBoxPaint(Sender: TObject);
    procedure UnitsPaintBoxPaint(Sender: TObject);
    procedure SaveasHTMLClick(Sender: TObject);
    procedure ResPaintBoxPaint(Sender: TObject);
    procedure ScBgPaintBoxPaint(Sender: TObject);
    procedure BtnPaintBox1Paint(Sender: TObject);
    procedure JvLabel19Click(Sender: TObject);
    procedure BtnPaintBox1Click(Sender: TObject);
    procedure PluginURLLabelClick(Sender: TObject);
    procedure OSPaintBoxPaint(Sender: TObject);
    procedure OSPaintBoxClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ResPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBox1PaintBoxClick(Sender: TObject);
    procedure CheckBox1PaintBoxPaint(Sender: TObject);
    procedure CheckBox2PaintBoxClick(Sender: TObject);
    procedure CheckBox2PaintBoxPaint(Sender: TObject);
    procedure UpdateBtnPaintBoxClick(Sender: TObject);
    procedure UpdateBtnPaintBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateBtnPaintBoxMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateBtnPaintBoxPaint(Sender: TObject);
  private
    TotCmdWin: HWND;     { handle of TC window }
    ParentWin: HWND;     { handle of Lister window }
    QuickView: Boolean;  { Ctrl+Q panel }

    BkgBitmap: TBitmap;
    MapBitmap: TBitmap;
    UnitsBitmap: TBitmap;
    BuildingsBitmap: TBitmap;
    ResearchesBitmap: TBitmap;
    BkgWinBitmap: TBitmap;
    TabDownBitmap, TabUpBitmap: TBitmap;
    OSBitmap: TBitmap;
    BtnDownBitmap, BtnUpBitmap: TBitmap;
    CheckedBitmap, UncheckedBitmap: TBitmap;
    TeamsVCL: TTeamsVCL;
    RecAnalyst: TRecAnalyst;
    ScAnalyst: TScAnalyst;
    ImageMap: TObjectList;

    ObjectivesLabel: TStaticText;

    FileName: String;

    AddCommentB: Boolean;
    CreateBackupB: Boolean;
    UpdateDown: Boolean;

    procedure UpdateGameSettingsVCL;
    procedure UpdateGameChatVCL;
    procedure UpdateTributingVCL;
    procedure UpdateUnitsVCL;
    procedure UpdateBuildingsVCL;
    procedure UpdateMapVCL;
    procedure UpdateResearchesVCL;
    procedure UpdateScenarioVCL;
    procedure UpdateCommentVCL;

    procedure ResetVCL;
    procedure LoadFile;

    procedure TrackMouse;
    procedure WMMouseLeave(var msg: TMessage); message WM_MOUSELEAVE;
    procedure AppException(Sender: TObject; E: Exception);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    TrackingMouse: Boolean;
    constructor CreateParented(ParentWindow: HWND; const FileToView: string); reintroduce;
    procedure OLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OLabelMouseLeave(Sender: TObject);
  end;

function ShowAoE2(ListerWin: HWND; const FileToLoad: String; ShowFlags: Integer): HWND;
procedure HideAoE2(PluginWin: HWND);

implementation

{$R *.dfm}
{$R winxp.res}

uses
  uHintForm, TransparentScrollBox, Context, CommCtrl, RecAnalystConsts, BitmapEx,
  ExtDlgs, pngimage, Dialogs, ShellApi, uLogger, uUploadForm;

type
  TPlugInfo = record
    PlugWinProc: Pointer; { callback function of our form }
    PlugForm: TMainForm;  { our form }
  end;

const
  sExceptionMsg = 'Plugin Error:'#13'%s';
  sExceptionDestroyMsg = 'DestroyWindow Error:'#13'%s';
  sCaption = 'AOC Recorded Games Mgx Viewer';

const
  PLUGIN_URL = 'http://aoc-mgx-utils.sourceforge.net/';
  PLUGIN_NAME = 'A2LView';
  PLUGIN_VERSION = 1.1;
  PLUGIN_DESC = 'tcmd lister plugin for viewing AOE 2 recorded games information';
  PLUGIN_AUTHOR = 'biegleux';
  PLUGIN_AUTHOR_EMAIL = 'biegleux@gmail.com';
  OS_URL = 'http://www.opensource.org/';
  COPYRIGHT_NOTE = 'Copyright (c) 2009 by %s <%s>';
  PLUGIN_CREDITS =
    'Thanks goes to:'#13#10#13#10 +
    ' - Christian Ghisler for the total commander and it''s great plugin interface'#13#10 +
    ' - Bari for sharing mgx file format description'#13#10 +
    ' - Dauro Ibero for constantly contributing, bug reporting and making the analysis much better, thank you!'#13#10 +
    ' - David Tombs for sharing scn and scx file format description'#13#10 +
    ' - www.aoe.cz for providing script hosting capacity'#13#10;

  ANALYZE_ERROR =
    'First four bytes of this file are zeroed. You can use mgxfix tool to fix it.';

  MAP_WIDTH  = 306;
  MAP_HEIGHT = 153;
  MAP_BG_COLOR = clWhite;
  MGL_EXT = '.mgl';
  MGX_EXT = '.mgx';
  SCN_EXT = '.scn';
  SCX_EXT = '.scx';
  MAX_COMMENT_LEN = 3000;

procedure TStaticText.TrackMouse;
var
  EventTrack: Windows.TTrackMouseEvent;
begin
  if not TrackingMouse then
  begin
    EventTrack.cbSize := SizeOf (EventTrack);
    EventTrack.dwFlags := TME_LEAVE;
    EventTrack.dwHoverTime := 0;
    EventTrack.hwndTrack := Handle;
    TrackMouseEvent (EventTrack);
    TrackingMouse := True;
  end;
end;

procedure TStaticText.WMMouseLeave(var Message: TMessage);
begin
  TrackingMouse := False;
  if Assigned (FOnMouseLeave) then
    FOnMouseLeave (Self);
end;

procedure TStaticText.WMMouseMove(var Message: TWMMouseMove);
begin
  TrackMouse;
  if Assigned (OnMouseMove) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with CalcCursorPos do
          MouseMove (KeysToShiftState(Keys), X, Y)
      else
        MouseMove (KeysToShiftState (Keys), Message.XPos, Message.YPos);
end;

procedure TMainForm.OLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  GetCursorPos (p);
  if (p.X + 8 + HintForm.Width > Screen.Width) and (p.X + 8 - HintForm.Width > 0) then
    HintForm.Left := p.X + 8 - HintForm.Width
  else
    HintForm.Left := p.X + 8;

  if (p.Y + 8 + HintForm.Height > Screen.Height) then
    HintForm.Top := p.Y + 8 - HintForm.Height
  else
    HintForm.Top := p.Y + 8;

  if not HintForm.IsVisible then
  begin
    HintForm.IsVisible := True;
    HintForm.Text := RecAnalyst.GameSettings.ObjectivesString;
  end;
end;

procedure TMainForm.OLabelMouseLeave(Sender: TObject);
begin
  if HintForm.IsVisible then
    HintForm.IsVisible := False;
end;

procedure TMainForm.TrackMouse;
var
  EventTrack: Windows.TTrackMouseEvent;
begin
  if not TrackingMouse then
  begin
    EventTrack.cbSize := SizeOf (EventTrack);
    EventTrack.dwFlags := TME_LEAVE;
    EventTrack.dwHoverTime := 0;
    EventTrack.hwndTrack := Handle;
    TrackMouseEvent (EventTrack);
    TrackingMouse := True;
  end;
end;

procedure TMainForm.WMMouseLeave(var msg: TMessage);
begin
  TrackingMouse := false;
end;

procedure wMsgBox(hWnd: HWND; Msg: String);
begin
  MessageBox (hWnd, PChar (Msg), sCaption, MB_OK + MB_ICONINFORMATION);
  //MB_OK or MB_ICONERROR
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
  wMsgBox (Handle, Format (sExceptionMsg, [E.Message]));
end;

procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams (Params);
  Params.Style := (WS_CHILD or WS_MAXIMIZE) and not WS_CAPTION and not WS_BORDER;
  Params.WindowClass.cbWndExtra := SizeOf (Pointer); // 4 bytes for address of form
end;

constructor TMainForm.CreateParented(ParentWindow: HWND; const FileToView: String);
const
  WinCmdClassName = 'TTOTAL_CMD';
begin
  inherited CreateParented (ParentWindow);
  TotCmdWin := FindWindow (WinCmdClassName, nil);
  ParentWin := ParentWindow;
  QuickView := GetParent (ParentWin) <> 0;

  FileName := FileToView;
//  RecAnalyst.FileName := FileToView;
//  LoadFileName (FileToView);
end;

function HookDestroy(PluginWin: HWND; Msg, wParam, lParam: LongInt): LongInt; stdcall;
var
  p: ^TPlugInfo;
begin
  { hook destroy our window }
  p := Pointer (GetWindowLong (PluginWin, GWL_USERDATA));
  if Msg <> WM_DESTROY then
    Result := CallWindowProc (p^.PlugWinProc, PluginWin, Msg, wParam, lParam)
  else
  begin
    { plugin close }
    HideAoE2 (PluginWin);
    Result := 0;
  end;
end;

procedure HideAoE2(PluginWin: HWND);
var
  p: ^TPlugInfo;
begin
  p := Pointer (GetWindowLong (PluginWin, GWL_USERDATA));
  with p^.PlugForm do
    try
      Application.RemoveComponent (p^.PlugForm);
      Application.Handle := 0;
      { restore callback function }
      SetWindowLong (Handle, GWL_WNDPROC, Integer (p^.PlugWinProc));
      Free;
    except
      on E: Exception do
        wMsgBox (Handle, Format (sExceptionDestroyMsg, [E.Message]));
    end;
  Dispose (p);
end;

function ShowAoE2(ListerWin: HWND; const FileToLoad: String; ShowFlags: Integer): HWND;
var
  MainForm: TMainForm;
  s: String;
  p: ^TPlugInfo;
begin
  try
    s := ExtractFilePath (FileToLoad);
    if not SetCurrentDir (s) then
      raise Exception.Create (Format ('Error of SetCurrentDir() for Folder: %s', [s]));

    MainForm := TMainForm.CreateParented (ListerWin, FileToLoad);
    MainForm.Show;
    { synchronize our form and Lister }
    Application.Handle := ListerWin;
    Application.OnException := MainForm.AppException;
    Application.InsertComponent (MainForm);

    { substitution callback function }
    New (p);
    SetWindowLong (MainForm.Handle, GWL_USERDATA, Integer (p));
    p^.PlugForm := MainForm;
    p^.PlugWinProc := Pointer (SetWindowLong (MainForm.Handle, GWL_WNDPROC, Integer (@HookDestroy)));

    { set focus to our window }
    if not MainForm.QuickView then
      PostMessage (MainForm.Handle, WM_SETFOCUS, 0, 0);
    Result := MainForm.Handle;
  except
    on E: Exception do
    begin
      wMsgBox (ListerWin, Format (sExceptionMsg, [E.Message]));
      Result := 0;
    end;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
//
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
//
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.FormCreate(Sender: TObject);
var
  CSS: TStringList;
  sPath: String;
  TSB: TTransparentScrollBox;
  srcRect, dstRect: TRect;
  bmp: TBitmap;
  FS: TFormatSettings;
  Msg: TLoggerMessage;
const
  BG_COLOR = clGray;
begin
//  PageControl.Align := alNone;
  PageControl.Left := 4;
  PageControl.Top := 4;
  PageControl.Width := MainPanel.ClientWidth - PageControl.Left - 4;
  PageControl.Height := MainPanel.ClientHeight - PageControl.Left - 4;
  PageControl.Anchors := [akLeft, akTop, akRight, akBottom];

  ObjectivesLabel := TStaticText.Create (Self);
  with ObjectivesLabel do
  begin
    Parent := GeneralScrollBox;
    ParentColor := True;
    ParentFont := True;
    Left := 424;
    Top := 32;
    Width := 65;
    Height := 17;
    Caption := 'Objectives';
    AutoSize := True;
    OnMouseMove := OLabelMouseMove;
    OnMouseLeave := OLabelMouseLeave;
  end;

  FS.DecimalSeparator := '.';
  PluginInfoLabel.Caption := Format ('%s v%.1f - %s', [PLUGIN_NAME, PLUGIN_VERSION, PLUGIN_DESC], FS);
  CopyrightLabel.Caption := Format (COPYRIGHT_NOTE, [PLUGIN_AUTHOR, PLUGIN_AUTHOR_EMAIL]);
  PluginURLLabel.Caption := PLUGIN_URL;
  AboutContentLabel.Caption := PLUGIN_CREDITS;
  JvLabel16.Caption := Format ('About %s plugin', [PLUGIN_NAME]);  

  BkgBitmap := TBitmap.Create;
  MapBitmap := TBitmap.Create;
  UnitsBitmap := TBitmap.Create;
  BuildingsBitmap := TBitmap.Create;
  ResearchesBitmap := TBitmap.Create;
  OSBitmap := TBitmap.Create;
  CheckedBitmap := TBitmap.Create;
  UncheckedBitmap := TBitmap.Create;
  BtnUpBitmap := TBitmap.Create;
  BtnDownBitmap := TBitmap.Create;  

  TeamsVCL := TTeamsVCL.Create (Self, GeneralScrollBox);

  RecAnalyst := TRecAnalyst.Create;
  RecAnalyst.KeepStreams := True;

  ScAnalyst := TScAnalyst.Create;
  ImageMap := TObjectList.Create;
  
  with UnitsBitmap do
  begin
    TransparentColor := BG_COLOR;
    Transparent := True;
    Canvas.Brush.Color := BG_COLOR;
    Canvas.Font.Name := 'Georgia';
  end;

  with BuildingsBitmap do
  begin
    TransparentColor := BG_COLOR;
    Transparent := True;
    Canvas.Font.Name := 'Georgia';
    Canvas.Brush.Color := BG_COLOR;
  end;

  with ResearchesBitmap do
  begin
    TransparentColor := BG_COLOR;
    Transparent := True;
    Canvas.Font.Name := 'Georgia';
    Canvas.Brush.Color := BG_COLOR;
  end;

  try
    TResourceDll.LoadBitmap (BACKGROUND_RESNAME, BkgBitmap);
  except
    on E: Exception do
      Logger.SendException ('Resource Name: %s', [BACKGROUND_RESNAME], E);
  end;

  try
    TResourceDll.LoadBitmap (OPENSRC_RESNAME, OSBitmap);
  except
    on E: Exception do
      Logger.SendException ('Resource Name: %s', [OPENSRC_RESNAME], E);
  end;

  if (DllHandle <> 0) then
  begin
    with ImageList do
    begin
      Handle := ImageList_LoadImage (DllHandle, ICONS_RESNAME, Width, AllocBy, clFuchsia,
                    IMAGE_BITMAP, LR_CREATEDIBSECTION);
    end;
    PageControl.Images := ImageList;
  end;

  try
    TResourceDll.LoadBitmap (BTNDOWN_RESNAME, BtnDownBitmap);
    TResourceDll.LoadBitmap (BTNUP_RESNAME, BtnUpBitmap);
  except
    on E: Exception do
    begin
      if BtnDownBitmap.HandleAllocated then
        BtnDownBitmap.Assign (nil);
      Logger.SendException (E);
    end;
  end;

  try
    TResourceDll.LoadBitmap (CHECKED_RESNAME, CheckedBitmap);
    TResourceDll.LoadBitmap (UNCHECKED_RESNAME, UncheckedBitmap);
  except
    on E: Exception do
    begin
      if CheckedBitmap.HandleAllocated then
        CheckedBitmap.Assign (nil);
      Logger.SendException (E);
    end;
  end;

  { adjust label positions }
  GameTypeLabel.Left := JvLabel1.Left + JvLabel1.Width + 2;
  MapStyleLabel.Left := JvLabel2.Left + JvLabel2.Width + 2;
  LocationLabel.Left := JvLabel3.Left + JvLabel3.Width + 2;
  PlayersLabel.Left := JvLabel4.Left + JvLabel4.Width + 2;
  DurationLabel.Left := JvLabel5.Left + JvLabel5.Width + 2;
  DifficultyLabel.Left := JvLabel6.Left + JvLabel6.Width + 2;
  PopulationLabel.Left := JvLabel7.Left + JvLabel7.Width + 2;
  MapSizeLabel.Left := JvLabel8.Left + JvLabel8.Width + 2;
  SpeedLabel.Left := JvLabel9.Left + JvLabel9.Width + 2;
  LockDiplomacyLabel.Left := JvLabel10.Left + JvLabel10.Width + 2;
  RevealMapLabel.Left := JvLabel11.Left + JvLabel11.Width + 2;
  VictoryLabel.Left := JvLabel12.Left + JvLabel12.Width + 2;
  POVLabel.Left := JvLabel13.Left + JvLabel13.Width + 2;
  GameVersionLabel.Left := JvLabel14.Left + JvLabel14.Width + 2;
  ScenarioFileLabel.Left := JvLabel15.Left + JvLabel15.Width + 2;

  { Initialize Web Browser Controls }
  sPath := IncludeTrailingPathDelimiter (ExtractFilePath (GetModuleName (HInstance)));
  CSS := TStringList.Create;
  try
    try
      CSS.LoadFromFile (sPath + CSS_FILE);
      ChatWB.HostCSS := CSS.Text;
      TributesWB.HostCSS := CSS.Text;
//      ChatWB.Navigate ('about:blank');
//      ChatWB.LoadFromString ('');
//      TributesWB.Navigate ('about:blank');
//      TributesWB.LoadFromString ('');
    except
      on E: Exception do
        Logger.SendException (E);
    end;
  finally
    CSS.Free;
  end;

  ChatWB.Width   := ChatWB.Parent.ClientWidth - 2 * ChatWB.Left;
  ChatWB.Height  := ChatWB.Parent.ClientHeight - ChatWB.Top - ChatWB.Left;
  ChatWB.Anchors := [akLeft, akTop, akRight, akBottom];

  TributesWB.Width   := TributesWB.Parent.ClientWidth - 2 * TributesWB.Left;
  TributesWB.Height  := TributesWB.Parent.ClientHeight - TributesWB.Top - TributesWB.Left;
  TributesWB.Anchors := [akLeft, akTop, akRight, akBottom];

  ResetVCL;

  { Scenario Tab Sheet }
  BkgWinBitmap := TBitmap.Create;
  BkgWinBitmap.Transparent := True;
  TabDownBitmap := TBitmap.Create;
  TabUpBitmap := TBitmap.Create;

  try
    TResourceDll.LoadBitmap (BGWIN_RESNAME, BkgWinBitmap);
    TResourceDll.LoadBitmap (TABDOWN_RESNAME, TabDownBitmap);
    TResourceDll.LoadBitmap (TABUP_RESNAME, TabUpBitmap);

    TSB := TTransparentScrollBox.Create (Self);
    with TSB do
    begin
      Parent := ScenarioInfoTabSheet;
      VertScrollBar.Tracking := True;
      HorzScrollBar.Tracking := True;
      Left := 80;
      Top := 100;
      Width := 489;
      Height := 337;
      BorderStyle := bsNone;
    end;
    srcRect := Rect (TSB.Left - ScBgPaintBox.Left, TSB.Top - ScBgPaintBox.Top,
        TSB.Left - ScBgPaintBox.Left + TSB.ClientWidth, TSB.Top -
        ScBgPaintBox.Top + TSB.ClientHeight);
    dstRect := Rect (0, 0, TSB.ClientWidth, TSB.ClientHeight);
    bmp := TBitmap.Create;
    bmp.Width := TSB.ClientWidth;
    bmp.Height := TSB.ClientHeight;
    bmp.Canvas.CopyRect (dstRect, BkgWinBitmap.Canvas, srcRect);
    TSB.Bitmap := bmp;
    TSB.OwnBitmap := True;

    ScContentLabel.Parent := TSB;
    ScContentLabel.Left := 0;
    ScContentLabel.Top := 0;
    ScenarioMapPaintBox.Parent := TSB;
    ScenarioMapPaintBox.Left := TSB.ClientWidth div 2 - ScenarioMapPaintBox.ClientWidth div 2;
    ScenarioMapPaintBox.Top := 60;
  except
    on E: Exception do
      Logger.SendException (E);
  end;

  AddCommentB := False;
  CreateBackupB := True;
  UpdateDown := False;
  CommentMemo.MaxLength := MAX_COMMENT_LEN;

  HintForm := THintForm.Create (Self);
  TeamsVCL.HintWin := HintForm;

  LoadFile;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BkgBitmap.Free;
  MapBitmap.Free;
  UnitsBitmap.Free;
  BuildingsBitmap.Free;
  ResearchesBitmap.Free;
  BkgWinBitmap.Free;
  TabDownBitmap.Free;
  TabUpBitmap.Free;
  OSBitmap.Free;
  TeamsVCL.Free;
  RecAnalyst.Free;
  ScAnalyst.Free;
  ImageMap.Clear;
end;

procedure TMainForm.LoadFile;
var
  Msg: TLoggerMessage;
  UF: TUploadForm;
begin
  { clear logs from previous view if any exist }
  Logger.MessageList.Clear;
  try
//    ResetVCL;
    if (ExtractFileExt (FileName) = SCN_EXT) or
       (ExtractFileExt (FileName) = SCX_EXT) then
    begin
      { we have got a scenario file }
//      ScAnalyst.Reset;
      ScAnalyst.FileName := FileName;
      try
        if ScAnalyst.Analyze then
          UpdateScenarioVCL
        else
          ResetVCL;
      except
        on E: Exception do
        begin
          Logger.SendException (E, True);
//          raise;
        end;
      end;
    end else
    begin
//      RecAnalyst.Reset;
      RecAnalyst.FileName := FileName;
      try
        if RecAnalyst.Analyze then
        begin
          UpdateGameSettingsVCL;
          UpdateMapVCL;
          TeamsVCL.Build (RecAnalyst, TeamsLabel.Left + 12, TeamsLabel.Top + 22);
          UpdateGameChatVCL;
          UpdateTributingVCL;
          UpdateResearchesVCL;
          UpdateUnitsVCL;
          UpdateBuildingsVCL;
          UpdateCommentVCL;
        end else
          ResetVCL;
      except
        on E: Exception do
        begin
          if not RecAnalyst.ZeroHeaderLen then
            Logger.SendException (E, True)
          else
            MessageDlg (ANALYZE_ERROR, mtInformation, [mbOK], 0);
          //raise;
        end;
      end;

      if (Logger.MessageList.Count > 0) then
      begin
        Msg := TLoggerMessage.Create;
        Msg.MessageType := mtMessage;
        Msg.MessageText := Format ('Generated log for file %s (plugin ver.: %f):', [ExtractFileName(FileName), PLUGIN_VERSION]);
        Logger.MessageList.Insert(Msg);

        if MessageDlg ('File was not properly analyzed. Would you like to upload it to make the analysis better?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          UF := TUploadForm.Create (Self);
          UF.FileName := FileName;
          UF.Show;
        end;
      end;

    end;
  finally
    //RecAnalyst.Reset;
  end;
end;

procedure TMainForm.UpdateGameSettingsVCL;
begin
  if not Assigned (RecAnalyst) then
    Exit;

  with RecAnalyst, RecAnalyst.GameSettings do
  begin
    MapStyleLabel.Visible := not IsScenario and IsAOC;
    LocationLabel.Visible := not IsScenario;
    PopulationLabel.Visible := not IsScenario;
    MapSizeLabel.Visible := not IsScenario;
    LockDiplomacyLabel.Visible := not IsScenario;
    ScenarioFileLabel.Visible := IsScenario;
    JvLabel15.Visible := IsScenario;
    ObjectivesLabel.Visible := not IsScenario;

    if IsScenario then
    begin
      JvLabel2.Font.Style := JvLabel2.Font.Style + [fsStrikeOut];
      JvLabel3.Font.Style := JvLabel3.Font.Style + [fsStrikeOut];
      JvLabel7.Font.Style := JvLabel7.Font.Style + [fsStrikeOut];
      JvLabel8.Font.Style := JvLabel8.Font.Style + [fsStrikeOut];
      JvLabel10.Font.Style := JvLabel10.Font.Style + [fsStrikeOut];
      ScenarioFileLabel.Caption := ScFileName;
      TeamsLabel.Top := JvLabel15.Top + 28;
    end else
    begin
      JvLabel2.Font.Style := JvLabel2.Font.Style - [fsStrikeOut];
      JvLabel3.Font.Style := JvLabel3.Font.Style - [fsStrikeOut];
      JvLabel7.Font.Style := JvLabel7.Font.Style - [fsStrikeOut];
      JvLabel8.Font.Style := JvLabel8.Font.Style - [fsStrikeOut];
      JvLabel10.Font.Style := JvLabel10.Font.Style - [fsStrikeOut];
      TeamsLabel.Top := JvLabel14.Top + 28;
    end;
    if IsAOK then
      JvLabel2.Font.Style := JvLabel2.Font.Style + [fsStrikeOut];

    GameTypeLabel.Caption := sGameType;
    MapStyleLabel.Caption := sMapStyle;
    LocationLabel.Caption := Map;
    PlayersLabel.Caption := Players;
    DurationLabel.Caption := TRecAnalyst.GameTimeToString (PlayTime);
    DifficultyLabel.Caption := sDifficultyLevel;
    PopulationLabel.Caption := IntToStr (PopLimit);
    MapSizeLabel.Caption := sMapSize;
    SpeedLabel.Caption := sGameSpeed;
    RevealMapLabel.Caption := sRevealMap;
    if LockDiplomacy then
      LockDiplomacyLabel.Caption := 'Yes'
    else
      LockDiplomacyLabel.Caption := 'No';
    VictoryLabel.Caption := Victory.VictoryString;
    POVLabel.Caption := POVEx;
    GameVersionLabel.Caption := sGameVersion;
  end;
end;

procedure TMainForm.UpdateGameChatVCL;
var
  ChatMessages: TStringList;
  sColor: String;
  i, sum: Integer;
  Player: TPlayer;
  Spammers: array[1..8] of Integer;
  ChatMessage: TChatMessage;
begin
  if not Assigned (RecAnalyst) then
    Exit;

  ChatTabSheet.TabVisible := RecAnalyst.IsAOC;
  if not RecAnalyst.IsAOC then
    Exit;

  ChatWB.LoadFromString (''); { need to call it twice? }
  FillChar (Spammers, SizeOf (Spammers), 0);
  ChatMessages := TStringList.Create;
  try
    { pre-game chat }
    if (RecAnalyst.PreGameChatMessages.Count > 0) then
    begin
      ChatMessages.Add ('<span class="section">pre-game chat:</span>');
      for i := 0 to RecAnalyst.PreGameChatMessages.Count - 1 do
      begin
        ChatMessage := RecAnalyst.PreGameChatMessages[i] as TChatMessage;
        if Assigned (ChatMessage.Player) then
        begin
          sColor := HTMLCOLORS[ChatMessage.Player.ColorId];
          Inc (Spammers[ChatMessage.Player.Index]);
        end else
          sColor := '#ffffff';
        ChatMessages.Add (Format ('<span style="color:%s">%s</span><br />',
            [sColor, HTMLSpecialChars (ChatMessage.Msg)]));
      end;
    end;
    { in-game chat }
    if (RecAnalyst.InGameChatMessages.Count > 0) then
    begin
      ChatMessages.Add ('<span class="section">in-game chat:</span>');
      for i := 0 to RecAnalyst.InGameChatMessages.Count - 1 do
      begin
        ChatMessage := RecAnalyst.InGameChatMessages[i] as TChatMessage;
        if Assigned (ChatMessage.Player) then
        begin
          sColor := HTMLCOLORS[ChatMessage.Player.ColorId];
          Inc (Spammers[ChatMessage.Player.Index]);
        end else
          sColor := '#ffffff';
        ChatMessages.Add (Format ('<span class="time">(%s)</span>',
            [TRecAnalyst.GameTimeToString (ChatMessage.Time)]));
        ChatMessages.Add (Format ('<span class="message" style="color:%s">%s</span><br />',
            [sColor, HTMLSpecialChars (ChatMessage.Msg)]));
      end;
    end;
    sum := 0;
    for i := Low (Spammers) to High (Spammers) do
      Inc (sum, Spammers[i]);
    if (sum > 0) then
    begin
      ChatMessages.Add (Format ('<span class="section">messages in total: %d</span>', [sum]));
      for i := Low (Spammers) to High (Spammers) do
      begin
        Player := RecAnalyst.PlayerList.GetPlayerByIndex (i);
        if not Assigned (Player) then
          Continue;

          ChatMessages.Add (Format ('<span style="color:%s">%s:</span> %d messages<br />',
              [HTMLCOLORS[Player.ColorId], HTMLSpecialChars (Player.Name), Spammers[i]]));
//          if (Spammers[i] > 0) then
      end;
    end;
    ChatWB.LoadFromStrings (ChatMessages);
  finally
    ChatMessages.Free;
  end;

  // AutoPopup? 
  if (RecAnalyst.PreGameChatMessages.Count > 0) or (RecAnalyst.InGameChatMessages.Count > 0) then
    ChatWB.PopupMenu := ChatPopupMenu
  else
    ChatWB.PopupMenu := nil;
end;

procedure TMainForm.UpdateTributingVCL;
type
  TTributeSent = record
    Amount: Integer;
    Food, Wood, Stone, Gold: Integer;
  end;
  TTributeReceived = TTributeSent; { Fee included }

var
  Tribute: TTribute;
  TributeMessages: TStringList;
  Msg: string;
  Player: TPlayer;
  Senders: array[0..7] of TTributeSent;
  Receivers: array[0..7] of TTributeReceived;
  i: Integer;
  sSnd, sRcv: String;
begin
  if not Assigned (RecAnalyst) then
    Exit;

  if (RecAnalyst.Tributes.Count = 0) then
  begin
    TributesWB.LoadFromString ('');
    Exit;
  end;

  TributesWB.LoadFromString (''); { need to call it twice? }
  FillChar (Senders, SizeOf (Senders), 0);
  FillChar (Receivers, SizeOf (Receivers), 0);

  TributeMessages := TStringList.Create;
  try
    for i := 0 to RecAnalyst.Tributes.Count - 1 do
    begin
      Tribute := RecAnalyst.Tributes[i] as TTribute;
      with Tribute do
      begin
        Msg := Format ('<span class="time">(%s)</span> <span style="color:%s">%s</span> ' +
                         'has sent %d (-%d) %s to <span style="color:%s">%s</span><br />',
                [TRecAnalyst.GameTimeToString (Time), HTMLCOLORS[PlayerFrom.ColorId], PlayerFrom.Name, Amount, Round (Amount + (Amount * Fee)),
                LowerCase (RESOURCES[ResourceId]), HTMLCOLORS[PlayerTo.ColorId], PlayerTo.Name]);

        TributeMessages.Add (Msg);

        if PlayerFrom.Index - 1 in [0..7] then
        begin
          Inc (Senders[PlayerFrom.Index - 1].Amount, Round (Amount + (Amount * Fee)));
          case ResourceId of
            rFood:  Inc (Senders[PlayerFrom.Index - 1].Food, Round (Amount + (Amount * Fee)));
            rWood:  Inc (Senders[PlayerFrom.Index - 1].Wood, Round (Amount + (Amount * Fee)));
            rStone: Inc (Senders[PlayerFrom.Index - 1].Stone, Round (Amount + (Amount * Fee)));
            rGold:  Inc (Senders[PlayerFrom.Index - 1].Gold, Round (Amount + (Amount * Fee)));
          end;
        end;
        if PlayerTo.Index - 1 in [0..7] then
        begin
          Inc (Receivers[PlayerTo.Index - 1].Amount, Amount);
          case ResourceId of
            rFood:  Inc (Receivers[PlayerTo.Index - 1].Food, Amount);
            rWood:  Inc (Receivers[PlayerTo.Index - 1].Wood, Amount);
            rStone: Inc (Receivers[PlayerTo.Index - 1].Stone, Amount);
            rGold:  Inc (Receivers[PlayerTo.Index - 1].Gold, Amount);
          end;
        end;
      end;
    end;

    sSnd := ''; sRcv := '';
    for i := 0 to 7 do
    begin
      Player := RecAnalyst.PlayerList.GetPlayerByIndex (i + 1);
      if not Assigned (Player) then
        Continue;
        
      if (Senders[i].Amount > 0) then
      begin
        sSnd := sSnd + Format ('<span style="color:%s">%s</span> has sent ' +
                                  '%d (food: %d, wood: %d, stone: %d, gold: %d) resources in total<br />' + #13#10,
                        [HTMLCOLORS[Player.ColorId], Player.Name, Senders[i].Amount, Senders[i].Food,
                        Senders[i].Wood, Senders[i].Stone, Senders[i].Gold]);
      end;

      if (Receivers[i].Amount > 0) then
      begin
        sRcv := sRcv + Format ('<span style="color:%s">%s</span> has received ' +
                                  '%d (food: %d, wood: %d, stone: %d, gold: %d) resources in total<br />',
                        [HTMLCOLORS[Player.ColorId], Player.Name, Receivers[i].Amount, Receivers[i].Food,
                        Receivers[i].Wood, Receivers[i].Stone, Receivers[i].Gold]);
      end;
    end;

    TributeMessages.Insert (0, '<br />');
    TributeMessages.Insert (0, sRcv);
    TributeMessages.Insert (0, sSnd);
    TributeMessages.Insert (0, '<span class="section">tributes in total:</span>');

    TributesWB.LoadFromStrings (TributeMessages);
  finally
    TributeMessages.Free;
  end;
end;

procedure TMainForm.UpdateUnitsVCL;
var
  ResourceBmp: TBitmapEx;
  TU: TTrainedUnit;
  i: Integer;
const
  RES_BMP_WH = 20;
  BG_COLOR = clGray;
  CAPTION_STR = 'trained units in total';
begin
  if (RecAnalyst.Units.Count = 0) or not DllLoaded then
  begin
    UnitsBitmap.Assign (nil);
    Exit;
  end;

  ResourceBmp := TBitmapEx.Create;
  try
    UnitsBitmap.Width := UnitsBitmap.Canvas.TextWidth (CAPTION_STR);
    UnitsBitmap.Height := RecAnalyst.Units.Count * RES_BMP_WH + 20;
    UnitsBitmap.Canvas.TextOut (0, 0, CAPTION_STR);

    for i := 0 to RecAnalyst.Units.Count - 1 do
    begin
      TU := RecAnalyst.Units[i];
      try
        TResourceDll.LoadBitmap (TU.ResName, TBitmap(ResourceBmp));
        ResourceBmp.Resize2 (20, 20);
        UnitsBitmap.Canvas.Draw (0, 20 + i * RES_BMP_WH, ResourceBmp);
//        UnitsBitmap.Canvas.StretchDraw (Rect (0, 20 + i * RES_BMP_WH,
//            RES_BMP_WH, 20 + i * RES_BMP_WH + RES_BMP_WH), ResourceBmp);
        UnitsBitmap.Canvas.TextOut (RES_BMP_WH + 4, 20 + i * RES_BMP_WH, IntToStr (TU.Count));
      except
        on E: Exception do
          Logger.SendException ('Data: U.Id: %d, U.ResName: %s', [TU.Id, TU.ResName], E, True);
          //TODO!
//          TLog.Log (Format ('%s (Data: U.Id: %d, U.ResName: %s, FileName: %s)',
//              [E.Message, TU.Id, TU.ResName, RecAnalyst.FileName]), ClassName, 'UpdateUnitsVCL');
      end;
    end;
  finally
    ResourceBmp.Free;
  end;

  UnitsPaintBox.ClientWidth := UnitsBitmap.Width;
  UnitsPaintBox.ClientHeight := UnitsBitmap.Height;
  UnitsPaintBox.Repaint;
end;

procedure TMainForm.UpdateBuildingsVCL;
type
  PBuilding = ^TBuildingRec;
  TBuildingRec = record
    Id, Sum: Integer;
    Num: array[0..7] of Integer;
    Name, ResName: String[255];
  end;

  function BuildingsCompare(Item1, Item2: Pointer): Integer;
  begin
    if PBuilding (Item1).Sum < PBuilding (Item2).Sum then
      Result := 1
    else if PBuilding (Item1).Sum > PBuilding (Item2).Sum then
      Result := -1
    else
      Result := 0;
  end;

var
  Player: TPlayer;
  B: TBuilding;
  i, j, k, idx: Integer;
  BuildingList: TList;
  BuildingPtr: PBuilding;
  ResBmp: TBitmapEx;
  TextWidth: Integer;
  x_offsets: array[0..8] of Integer; { 0 is for column "total" }
  BmpWidth: Integer;
const
  RES_BMP_WH = 20;
  BG_COLOR = clGray;
  TOTAL_STR = 'total';
begin
  if not DllLoaded then
    Exit;

  { shifting PB according to units PB }
  BuildingsPaintBox.Left := UnitsPaintBox.Left + UnitsPaintBox.Width;

  { create rearranged list of buildings
    columns: building id, total, # for player1, # for player2, etc. }
  BuildingList := TList.Create;
  try
    { for each player }
    for i := 0 to RecAnalyst.PlayerList.Count - 1 do
    begin
      Player := RecAnalyst.PlayerList[i];
      { for each her / his building built }
      for j := 0 to Player.Buildings.Count - 1 do
      begin
        B := Player.Buildings[j];

        { find given building in our new building list (based on its id) }
        idx := -1;
        for k := 0 to BuildingList.Count - 1 do
        begin
          if (PBuilding (BuildingList[k])^.Id = B.Id) then
          begin
            { found, update column "total" as well as column for this player }
            Inc (PBuilding (BuildingList[k])^.Sum, B.Count);
            PBuilding (BuildingList[k])^.Num[Player.Index - 1] := B.Count;
            idx := k;
            Break;
          end;
        end;

        if (idx = -1) then
        begin
          { not found, building has not been added to our new building list yet, add it than }
          New (BuildingPtr);
          FillChar (BuildingPtr^, SizeOf (TBuildingRec), 0);
          with BuildingPtr^ do
          begin
            Id := B.Id;
            Sum := B.Count;
            Num[Player.Index - 1] := B.Count;
            Name := B.Name;
            ResName := B.ResName;
          end;
          BuildingList.Add (BuildingPtr);
        end;
      end;
    end;

    if (BuildingList.Count = 0) then
    begin
      BuildingsBitmap.Assign (nil);
      Exit;
    end;

    { sort our new building list by the column "total" }
    BuildingList.Sort (@BuildingsCompare);

    { draw bitmap finally }
    ResBmp := TBitmapEx.Create;
    try
      { calculate x-offsets for each column }
      FillChar (x_offsets, SizeOf (x_offsets), 0);

      BmpWidth := RES_BMP_WH + 10;
      x_offsets[0] := BmpWidth;      
      TextWidth := BuildingsBitmap.Canvas.TextWidth (TOTAL_STR);
      for i := 0 to RecAnalyst.PlayerList.Count - 1 do
      begin
        Player := RecAnalyst.PlayerList[i];
        { TODO: testovat coop }
        if Player.IsCooping then
          Continue;
        Inc (BmpWidth, TextWidth + 10);
        x_offsets[Player.Index] := BmpWidth;
        TextWidth := BuildingsBitmap.Canvas.TextWidth (Player.Name);
      end;
      Inc (BmpWidth, TextWidth + 10);

      { set calculated dimensions }
      BuildingsBitmap.Width := BmpWidth;
      BuildingsBitmap.Height := BuildingList.Count * RES_BMP_WH + 20;

      { draw "total" and player names in the header }
      BuildingsBitmap.Canvas.TextOut (x_offsets[0], 0, TOTAL_STR);
      for i := 0 to RecAnalyst.PlayerList.Count - 1 do
      begin
        Player := RecAnalyst.PlayerList[i];
        { TODO: testovat coop }
        if Player.IsCooping then
          Continue;
        BuildingsBitmap.Canvas.Font.Color := Player.Color;
        BuildingsBitmap.Canvas.TextOut (x_offsets[Player.Index], 0, Player.Name);
      end;

      { set the color back to black }
      BuildingsBitmap.Canvas.Font.Color := clBlack;

      { draw columns, iterating by lines }
      for i := 0 to BuildingList.Count - 1 do
      begin
        BuildingPtr := BuildingList[i];

        { draw building image and column "total" }
        try
          { moze sa stat, ze BuildingPtr^.ResName = '' ak neexistujuce Id }
          TResourceDll.LoadBitmap (BuildingPtr^.ResName, TBitmap(ResBmp));
          ResBmp.Resize2 (RES_BMP_WH, RES_BMP_WH);
          BuildingsBitmap.Canvas.Draw (0, RES_BMP_WH + i * RES_BMP_WH, ResBmp);
//          BuildingsBitmap.Canvas.StretchDraw (Rect (0, RES_BMP_WH + i * RES_BMP_WH, RES_BMP_WH, RES_BMP_WH + i * RES_BMP_WH + RES_BMP_WH), ResBmp);
          BuildingsBitmap.Canvas.TextOut (x_offsets[0], RES_BMP_WH + i * RES_BMP_WH, IntToStr (BuildingPtr^.Sum));
        except
          on E: Exception do
            Logger.SendException ('Data: B.Id: %d, B.ResName: %s', [BuildingPtr^.Id, BuildingPtr^.ResName], E, True);
            //TODO!
            //TLog.Log (Format ('%s (Data: B.Id: %d, B.ResName: %s, FileName: %s)',
            //    [E.Message, BuildingPtr^.Id, BuildingPtr^.ResName, RecAnalyst.FileName]), ClassName, 'UpdateBuildingsVCL');
        end;

        { draw each player's column (0..7) }
        for j := Low (BuildingPtr^.Num) to High (BuildingPtr^.Num) do
        begin
          Player := RecAnalyst.PlayerList.GetPlayerByIndex (j + 1);
          if not Assigned (Player) then
            Continue;

          if (BuildingPtr^.Num[j] = 0) then
            BuildingsBitmap.Canvas.TextOut (x_offsets[Player.Index], RES_BMP_WH + i * RES_BMP_WH, '-')
          else
            BuildingsBitmap.Canvas.TextOut (x_offsets[Player.Index], RES_BMP_WH + i * RES_BMP_WH, IntToStr (BuildingPtr^.Num[j]));
        end;

      end;
      { set dimensions for PB according to buildings image and repaint }
      BuildingsPaintBox.ClientWidth := BuildingsBitmap.Width;
      BuildingsPaintBox.ClientHeight := BuildingsBitmap.Height;
      BuildingsPaintBox.Repaint;
    finally
      ResBmp.Free;
    end;

  finally
    for i := BuildingList.Count - 1 downto 0 do
    begin
      BuildingPtr := BuildingList[i];
      BuildingList.Delete (i);
      Dispose (BuildingPtr);
    end;
    BuildingList.Free;
  end;
end;

procedure TMainForm.UpdateMapVCL;
begin
  if not RecAnalyst.GenerateMap (MapBitmap, MAP_WIDTH, MAP_HEIGHT, MAP_BG_COLOR) then
    MapBitmap.Assign (nil);

  if Assigned (MapBitmap) and MapBitmap.HandleAllocated then
  begin
    MapBitmap.Transparent := True;
    MapPaintBox.Repaint;
    MapPaintBox.PopupMenu := MapPopupMenu;
  end else
    MapPaintBox.PopupMenu := nil;
end;

procedure TMainForm.UpdateResearchesVCL;
begin
  if (RecAnalyst.Researches.Count = 0) or not DllLoaded then
    Exit;

  if not RecAnalyst.GenerateResearches (ResearchesBitmap, ImageMap) then
  begin
    ResearchesBitmap.Assign (nil);
    Exit;
  end;

  if Assigned (ResearchesBitmap) and ResearchesBitmap.HandleAllocated then
  begin
    ResPaintBox.ClientWidth := ResearchesBitmap.Width;
    ResPaintBox.ClientHeight := ResearchesBitmap.Height;
    ResPaintBox.Repaint;
  end;
end;

procedure TMainForm.UpdateScenarioVCL;
var
  i: Integer;
begin
  if not ScAnalyst.GenerateMap (MapBitmap, MAP_WIDTH, MAP_HEIGHT, MAP_BG_COLOR) then
    MapBitmap.Assign (nil);

  if Assigned (MapBitmap) and MapBitmap.HandleAllocated then
  begin
    MapBitmap.Transparent := True;
    ScenarioMapPaintBox.Repaint;
    ScenarioMapPaintBox.PopupMenu := MapPopupMenu;
  end else
    ScenarioMapPaintBox.PopupMenu := nil;

  BtnPaintBox1Click(BtnPaintBox1);
  for i := 0 to PageControl.PageCount - 1 do
    PageControl.Pages[i].TabVisible := False;
  ScenarioInfoTabSheet.TabVisible := True;
  AboutTabSheet.TabVisible := True;
end;

procedure TMainForm.UpdateCommentVCL;
begin
  if RecAnalyst.GameSettings.IsScenario then
    Exit;
  CommentTabSheet.TabVisible := True;
  AddCommentB := (RecAnalyst.CommentString <> '');
  CommentMemo.Enabled := AddCommentB;
  CommentMemo.Lines.Text := RecAnalyst.CommentString;
end;

procedure TMainForm.ResetVCL;
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount - 1 do
    PageControl.Pages[i].TabVisible := True;
  ScenarioInfoTabSheet.TabVisible := False;
  CommentTabSheet.TabVisible := False;  

  if MapBitmap.HandleAllocated then
    MapBitmap.Assign (nil);

  TeamsVCL.Clear;
  ChatWB.LoadFromString ('');
  TributesWB.LoadFromString ('');

  if UnitsBitmap.HandleAllocated then
    UnitsBitmap.Assign (nil);
  if BuildingsBitmap.HandleAllocated then
    BuildingsBitmap.Assign (nil);
  if ResearchesBitmap.HandleAllocated then
    ResearchesBitmap.Assign (nil);

  ScenarioFileLabel.Visible := False;
  JvLabel15.Visible := False;
  TeamsLabel.Top := JvLabel14.Top + 28;
  ObjectivesLabel.Visible := False;

  ImageMap.Clear;
  
  { clear values }
  GameTypeLabel.Caption := '';
  MapStyleLabel.Caption := '';
  LocationLabel.Caption := '';
  PlayersLabel.Caption := '';
  DurationLabel.Caption := '';
  DifficultyLabel.Caption := '';
  PopulationLabel.Caption := '';
  MapSizeLabel.Caption := '';
  SpeedLabel.Caption := '';
  LockDiplomacyLabel.Caption := '';
  RevealMapLabel.Caption := '';
  VictoryLabel.Caption := '';
  POVLabel.Caption := '';
  GameVersionLabel.Caption := '';
  ScenarioFileLabel.Caption := '';
  ScContentLabel.Caption := '';

  AddCommentB := False;
  CreateBackupB := True;
  CommentMemo.Text := '';
  CommentMemo.Enabled := True;
end;

procedure TMainForm.GeneralPaintBoxPaint(Sender: TObject);
var
  x, y: Integer;
begin
  { ...or Bitmap has no image assigned }
  if not Assigned (BkgBitmap) or not BkgBitmap.HandleAllocated then
    Exit;

  if not (Sender is TPaintBox) then
    Exit;

  x := 0; y := 0;
  while y < (Sender as TPaintBox).ClientHeight do
  begin
    while x < (Sender as TPaintBox).ClientWidth do
    begin
      (Sender as TPaintBox).Canvas.Draw (x, y, BkgBitmap);
      Inc (x, BkgBitmap.Width);
    end;
    Inc (y, BkgBitmap.Height);
    x := 0;
  end;
end;

procedure TMainForm.MapPaintBoxPaint(Sender: TObject);
begin
  if Assigned (MapBitmap) and MapBitmap.HandleAllocated then
    (Sender as TPaintBox).Canvas.Draw (0, 0, MapBitmap);
end;

procedure TMainForm.SaveMapAsClick(Sender: TObject);
var
  SavePictureDialog: TSavePictureDialog;
   PngObject: TPngObject;
begin
  if not Assigned (MapBitmap) or not MapBitmap.HandleAllocated then
    Exit;

  SavePictureDialog := TSavePictureDialog.Create (Self);
  try
    // SavePictureDialog.InitialDir := ExtractFilePath (RecAnalyst.FileName);
    SavePictureDialog.Filter := 'Portable Nework Graphics (*.png)|*.png|Bitmaps (*.bmp)|*.bmp';
    SavePictureDialog.DefaultExt := 'png';
    SavePictureDialog.FilterIndex := 1;
    SavePictureDialog.Options := SavePictureDialog.Options + [ofOverwritePrompt, ofPathMustExist];

    if SavePictureDialog.Execute then
    begin
      case SavePictureDialog.FilterIndex of
        1: { png }
          begin
            PngObject := TPngObject.Create;
            try
              PngObject.Assign (MapBitmap);
              PngObject.SaveToFile (SavePictureDialog.FileName);
            finally
              PngObject.Free;
            end;
          end;
        2: { bmp }
          begin
            MapBitmap.SaveToFile (SavePictureDialog.FileName);
          end;
      end;
    end;
  finally
    SavePictureDialog.Free;
  end;
end;

procedure TMainForm.BuildingsPaintBoxPaint(Sender: TObject);
begin
  if Assigned (BuildingsBitmap) and BuildingsBitmap.HandleAllocated then
    BuildingsPaintBox.Canvas.Draw (0, 0, BuildingsBitmap);
end;

procedure TMainForm.UnitsPaintBoxPaint(Sender: TObject);
begin
  if Assigned (UnitsBitmap) and UnitsBitmap.HandleAllocated then
    UnitsPaintBox.Canvas.Draw (0, 0, UnitsBitmap);
end;

procedure TMainForm.SaveasHTMLClick(Sender: TObject);
var
  HTML: TStringList;
  SaveDialog: TSaveDialog;
  HTMLText: String;
begin
  SaveDialog := TSaveDialog.Create (Self);
  try
    // SaveDialog.InitialDir := GetCurrentDir;
    SaveDialog.Filter := 'HTML Files|*.html';
    SaveDialog.DefaultExt := 'html';
    SaveDialog.FilterIndex := 1;
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt, ofPathMustExist];

    if SaveDialog.Execute then
    begin
      HTML := TStringList.Create;
      try
        ChatWB.SaveToStrings (HTML);
        try
          HTMLText := TResourceDll.LoadString (HTMLTPL_RESNAME);
        except
          // error loading resource
        end;
        HTML.Text := Format (HTMLText, [ChatWB.HostCSS, HTML.Text]);
        HTML.SaveToFile (SaveDialog.FileName);
      finally
        HTML.Free;
      end;
    end;

  finally
    SaveDialog.Free;
  end;
end;

procedure TMainForm.ResPaintBoxPaint(Sender: TObject);
begin
  if Assigned (ResearchesBitmap) and ResearchesBitmap.HandleAllocated then
    ResPaintBox.Canvas.Draw (0, 0, ResearchesBitmap);
end;

procedure TMainForm.ScBgPaintBoxPaint(Sender: TObject);
begin
  if Assigned (BkgWinBitmap) and BkgWinBitmap.HandleAllocated then
    (Sender as TPaintBox).Canvas.Draw (0, 0, BkgWinBitmap);
end;

procedure TMainForm.BtnPaintBox1Paint(Sender: TObject);
begin
  if Assigned (TabDownBitmap) and TabDownBitmap.HandleAllocated then
  begin
    case (Sender as TPaintBox).Tag of
      0: (Sender as TPaintBox).Canvas.Draw (0, 0, TabDownBitmap);
      1: (Sender as TPaintBox).Canvas.Draw (0, 0, TabUpBitmap);
    else
      (Sender as TPaintBox).Canvas.Draw (0, 0, TabUpBitmap);
    end;
  end;
end;

procedure TMainForm.BtnPaintBox1Click(Sender: TObject);
begin
  if ((Sender as TPaintBox).Tag = 0) then
  begin
    BtnPaintBox1.Tag := 0; BtnPaintBox1.Repaint;
    BtnPaintBox2.Tag := 0; BtnPaintBox2.Repaint;
    BtnPaintBox3.Tag := 0; BtnPaintBox3.Repaint;
    BtnPaintBox4.Tag := 0; BtnPaintBox4.Repaint;

    (Sender as TPaintBox).Tag := 1;
    (Sender as TPaintBox).Repaint;

    if (Sender = BtnPaintBox1) then
    begin
      HeaderLabel.Caption := 'General';
      ScenarioMapPaintBox.Visible := True;
      ScContentLabel.Top := ScenarioMapPaintBox.Top + ScenarioMapPaintBox.Height + 16;
      ScContentLabel.Caption := Format ('Global Victory: %s'#13#10, [ScAnalyst.Victory.sVictory]);
      if ScAnalyst.AllTechs then
        ScContentLabel.Caption := ScContentLabel.Caption + 'All Techs: Yes'#13#10
      else
        ScContentLabel.Caption := ScContentLabel.Caption + 'All Techs: No'#13#10;
      ScContentLabel.Caption := ScContentLabel.Caption + Format ('Original File Name: %s'#13#10, [ScAnalyst.OriginalFileName]);
      ScContentLabel.Caption := ScContentLabel.Caption + Format ('Last Updated: %s'#13#10, [DateTimeToStr (ScAnalyst.LastSave)]);
    end
    else if (Sender = BtnPaintBox2) then
    begin
      HeaderLabel.Caption := 'Instructions';
      ScContentLabel.Top := 0;
      ScContentLabel.Caption := ScAnalyst.sInstructions;
      ScenarioMapPaintBox.Visible := False;
    end
    else if (Sender = BtnPaintBox3) then
    begin
      HeaderLabel.Caption := 'Hints';
      ScContentLabel.Top := 0;
      ScContentLabel.Caption := ScAnalyst.sHints;
      ScenarioMapPaintBox.Visible := False;
    end
    else if (Sender = BtnPaintBox4) then
    begin
      HeaderLabel.Caption := 'Scouts';
      ScContentLabel.Top := 0;
      ScContentLabel.Caption := ScAnalyst.sScouts;
      ScenarioMapPaintBox.Visible := False;
    end;
    HeaderLabel.Left := 140 + (490 - 140) div 2 - (HeaderLabel.ClientWidth div 2);
  end;
end;

procedure TMainForm.JvLabel19Click(Sender: TObject);
begin
  if (Sender = JvLabel19) then
    BtnPaintBox1Click (BtnPaintBox1)
  else if (Sender = JvLabel17) then
    BtnPaintBox1Click (BtnPaintBox2)
  else if (Sender = JvLabel18) then
    BtnPaintBox1Click (BtnPaintBox3)
  else if (Sender = JvLabel20) then
    BtnPaintBox1Click (BtnPaintBox4);
end;

procedure TMainForm.PluginURLLabelClick(Sender: TObject);
begin
  ShellExecute (Handle, 'open', PChar (PLUGIN_URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.OSPaintBoxPaint(Sender: TObject);
begin
  if Assigned (OSBitmap) and OSBitmap.HandleAllocated then
    (Sender as TPaintBox).Canvas.Draw (0, 0, OSBitmap);
end;

procedure TMainForm.OSPaintBoxClick(Sender: TObject);
begin
  ShellExecute (Handle, 'open', PChar (OS_URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TrackMouse;
end;

procedure TMainForm.ResPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i, idx: Integer;
  Item: TImageMapItem;
  p: TPoint;
begin
  if not Assigned (ResearchesBitmap) or not ResearchesBitmap.HandleAllocated then
    Exit;

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

  if (idx <> -1) and Assigned (Item) then
  begin
    GetCursorPos (p);
    HintForm.Left := p.X + 8;
    HintForm.Top := p.Y + 8;
    HintForm.Text := Item.Hint;
    HintForm.IsVisible := True;
  end else
    if HintForm.IsVisible then
      HintForm.IsVisible := False;
end;

procedure TMainForm.CheckBox1PaintBoxClick(Sender: TObject);
begin
  AddCommentB := not AddCommentB;
  CommentMemo.Enabled := AddCommentB;
  CheckBox1PaintBox.Repaint;
end;

procedure TMainForm.CheckBox1PaintBoxPaint(Sender: TObject);
begin
  if AddCommentB then
  begin
    if Assigned (CheckedBitmap) and CheckedBitmap.HandleAllocated then
      (Sender as TPaintBox).Canvas.Draw (0, 0, CheckedBitmap);
  end else
  begin
    if Assigned (UncheckedBitmap) and UncheckedBitmap.HandleAllocated then
      (Sender as TPaintBox).Canvas.Draw (0, 0, UncheckedBitmap);
  end;
end;

procedure TMainForm.CheckBox2PaintBoxClick(Sender: TObject);
begin
  CreateBackupB := not CreateBackupB;
  CheckBox2PaintBox.Repaint;
end;

procedure TMainForm.CheckBox2PaintBoxPaint(Sender: TObject);
begin
  if CreateBackupB then
  begin
    if Assigned (CheckedBitmap) and CheckedBitmap.HandleAllocated then
      (Sender as TPaintBox).Canvas.Draw (0, 0, CheckedBitmap);
  end else
  begin
    if Assigned (UncheckedBitmap) and UncheckedBitmap.HandleAllocated then
      (Sender as TPaintBox).Canvas.Draw (0, 0, UncheckedBitmap);
  end;
end;

procedure TMainForm.UpdateBtnPaintBoxClick(Sender: TObject);
var
  FileName, CommentString: String;
  fh: Integer;
  fdate: Integer;
const
  BACKUP_EXT = '.backup';
begin
  CommentMemo.Text := Trim (CommentMemo.Text);
  if (Length (CommentMemo.Text) > MAX_COMMENT_LEN) then
    CommentMemo.Text := Copy (CommentMemo.Text, 1, MAX_COMMENT_LEN);

  if AddCommentB then
    CommentString := CommentMemo.Text
  else
    CommentString := '';

  if (CommentMemo.Text = '') then
  begin
    AddCommentB := False;
    CommentMemo.Enabled := AddCommentB;
    CheckBox1PaintBox.Repaint;
  end;

  if CommentString = RecAnalyst.CommentString then
    Exit;

  FileName := RecAnalyst.FileName;
  try
    fdate := 0;
    fh := FileOpen (FileName, fmOpenRead);
    if (fh > 0) then
      fdate := FileGetDate (fh);
    FileClose (fh);

    if CreatebackupB then
    begin
      if FileExists (FileName + BACKUP_EXT) then
        if MessageDlg ('Backup file already exists. Overwrite file?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          if not DeleteFile (FileName + BACKUP_EXT) then
            ShowMessage ('Unable to delete file.');

      if not RenameFile (FileName, FileName + BACKUP_EXT) then
        if MessageDlg ('Unable to create backup file. Do you wish to continue?',
            mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;
    end;

    if RecAnalyst.AddComment (CommentString) then
      RecAnalyst.Build (FileName)
    else
      ShowMessage ('Unable to update comment.');

    FileSetDate (FileName, fdate);
  except
    on E: Exception do
    begin
      Logger.SendException(E);
      ShowMessage ('Unable to update comment.');
    end;
  end;
end;

procedure TMainForm.UpdateBtnPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not UpdateDown and (ssLeft in Shift) then
  begin
    UpdateDown := True;
    UpdateBtnPaintBox.Repaint;
    UpdateLabel.Left := UpdateLabel.Left + 1;
    UpdateLabel.Top := UpdateLabel.Top + 1;
  end;
end;

procedure TMainForm.UpdateBtnPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if UpdateDown then
  begin
    UpdateDown := False;
    UpdateBtnPaintBox.Repaint;
    UpdateLabel.Left := UpdateLabel.Left - 1;
    UpdateLabel.Top := UpdateLabel.Top - 1;
  end;
end;

procedure TMainForm.UpdateBtnPaintBoxPaint(Sender: TObject);
begin
  if UpdateDown then
  begin
    if Assigned (BtnDownBitmap) and BtnDownBitmap.HandleAllocated then
      (Sender as TPaintBox).Canvas.Draw (0, 0, BtnDownBitmap);
  end else
  begin
    if Assigned (BtnUpBitmap) and BtnUpBitmap.HandleAllocated then
      (Sender as TPaintBox).Canvas.Draw (0, 0, BtnUpBitmap);
  end;
end;

end.

