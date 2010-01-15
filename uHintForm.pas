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
unit uHintForm;

interface

uses
  Windows, Classes, Controls, StdCtrls, Forms, Messages;

type
  THintForm = class(TForm)
    HintLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FRoundRadius: Integer;
    FIsVisible: Boolean;
    FText: String;
    procedure SetText(AText: String);
    procedure SetVisibility(const AValue: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetMinMaxInfo(var Msg: TWMGETMINMAXINFO); message WM_GETMINMAXINFO;
  public
    property IsVisible: Boolean read FIsVisible write SetVisibility;
    property Text: String read FText write SetText;
  end;

var
  HintForm: THintForm;

implementation

{$R *.dfm}

uses
  SysUtils;

function IsWin9xFamily: Boolean;
begin
  Result := (Win32Platform = Ver_Platform_Win32_Windows);
end;

procedure THintForm.CreateParams(var Params: TCreateParams);
begin
  BorderStyle := bsNone;
  inherited CreateParams (Params);
  with Params do
  begin
    ExStyle := ExStyle or WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
    if not IsWin9xFamily then
      ExStyle := ExStyle or WS_EX_TRANSPARENT;
  end;
end;

{ by default there is in Windows min width of form 112 / 118 pixels }
procedure THintForm.GetMinMaxInfo(var Msg: TWMGETMINMAXINFO);
begin
  with Msg.MinMaxInfo^ do
  begin
    ptMinTrackSize.X := 0;  { min. Width }
    ptMinTrackSize.Y := 0;  { min. Height }
  end;
end;

procedure THintForm.FormCreate(Sender: TObject);
begin
  FRoundRadius := 10;
  AlphaBlendValue := 200;
  AlphaBlend := True;
  Color := $000367D5;
  FIsVisible := False;
  Resize;
end;

procedure THintForm.SetVisibility(const AValue: Boolean);
begin
  if FText = '' then
    Exit;
    
  FIsVisible := AValue;
  if FIsVisible then
    ShowWindow (Handle, SW_SHOWNOACTIVATE)
  else
    ShowWindow (Handle, SW_HIDE);
end;

procedure THintForm.FormResize(Sender: TObject);
var
  Rgn: HRGN;
begin
  Rgn := CreateRoundRectRgn (0, 0, Width - 0, Height - 0, FRoundRadius, FRoundRadius);
  SetWindowRgn (Handle, Rgn, True);
end;

procedure THintForm.SetText(AText: String);
var
  Lines: TStringList;
  i, max: Integer;
begin
  Lines := TStringList.Create;
  FText := AText;
  try
    try
      Lines.Text := FText;
      max := 0;
      for i := 0 to Lines.Count - 1 do
        if (HintLabel.Canvas.TextWidth (Lines[i]) > max) then
          max := HintLabel.Canvas.TextWidth (Lines[i]);
      HintLabel.Caption := FText;
      ClientWidth := max + 2 * HintLabel.Left;
      ClientHeight := HintLabel.Canvas.TextHeight (FText) * Lines.Count + 2 * HintLabel.Top;
    except
      FText := '';
    end;
  finally
    Lines.Free;
  end;
end;

end.
