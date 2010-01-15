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
unit TransparentScrollBox;

interface

uses
  Windows, Forms, Messages, Controls, Classes, Graphics;

type
  TTransparentScrollBox = class(TScrollBox)
  private
    FBitmap: TBitmap;
    FOwnBitmap: Boolean;
    procedure SetBitmap(const ABitmap: TBitmap);
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property OwnBitmap: Boolean read FOwnBitmap write FOwnBitmap;
  end;

implementation

procedure TTransparentScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams (Params);
  SetWindowLong (Parent.Handle, GWL_STYLE, GetWindowLong (Parent.Handle, GWL_STYLE) and not WS_CLIPCHILDREN);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TTransparentScrollBox.SetBitmap(const ABitmap: TBitmap);
begin
  FBitmap := ABitmap;
end;

procedure TTransparentScrollBox.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_ERASEBKGND) and Assigned (FBitmap) then
  begin
    Msg.Result := 1;
    BitBlt (Msg.WParam, 0, 0, ClientWidth, ClientHeight, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end else
  if Visible then
  begin
    if (Msg.Msg = WM_HSCROLL) and ((Lo (Msg.wParam) <> SB_THUMBTRACK) or (HorzScrollBar.Tracking)) then
      Invalidate
    else
    if (Msg.Msg = WM_VSCROLL) and ((Lo (Msg.wParam) <> SB_THUMBTRACK) or (VertScrollBar.Tracking)) then
      Invalidate;
    inherited WndProc (Msg);
  end else
    inherited WndProc (Msg);
end;

constructor TTransparentScrollBox.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FOwnBitmap := False;
end;

destructor TTransparentScrollBox.Destroy;
begin
  if FOwnBitmap and Assigned (FBitmap) then
    FBitmap.Free;
  inherited Destroy;
end;

end.
