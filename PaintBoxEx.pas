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
unit PaintBoxEx;

interface

uses
  Windows, Controls, Classes, Messages;

type
  { We need a PaintBox with Handle to catch WM_MOUSELEAVE and WM_MOUSEMOVE messages }
  TPaintBoxEx = class(TCustomControl)
  private
    FOnPaint: TNotifyEvent;
//    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    TrackingMouse: Boolean;
    procedure TrackMouse;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
//    property Canvas;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
//    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Canvas;
  end;

implementation

uses
  Graphics, Forms;

{ TPaintBoxEx }
constructor TPaintBoxEx.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 105;
  Height := 105;
end;

procedure TPaintBoxEx.Paint;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle (0, 0, Width, Height);
    end;
  if Assigned (FOnPaint) then
    FOnPaint (Self);
end;

procedure TPaintBoxEx.TrackMouse;
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

procedure TPaintBoxEx.WMMouseLeave(var Message: TMessage);
begin
  TrackingMouse := False;

  if Assigned (FOnMouseLeave) then
    FOnMouseLeave (Self);
end;

procedure TPaintBoxEx.WMMouseMove(var Message: TWMMouseMove);
begin
  TrackMouse;
  if Assigned (OnMouseMove) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with CalcCursorPos do
          MouseMove (KeysToShiftState (Keys), X, Y)
      else
        MouseMove (KeysToShiftState (Keys), Message.XPos, Message.YPos);
end;

end.
 