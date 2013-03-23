{*
 * $Id$
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
library A2LView;

{$IFNDEF FPC}
  {$IFDEF WIN64}
    {$E wlx64}
  {$ELSE}
    {$E wlx}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  SysUtils,
  Windows,
  uMainForm in 'uMainForm.pas' {MainForm};

const
  _DetectString: PAnsiChar = 'EXT="MGX" | EXT="MGL" | EXT="MGZ"';

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrLCopy(DetectString, _DetectString, MaxLen);
end;

function ListLoad(ListerWin: HWND; FileToLoad: PAnsiChar; ShowFlags: Integer): HWND; stdcall;
begin
  Result := ShowAoE2(ListerWin, String(AnsiString(FileToLoad)), ShowFlags);
end;

function ListLoadW(ListerWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): HWND; stdcall;
begin
  Result := ShowAoE2(ListerWin, WideString(FileToLoad), ShowFlags);
end;

procedure ListCloseWindow(PluginWin: HWND); stdcall;
begin
  HideAoE2(PluginWin);
end;

exports
  ListGetDetectString,
  ListLoad,
  ListLoadW,
  ListCloseWindow;

end.

