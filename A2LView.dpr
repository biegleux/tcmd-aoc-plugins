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
library A2LView;

uses
  SysUtils,
  Windows,
  uMainForm in 'uMainForm.pas' {MainForm};

{$IFNDEF FPC}
  {$E wlx}
{$ENDIF}        

const
  _DetectString: PChar = 'EXT="MGX" | EXT="MGL" | EXT="SCX" | EXT="SCN"';

procedure ListGetDetectString(DetectString: PChar; MaxLen: Integer); stdcall;
begin
  StrLCopy (DetectString, _DetectString, MaxLen);
end;

function ListLoad(ListerWin: HWND; FileToLoad: PChar; ShowFlags: Integer): HWND; stdcall;
begin
  Result := ShowAoE2 (ListerWin, FileToLoad, ShowFlags);
end;

procedure ListCloseWindow(PluginWin: HWND); stdcall;
begin
  HideAoE2 (PluginWin);
end;

exports
  ListGetDetectString,
  ListLoad,
  ListCloseWindow;

end.
 
