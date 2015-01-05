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
unit Context;

interface

var
  DllHandle: THandle = 0;
  DllLoaded: Boolean = False;

procedure InitializeGdiplus();
procedure FinalizeGdiplus();

implementation

uses
  Windows, SysUtils, GDIPAPI, Forms;

const
  ResourcesDLL = 'resources.dll';

var
  gdiplusToken: ULONG;

function LoadResourcesDll(const LibName: String): Boolean;
begin
  Result := DllLoaded;
  if Result then Exit;
  DllHandle := LoadLibraryEx(PChar(LibName), 0, LOAD_LIBRARY_AS_DATAFILE);
  Result := (Dllhandle <> 0);
end;

procedure InitializeGdiplus();
var
  StartupInput: TGDIPlusStartupInput;
begin
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure FinalizeGdiplus();
begin
  // Close GDI +
  GdiplusShutdown(gdiplusToken);
end;

initialization
  DllLoaded := LoadResourcesDll(IncludeTrailingPathDelimiter(
      ExtractFilePath(GetModuleName(HInstance))) + ResourcesDLL);

finalization
  if (DllHandle <> 0) then
    FreeLibrary(DllHandle);
end.

