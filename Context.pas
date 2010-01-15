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
unit Context;

interface

uses
  Windows, Graphics;

//function ArrayMax(const Arr: array of Integer): Integer;
//function ArraySum(const Arr: array of Integer): Integer;
function InArray(const Ary: array of Integer; Value: Integer): Integer;
function FindCharEx(const Ch: AnsiChar; const S: String; Offset: Cardinal = 1): Integer;
function FindChar(const Ch: AnsiChar; const S: String): Integer;
function HTMLSpecialChars(const AHTML: String): String;

type
  TResourceDll = class
  private
  public
    class function Load(const LibName: String): Boolean;
    class procedure LoadFont;
    class procedure UnloadFont;
    class function LoadBitmap(const ResourceName: String; var Bitmap: TBitmap): Boolean;
    class function LoadCSS: Boolean;
    class function LoadString(const ResourceName: String): String;
  end;

var
  DllHandle: THandle;
  DllLoaded: Boolean = False;

const
  FONT_RESNAME = 'LBLACK';
  AGES_RESNAME = 'AGES';
  BACKGROUND_RESNAME = 'BACKGROUND';
  CSSBACKGROUND_RESNAME = 'CSSBACKGROUND';
  CSSSTYLE_RESNAME = 'CSSSTYLE';
  CSS_FILE = 'style.css';
  FONT_FILE = 'lblack.ttf';
  CSS_BACKGROUND_FILE = 'background.png';
  ICONS_RESNAME = 'ICONS';
  HTMLTPL_RESNAME = 'HTMLTEMPLATE';
  BGWIN_RESNAME = 'BGWIN';
  TABUP_RESNAME = 'TABUP';
  TABDOWN_RESNAME = 'TABDOWN';
  OPENSRC_RESNAME = 'OPENSRC';
  BTNDOWN_RESNAME = 'BTNDOWN';
  BTNUP_RESNAME = 'BTNUP';
  CHECKED_RESNAME = 'CHECKED';
  UNCHECKED_RESNAME = 'UNCHECKED';

implementation

uses
  Classes, SysUtils, Forms{$IFNDEF FPC}, PNGImage{$ENDIF}, Messages, uLogger;

var
  hFont: THandle = 0; { handle to the font added }

const
  ResourcesDLL = 'resources.dll';
  FONT_NAME = 'Lucida Blackletter';

function IsVista: Boolean;
var
  pFunction: Pointer;
begin
  { this api export is present only in vista and above }
  pFunction := GetProcAddress (GetModuleHandle ('kernel32.dll'), 'GetProductInfo');
  Result := Assigned (pFunction);
end;

function ArrayMax(const Arr: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low (Arr) to High (Arr) do
    if (Result < Arr[i]) then
      Result := Arr[i];
end;

function ArraySum(const Arr: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low (Arr) to High (Arr) do
    Inc (Result, Arr[i]);
end;

function InArray(const Ary: array of Integer; Value: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Ary) to High(Ary) do
    if (Ary[i] = Value) then
    begin
      Result := i;
      Break;
    end;
end;

function FindCharEx(const Ch: AnsiChar; const S: String; Offset: Cardinal = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Offset to Length (S) do
    if (S[i] = Ch) then
    begin
      Result := i;
      Exit;
    end;
end;

function FindChar(const Ch: AnsiChar; const S: String): Integer;
begin
  Result := FindCharEx (Ch, S);
end;

function HTMLSpecialChars(const AHTML: String): String;
begin
  Result := StringReplace (StringReplace (StringReplace (AHTML, '&', '&amp;', [rfReplaceAll]), '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
end;

class function TResourceDll.Load(const LibName: String): Boolean;
begin
  Result := DllLoaded;
  if Result then
    Exit;
  {$IFDEF FPC}
  DllHandle := LoadLibrary (LibName);
  {$ELSE}
  DllHandle := LoadLibrary (PChar (LibName));
  {$ENDIF}
  Result := (Dllhandle <> 0);
end;

{ Adds a font resource from resources to the system }
class procedure TResourceDll.LoadFont;
var
  Res: TResourceStream;
  ResFontCount: DWORD;
  FontBuffer: PChar;
  ResSize: Integer;
  sPath: String;
  FileStream: TFileStream;
begin
  if not DllLoaded or (Screen.Fonts.IndexOf (FONT_NAME) <> -1) then
    Exit;

  if IsVista then
  begin
    sPath := IncludeTrailingPathDelimiter (ExtractFilePath (GetModuleName (HInstance)));
    if not FileExists (sPath + FONT_FILE) then
      try
        Res := TResourceStream.Create (DllHandle, FONT_RESNAME, RT_RCDATA);
        try
          FileStream := TFileStream.Create (sPath + FONT_FILE, fmCreate);
          try
            FileStream.CopyFrom (Res, 0);
          finally
            FileStream.Free;
          end;
        finally
          Res.Free;
        end;
      except
        on E: Exception do
          Logger.SendException (E);
      end;

    if (AddFontResource (PChar (sPath + FONT_FILE)) <> 0) then
      PostMessage (HWND_BROADCAST, WM_FONTCHANGE, 0,0);
  end else
  begin
    try
      Res := TResourceStream.Create (DllHandle, FONT_RESNAME, RT_RCDATA);
      try
        ResSize := Res.Size + 1;
        FontBuffer  := AllocMem (ResSize);
        Res.Read (FontBuffer^, ResSize);
      finally
        Res.Free;
      end;
      ResFontCount := 1;
      { doesn't work in vista }
      hFont := AddFontMemResourceEx (FontBuffer, ResSize, nil, @ResFontCount);
      FreeMem (FontBuffer, ResSize);
    except
      on E: Exception do
        Logger.SendException (E);
    end;
  end;
end;

{ Removes added font }
class procedure TResourceDll.UnloadFont;
var
  sPath: String;
begin
  if IsVista then
  begin
    sPath := IncludeTrailingPathDelimiter (ExtractFilePath (GetModuleName (HInstance)));
    if RemoveFontResource (PChar (sPath + FONT_FILE)) then
      PostMessage (HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  end else
    if (hFont <> 0) then
      RemoveFontMemResourceEx (hFont);
end;

{ Loads png image resource as bitmap }
class function TResourceDll.LoadBitmap(const ResourceName: String; var Bitmap: TBitmap): Boolean;
var
  PngObject: TPngObject;
begin
  Result := False;
  if not DllLoaded {or not Assigned (Bitmap)} then
    Exit;

  PngObject := {$IFDEF FPC}TPortableNetworkGraphic{$ELSE}TPngObject{$ENDIF}.Create;
  try
    PngObject.LoadFromResourceName (DllHandle, ResourceName);
    Bitmap.Assign (PngObject);
    Result := True;
  finally
    PngObject.Free;
  end;
end;

class function TResourceDll.LoadCSS: Boolean;
var
  PngObject: TPngObject;
  Res: TResourceStream;
  sPath: String;
  CSS: TStringList;
begin
  Result := False;
  if not DllLoaded then
    Exit;

  sPath := IncludeTrailingPathDelimiter (ExtractFilePath (GetModuleName (HInstance)));

  if FileExists (sPath + CSS_FILE) and FileExists (sPath + CSS_BACKGROUND_FILE) then
    Exit;

  try
    Res := TResourceStream.Create (DllHandle, CSSSTYLE_RESNAME, RT_RCDATA);
    CSS := TStringList.Create;
    PngObject := {$IFDEF FPC}TPortableNetworkGraphic{$ELSE}TPngObject{$ENDIF}.Create;
    try
      CSS.LoadFromStream (Res);
      CSS.Text := Format (CSS.Text, [sPath + CSS_BACKGROUND_FILE]);
      CSS.SaveToFile (sPath + CSS_FILE);
      PngObject.LoadFromResourceName (DllHandle, CSSBACKGROUND_RESNAME);
      PngObject.SaveToFile (sPath + CSS_BACKGROUND_FILE);
      Result := True;
    finally
      Res.Free;
      CSS.Free;
      PngObject.Free;
    end;
  except
    on E: Exception do
      Logger.SendException (E);
  end;
end;

class function TResourceDll.LoadString(const ResourceName: String): String;
var
  Res: TResourceStream;
  SL: TStringList;
begin
  Result := '';
  if not DllLoaded then
    Exit;

  Res := TResourceStream.Create (DllHandle, ResourceName, RT_RCDATA);  
  SL := TStringList.Create;
  try
    Res := TResourceStream.Create (DllHandle, ResourceName, RT_RCDATA);
    SL.LoadFromStream (Res);
    Result := SL.Text;
  finally
    Res.Free;
    SL.Free;
  end;
end;

initialization
   DllLoaded := TResourceDll.Load (IncludeTrailingPathDelimiter
          (ExtractFilePath (GetModuleName (HInstance))) + ResourcesDLL);

  { load lb font if not present, call before any vcl component (label) has been created }
  TResourceDll.LoadFont;
  TResourceDll.LoadCSS;

finalization
  if (DllHandle <> 0) then
    {$IFNDEF FPC}FreeLibrary (DllHandle){$ELSE}UnloadLibrary (DllHandle){$ENDIF};

  { unload our font if it was loaded }
  TResourceDll.UnloadFont;
end.
