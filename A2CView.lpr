{*
 * $Id$
 * This file is part of the A2CView project.
 *
 * Copyright (c) 2009-2013 biegleux <biegleux[at]gmail[dot]com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, see <http://www.gnu.org/licenses>.
 *}
library A2CView;

{$mode objfpc}{$H+}

// http://bugs.freepascal.org/view.php?id=23184
//{$IFDEF WIN32}
//{$LIBSUFFIX '32'}
//{$ENDIF}

//{$IFDEF WIN64}
//{$LIBSUFFIX '64'}
//{$ENDIF}

{$IFNDEF FPC}
  {$E wdx}
{$ENDIF}

uses
  Windows, contplug, AoE2Data;

const
  _DetectString: PAnsiChar = 'EXT="MGX" | EXT="MGL" | EXT="MGZ"';

  _FieldsNum = 9;
  _Fields: array[0.._FieldsNum - 1] of PAnsiChar = (
    'Game Type',
    'Map Style',
    'Map',
    'Duration',
    'Players',
    'POV',
    'Version',
    '# of players',
    'PlayersEx'
  );

  _FieldTypes: array[0.._FieldsNum - 1] of Integer = (
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW,
    FT_STRINGW
  );

  _FieldUnits: array[0.._FieldsNum - 1] of PAnsiChar = (
    '', '', '', '', '', '', '', '', ''
  );

procedure StrLCpyA(p, p2: PAnsiChar; MaxLen: Integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynA(p, p2, MaxLen);
end;

procedure StrLCpyW(p, p2: PWideChar; MaxLen: Integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynW(p, p2, MaxLen);
end;

procedure ContentGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrLCpyA(DetectString, _DetectString, MaxLen);
end;

function ContentGetSupportedField(FieldIndex: Integer; FieldName, Units: PAnsiChar;
  MaxLen: Integer): Integer; stdcall;
begin
  if (FieldIndex < 0) or (FieldIndex >= _FieldsNum) then
  begin
    Result:= FT_NOMOREFIELDS;
    Exit
  end;

  StrLCpyA(FieldName, _Fields[FieldIndex], MaxLen);
  StrLCpyA(Units, _FieldUnits[FieldIndex], MaxLen);
  Result := _FieldTypes[FieldIndex];
end;

function ContentGetValue(FileName: PAnsiChar; FieldIndex, UnitIndex: Integer;
  FieldValue: PByte; MaxLen, Flags: Integer): Integer; stdcall;
begin
  Result:= FT_FIELDEMPTY;
end;

function ContentGetValueW(FileName: PWideChar; FieldIndex, UnitIndex: Integer;
  FieldValue: PByte; MaxLen, Flags: Integer): Integer; stdcall;
var
  s: String;
begin
  if (Flags and CONTENT_DELAYIFSLOW) > 0 then
  begin
    Result:= FT_DELAYED;
    Exit;
  end;

  //Text field
  if (FieldIndex = Pred(_FieldsNum)) then
  begin
    if (UnitIndex = -1) then
    begin
      fPlayerList.Clear();
      Result := FT_FIELDEMPTY;
      Exit;
    end;

    if (UnitIndex = 0) then
      if not GetData(FileName) then
      begin
        Result := FT_FILEERROR;
        Exit;
      end;

    s := Copy(fPlayerList.Text, UnitIndex + 1, MaxInt);
    if (s <> '') then
    begin
      StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(s)), MaxLen div 2);
      Result := FT_FULLTEXT;
    end else
      Result := FT_FIELDEMPTY;
    Exit;
  end;

  //ordinary fields
  if (FieldIndex < 0) or (FieldIndex >= _FieldsNum) then
  begin
    Result := FT_NOSUCHFIELD;
    Exit;
  end;

  if not GetData(FileName) then
  begin
    Result := FT_FILEERROR;
    Exit;
  end;

  Result := _FieldTypes[FieldIndex];

  {
    'Game Type',
    'Map Style',
    'Map',
    'Duration',
    'Players',
    'POV',
    'Version',
    '# of Players'
  }

  case FieldIndex of
    0: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fGameType)), MaxLen div 2);
    1: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fMapStyle)), MaxLen div 2);
    2: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fMap)), MaxLen div 2);
    3: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fDuration)), MaxLen div 2);
    4: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fPlayers)), MaxLen div 2);
    5: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fPOV)), MaxLen div 2);
    6: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fVersion)), MaxLen div 2);
    7: StrLCpyW(PWideChar(FieldValue), PWideChar(WideString(fPlayerCount)), MaxLen div 2);
  end;

  if (Result = FT_STRINGW) and (PWideChar(FieldValue)[0] = #0) then
    Result := FT_FIELDEMPTY;
end;

exports
  ContentGetDetectString,
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW;
end.

