{*
 * $Id$
 * This file is part of the A2CView project.
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
unit contplug;

{$mode objfpc}{$H+}

interface

uses
  Windows;

const
  FT_NOMOREFIELDS     = 0;
  FT_NUMERIC_32       = 1;
  FT_NUMERIC_64       = 2;
  FT_NUMERIC_FLOATING = 3;
  FT_DATE             = 4;
  FT_TIME             = 5;
  FT_BOOLEAN          = 6;
  FT_MULTIPLECHOICE   = 7;
  FT_STRING           = 8;
  FT_FULLTEXT         = 9;
  FT_DATETIME         = 10;
  FT_STRINGW          = 11;
  FT_COMPARECONTENT   = 100;

// for ContentGetValue
  FT_NOSUCHFIELD  = -1;
  FT_FILEERROR    = -2;
  FT_FIELDEMPTY   = -3;
  FT_ONDEMAND     = -4;
  FT_NOTSUPPORTED = -5;
  FT_SETCANCEL    = -6;
  FT_DELAYED      = 0;

// for ContentSetValue
  FT_SETSUCCESS = 0;     // setting of the attribute succeeded

// for ContentGetSupportedFieldFlags
  CONTFLAGS_EDIT                   = 1;
  CONTFLAGS_SUBSTSIZE              = 2;
  CONTFLAGS_SUBSTDATETIME          = 4;
  CONTFLAGS_SUBSTDATE              = 6;
  CONTFLAGS_SUBSTTIME              = 8;
  CONTFLAGS_SUBSTATTRIBUTES        = 10;
  CONTFLAGS_SUBSTATTRIBUTESTR      = 12;
  CONTFLAGS_PASSTHROUGH_SIZE_FLOAT = 14;
  CONTFLAGS_SUBSTMASK              = 14;
  CONTFLAGS_FIELDEDIT              = 16;

// for ContentSendStateInformation
  CONTST_READNEWDIR        = 1;
  CONTST_REFRESHPRESSED    = 2;
  CONTST_SHOWHINT          = 4;
  SETFLAGS_FIRST_ATTRIBUTE = 1;  // First attribute of this file
  SETFLAGS_LAST_ATTRIBUTE  = 2;  // Last attribute of this file
  SETFLAGS_ONLY_DATE       = 4;  // Only set the date of the datetime value!
  CONTENT_DELAYIFSLOW      = 1;  // ContentGetValue called in foreground
  CONTENT_PASSTHROUGH      = 2;  // If requested via contflags_passthrough_size_float: The size
                                 // is passed in as floating value, TC expects correct value
                                 // from the given units value, and optionally a text string

type
  PContentDefaultParamStruct = ^TContentDefaultParamStruct;
  TContentDefaultParamStruct = record
    Size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: LongInt;
    DefaultIniName: array[0..MAX_PATH - 1] of Char;
  end;

type
  PDateFormat = ^TDateFormat;
  TDateFormat = record
    wYear,
    wMonth,
    wDay: Word;
  end;

type
  PTimeFormat = ^TTimeFormat;
  TTimeFormat = record
    wHour,
    wMinute,
    wSecond: Word;
  end;

  PFileDetailsStruct = ^TFileDetailsStruct;
  TFileDetailsStruct = record
    FileSize1Lo,
    FileSize1Hi: DWORD;
    FileSize2Lo,
    FileSize2Hi: DWORD;
    FileTime1: TFileTime;
    FileTime2: TFileTime;
    Attr1,
    Attr2: DWORD;
  end;

type
  TProgressCallbackProc = function(NextBlockData: Integer): Integer; stdcall;

implementation

{ Function prototypes: }

{
procedure ContentGetDetectString(DetectString: PAnsiChar; MaxLen: integer); stdcall;
function ContentGetSupportedField(FieldIndex: integer; FieldName, Units: PAnsiChar;
  MaxLen: integer): integer; stdcall;
function ContentGetValue(FileName: PAnsiChar; FieldIndex, UnitIndex: integer; FieldValue: PByte;
  MaxLen, Flags: integer): integer; stdcall;
function ContentGetValueW(FileName: PWideChar; FieldIndex, UnitIndex: integer; FieldValue: PByte;
  MaxLen, Flags: integer): integer; stdcall;
procedure ContentSetDefaultParams(Dps: PContentDefaultParamStruct); stdcall;
procedure ContentPluginUnloading; stdcall;
procedure ContentStopGetValue(FileName: PAnsiChar); stdcall;
procedure ContentStopGetValueW(FileName: PWideChar); stdcall;
function ContentGetDefaultSortOrder(FieldIndex: integer): integer; stdcall;
function ContentGetSupportedFieldFlags(FieldIndex: integer): integer; stdcall;
function ContentSetValue(FileName: PAnsiChar; FieldIndex, UnitIndex, FieldType: integer;
  FieldValue: PByte; Flags: integer): integer; stdcall;
function ContentSetValueW(FileName: PWideChar; FieldIndex, UnitIndex, FieldType: integer;
  FieldValue: PByte; Flags: integer): integer; stdcall;
procedure ContentSendStateInformation(State: integer; Path: PAnsiChar); stdcall;
procedure ContentSendStateInformationW(State: integer; Path: PWideChar); stdcall;
function ContentEditValue(Handle: THandle; FieldIndex, UnitIndex, FieldType: integer;
  FieldValue: PAnsiChar; MaxLen: integer; Flags: integer; LangIdentifier: PAnsiChar): integer; stdcall;
function ContentCompareFiles(ProgressCallback: TProgressCallbackProc; CompareIndex: integer;
  FileName1, FileName2: PAnsiChar; FileDetails: PFileDetailsStruct): integer; stdcall;
function ContentCompareFilesW(ProgressCallback: TProgressCallbackProc; CompareIndex: integer;
  FileName1, FileName2: PWideChar; FileDetails: PFileDetailsStruct): integer; stdcall;
}

end.

