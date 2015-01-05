{*
 * Interface for RecAnalyst Library

 * Copyright (c) 2009-2013 biegleux <biegleux[at]gmail[dot]com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses>.
 *}
unit recanalystd; { d as dynamic linking }

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Windows;

type
  PRecAnalyst = Pointer;

  PInitialStateStruct = ^TInitialStateStruct;
  TInitialStateStruct = record
    dwFood: DWORD;
    dwWood: DWORD;
    dwStone: DWORD;
    dwGold: DWORD;
    iStartingAge: Integer;
    dwHouseCapacity: DWORD;
    dwPopulation: DWORD;
    dwCivilianPop: DWORD;
    dwMilitaryPop: DWORD;
    dwExtraPop: DWORD;
    ptPosition: TPoint;
    szStartingAge: array[0..MAXBYTE] of AnsiChar;
  end;

  PPlayerStruct = ^TPlayerStruct;
  TPlayerStruct = record
    szName: array[0..MAXBYTE] of AnsiChar;
    dwIndex: DWORD;
    bHuman: BOOL;
    dwTeam: DWORD;
    bOwner: BOOL;
    szCivilization: array[0..MAXBYTE] of AnsiChar;
    dwCivId: DWORD;
    dwColorId: DWORD;
    bIsCooping: BOOL;
    dwFeudalTime: DWORD;
    dwCastleTime: DWORD;
    dwImperialTime: DWORD;
    dwResignTime: DWORD;
    dwDisconnectTime: DWORD;
    lpInitialState: PInitialStateStruct; // pointer to InitialState struct data
  end;

  PVictoryStruct = ^TVictoryStruct;
  TVictoryStruct = record
    dwTimeLimit: DWORD;
    dwScoreLimit: DWORD;
    dwVictoryCondition: DWORD;
    szVictory: array[0..MAXBYTE] of AnsiChar;
  end;

  PGameSettingsStruct = ^TGameSettingsStruct;
  TGameSettingsStruct = record
    dwGameType: DWORD;
    dwMapStyle: DWORD;
    dwDifficultyLevel: DWORD;
    dwGameSpeed: DWORD;
    dwRevealMap: DWORD;
    dwMapSize: DWORD;
    bIsScenario: BOOL;
    dwPlayers: DWORD;
    dwPOV: DWORD;
    dwMapId: DWORD;
    dwPopLimit: DWORD;
    bLockDiplomacy: BOOL;
    dwPlayTime: DWORD;
    bInGameCoop: BOOL;
    bIsFFA: BOOL;
    dwVersion: DWORD;
    szMap: array[0..MAXWORD] of AnsiChar;
    szPlayersType: array[0..MAXCHAR] of AnsiChar;
    szPOV: array[0..MAXBYTE] of AnsiChar;
//    szPOVEx
    szGameType: array[0..MAXBYTE] of AnsiChar;
    szMapStyle: array[0..MAXBYTE] of AnsiChar;
    szDifficultyLevel: array[0..MAXBYTE] of AnsiChar;
    szGameSpeed: array[0..MAXBYTE] of AnsiChar;
    szRevealMap: array[0..MAXBYTE] of AnsiChar;
    szMapSize: array[0..MAXBYTE] of AnsiChar;
    szVersion: array[0..MAXBYTE] of AnsiChar;
    szScFileName: array[0..MAXWORD] of AnsiChar;
    lpVictory: PVictoryStruct;  // pointer to TVictoryStruct structure
  end;

  PChatMessageStruct = ^TChatMessageStruct;
  TChatMessageStruct = record
    dwTime: DWORD;
    dwPlayerId: DWORD;
    szMessage: array[0..MAXBYTE] of AnsiChar;
  end;

  PTributeStruct = ^TTributeStruct;
  TTributeStruct = record
    dwTime: DWORD;
    dwPlayerFrom: DWORD;
    dwPlayerTo: DWORD;
    byResourceId: Byte;
    dwAmount: DWORD;
    fFee: Single;
  end;

  PResearchStruct = ^TResearchStruct;
  TResearchStruct = record
    dwTime: DWORD;
    dwId: DWORD;
    dwPlayerId: DWORD;
    szName: array[0..MAXBYTE] of AnsiChar;
  end;

  EnumPlayersProc = function(lpPlayer: PPlayerStruct; lParam: LPARAM): BOOL; stdcall;
  EnumChatMessagesProc = function(lpChatMessage: PChatMessageStruct; lParam: LPARAM): BOOL; stdcall;
  EnumTributesProc = function(lpTribute: PTributeStruct; lParam: LPARAM): BOOL; stdcall;
  EnumResearchesProc = function(lpResearch: PResearchStruct; lParam: LPARAM): BOOL; stdcall;

const
  { Return codes }
  RECANALYST_OK          = 0;
  { analyze error codes }
  RECANALYST_NOFILE      = -1;
  RECANALYST_FILEEXT     = -2;
  RECANALYST_EMPTYHEADER = -3;
  RECANALYST_DECOMP      = -4;
  RECANALYST_FILEREAD    = -5;
  RECANALYST_FILEOPEN    = -6;
  RECANALYST_UNKNOWN     = -7;
  RECANALYST_HEADLENREAD = -8;
  RECANALYST_NOTRIGG     = -9;
  RECANALYST_NOGAMESETS  = -10;
  RECANALYST_READPLAYER  = -11;
  { general error codes }
  RECANALYST_INVALIDPTR  = -12;
  RECANALYST_FREEOBJ     = -13;
  RECANALYST_NOCALLBACK  = -14;
  RECANALYST_ENUMP       = -15;
  RECANALYST_ANALYZEF    = -16;
  RECANALYST_NOTANALYZED = -17;
  RECANALYST_TIMECONV    = -18;
  RECANALYST_OBJECTIVES  = -19;
  RECANALYST_ENUMPRECHAT = -20;
  RECANALYST_ENUMINCHAT  = -21;
  RECANALYST_ENUMT       = -22;
  RECANALYST_ENUMR       = -23;
  RECANALYST_GAMESETTS   = -24;
  RECANALYST_GENMAP      = -25;
  RECANALYST_ANLTIME     = -26;

const
  RecAnalystDLL = {$IFDEF WIN64}'recanalyst64.dll'{$ELSE}'recanalyst32.dll'{$ENDIF};

var
  recanalyst_create: function(): PRecAnalyst; stdcall;
  recanalyst_free: function(lpRecAnalyst: PRecAnalyst): Integer; stdcall;
  recanalyst_analyze: function(lpRecAnalyst: PRecAnalyst; lpFileName: PAnsiChar): Integer; stdcall;
  recanalyst_getgamesettings: function(lpRecAnalyst: PRecAnalyst; lpGameSettings: PGameSettingsStruct): Integer; stdcall;
  recanalyst_enumplayers: function(lpRecAnalyst: PRecAnalyst; lpEnumFunc: EnumPlayersProc; lParam: LPARAM): Integer; stdcall;
  recanalyst_getobjectives: function(lpRecAnalyst: PRecAnalyst; lpObjectives: PAnsiChar): Integer; stdcall;
  recanalyst_enumpregamechat: function(lpRecAnalyst: PRecAnalyst; lpEnumFunc: EnumChatMessagesProc; lParam: LPARAM): Integer; stdcall;
  recanalyst_enumingamechat: function(lpRecAnalyst: PRecAnalyst; lpEnumFunc: EnumChatMessagesProc; lParam: LPARAM): Integer; stdcall;
  recanalyst_enumtributes: function(lpRecAnalyst: PRecAnalyst; lpEnumFunc: EnumTributesProc; lParam: LPARAM): Integer; stdcall;
  recanalyst_enumresearches: function(lpRecAnalyst: PRecAnalyst; lpEnumFunc: EnumResearchesProc; lParam: LPARAM): Integer; stdcall;
  recanalyst_generatemap: function(lpRecAnalyst: PRecAnalyst; dwWidth, dwHeight: DWORD; lpImageBuffer: Pointer): Integer; stdcall;
  recanalyst_analyzetime: function(lpRecAnalyst: PRecAnalyst): Integer; stdcall;
  recanalyst_timetostring: function(dwTime: DWORD; lpTime: PAnsiChar): Integer; stdcall;
  recanalyst_errmsg: function(iErrCode: Integer): PAnsiChar; stdcall;
  recanalyst_libversion: function(): PAnsiChar; stdcall;

function RecAnalystLoadLib(const LibName: String): Boolean;

var
  RecAnalystLoadLoaded: Boolean = False;

implementation

uses
  SysUtils;

var
  RecAnalystLibHandle: THandle = 0;

procedure ClearPointers(); forward;

function RecAnalystLoadLib(const LibName: String): Boolean;

  function GetProcAddr(lpProcName: PAnsiChar): Pointer;
  begin
    Result := GetProcAddress(RecAnalystLibHandle, lpProcName);

    if not Assigned(Result) then
      raise Exception.CreateFmt('The function %s was not found in the current installed RecAnalyst library',
                          [lpProcName]);
  end;

begin
  Result := RecAnalystLoadLoaded;
  if Result then Exit;

  RecAnalystLibHandle := LoadLibrary(PChar(LibName));

  if (RecAnalystLibHandle <> 0) then
  begin
    try
      @recanalyst_create := GetProcAddr('recanalyst_create');
      @recanalyst_free := GetProcAddr('recanalyst_free');
      @recanalyst_analyze := GetProcAddr('recanalyst_analyze');
      @recanalyst_getgamesettings := GetProcAddr('recanalyst_getgamesettings');
      @recanalyst_enumplayers := GetProcAddr('recanalyst_enumplayers');
      @recanalyst_getobjectives := GetProcAddr('recanalyst_getobjectives');
      @recanalyst_enumpregamechat := GetProcAddr('recanalyst_enumpregamechat');
      @recanalyst_enumingamechat := GetProcAddr('recanalyst_enumingamechat');
      @recanalyst_enumtributes := GetProcAddr('recanalyst_enumtributes');
      @recanalyst_enumresearches := GetProcAddr('recanalyst_enumresearches');
      @recanalyst_generatemap := GetProcAddr('recanalyst_generatemap');
      @recanalyst_analyzetime := GetProcAddr('recanalyst_analyzetime');
      @recanalyst_timetostring := GetProcAddr('recanalyst_timetostring');
      @recanalyst_errmsg := GetProcAddr('recanalyst_errmsg');
      @recanalyst_libversion := GetProcAddr('recanalyst_libversion');
      Result := True;
    except
      Result := False;
      FreeLibrary(RecAnalystLibHandle);
      RecAnalystLibHandle := 0;
      ClearPointers();
    end;
  end;
end;

procedure ClearPointers();
begin
  recanalyst_create := nil;
  recanalyst_free := nil;
  recanalyst_analyze := nil;
  recanalyst_getgamesettings := nil;
  recanalyst_enumplayers := nil;
  recanalyst_getobjectives := nil;
  recanalyst_enumpregamechat := nil;
  recanalyst_enumingamechat := nil;
  recanalyst_enumtributes := nil;
  recanalyst_enumresearches := nil;
  recanalyst_generatemap := nil;
  recanalyst_analyzetime := nil;
  recanalyst_timetostring := nil;
  recanalyst_errmsg := nil;
  recanalyst_libversion := nil;
end;

initialization
  RecAnalystLoadLoaded := RecAnalystLoadLib(IncludeTrailingPathDelimiter
          (ExtractFilePath(GetModuleName(HInstance))) + RecAnalystDLL);

finalization
  if (RecAnalystLibHandle <> 0) then
    FreeLibrary(RecAnalystLibHandle);

end.

