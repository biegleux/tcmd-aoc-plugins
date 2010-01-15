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
unit ScAnalyst;

interface

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

uses
  SysUtils, Classes, Windows, Graphics;

type
  TVictoryMode = (vmStandard, vmConquest, vmScoreLimit, vmTimeLimit, vmCustom);
  TScenarioVersion = (sv1_18, sv1_20, sv1_21, sv1_22);

  TScenarioVictory = class(TObject)
  private
    function GetVictoryString: String;
  public
    Mode: TVictoryMode;
    All: Boolean; { for vmCustom }
    Conquest: Boolean;  { for vmCustom }
    Relics: Integer;  { for vmCustom }
    Exploration: Integer;  { for vmCustom }
    ScoreLimit: Integer;
    TimeLimit: Integer;
    property sVictory: String read GetVictoryString;
  end;

  EScAnalystException = class(Exception);

  TScAnalyst = class
  protected
    FStream: TMemoryStream;
    FFileName: String;
    FAnalyzeTime: Integer;
    FOriginalFileName: String;
    FInstructions: String;
    FHints: String;
    FVictory: String;
    FDefeat: String;
    FHistory: String;
    FScouts: String;
    FMapData: array of array of Integer;
    FMapWidth: Integer;
    FMapHeight: Integer;
    FAllTechs: Boolean;
    FLastSave: TDateTime;
    FVersion: TScenarioVersion;
    function ExtractStream: Boolean;
    function AnalyzeStream: Boolean;
  public
    Victory: TScenarioVictory;
    constructor Create;
    destructor Destroy; override;
    function Analyze: Boolean;
    procedure Reset;
    function GenerateMap(var Bitmap: TBitmap; const Width: Integer; const Height: Integer; bgColor: TColor): Boolean;
    property FileName: String read FFileName write FFileName;
    property AnalyzeTime: Integer read FAnalyzeTime;
    property OriginalFileName: String read FOriginalFileName;
    property sInstructions: String read FInstructions;
    property sHints: String read FHints;
    property sVictory: String read FVictory;
    property sDefeat: String read FDefeat;
    property sHistory: String read FHistory;
    property sScouts: String read FScouts;
    property AllTechs: Boolean read FAllTechs;
    property LastSave: TDateTime read FLastSave;
  end;

const
  VICTORY_MODES: array[TVictoryMode] of String = (
    'Standard', 'Conquest', 'Score Limit', 'Time Limit', 'Custom');

implementation

uses
  {$IFDEF FPC}paszlib{$ELSE}ZlibEx{$ENDIF}, Math, Context, BitmapEx, RecAnalystConsts;

resourcestring
  c_filenotspecified = 'No file has been specified for analyzing.';
  c_corruptedheader = 'Corruped header section.';
  c_cannotopenfile = 'Cannot open file "%s".';
  c_cannotreadsection = 'Cannot read sections.';
  c_cannotdecompress = 'Cannot decompress header section.';
  c_unknown = 'Unknown error.';
  c_wrongfileext = 'Wrong file extension, file format is not supported.';
  c_outofmemory = 'Out of memory';

{$IFDEF FPC}
function ZDecompressStream2(inStream, outStream: TStream; windowBits: Integer): Integer; forward;
{$ENDIF}
function time_tToDateTime(vtime_t: Integer): TDateTime; forward;

const
  SCN_EXT = '.scn';
  SCX_EXT = '.scx';

{ TScenarioVictory }
function TScenarioVictory.GetVictoryString: String;
var
  sCustom: String;
begin
  Result := '';
  if not (Mode in [Low (VICTORY_MODES)..High (VICTORY_MODES)]) then
    Exit;
  Result := VICTORY_MODES[Mode];

  case Mode of
    vmScoreLimit:
      begin
        if (ScoreLimit <> 0) then
          Result := Format ('%s (%d)', [Result, ScoreLimit]);
      end;
    vmTimeLimit:
      begin
        if (TimeLimit <> 0) then
          Result := Format ('%s (%d years)', [Result, TimeLimit]);
      end;
    vmCustom:
      begin
        sCustom := '';
        if Conquest then
          sCustom := 'Conquest, ';
        if (Relics <> 0) then
          sCustom := sCustom + Format ('Relics: %d, ', [Relics]);
        if (Exploration <> 0) then
          sCustom := sCustom + Format ('Exploration: %d%%, ', [Exploration]);
        if (sCustom <> '') then
        begin
          if All then
            sCustom := sCustom + 'All'
          else
            sCustom := sCustom + 'Any';
          Result := Format ('%s (%s)', [Result, sCustom]);
        end;
      end;
  end;
end;

{ TScAnalyst }
constructor TScAnalyst.Create;
begin
  FStream := TMemoryStream.Create;
  Victory := TScenarioVictory.Create;
  Reset;
end;

destructor TScAnalyst.Destroy;
begin
  FStream.Free;
  Victory.Free;
  SetLength (FMapData, 0);
end;

function TScAnalyst.ExtractStream: Boolean;
var
  ms, inStream: TMemoryStream;
  header_len: Integer;
  version: array[0..3] of Char;
  instructions_len: Longint;
  buff: PChar;
  lastsave: Integer;
begin
  Result := False;

  if (FFileName = '') then
    raise EScAnalystException.Create (c_filenotspecified);

  if (LowerCase (ExtractFileExt (FFileName)) <> SCN_EXT) and
     (LowerCase (ExtractFileExt (FFileName)) <> SCX_EXT) then
  raise EScAnalystException.Create (c_wrongfileext);

  ms := TMemoryStream.Create;
  inStream := TMemoryStream.Create;
  try
    try
      ms.LoadFromFile (FFileName);
      ms.Seek (0, soFromBeginning);

      ms.Read (version, SizeOf (version));
      if (version = '1.18') then
        FVersion := sv1_18
      else if (version = '1.21') then
        FVersion := sv1_21;

      ms.Read (header_len, SizeOf (header_len));
      Inc (header_len, 8);
      ms.Seek (4, soFromCurrent);
      ms.Read (lastsave, SizeOf (lastsave));
      FLastSave := time_tToDateTime (lastsave);
      ms.Read (instructions_len, SizeOf (instructions_len));
      GetMem (buff, instructions_len);
      try
        ms.Read (buff[0], instructions_len);
        FInstructions := buff;
      finally
        FreeMem (buff);
      end;
      ms.Seek (8, soFromCurrent);
      inStream.CopyFrom (ms, ms.Size - header_len);
      instream.Seek (0, soFromBeginning);

      {$IFDEF FPC}
      if (ZDecompressStream2 (inStream, FStream, -15) < 0) then
        raise ERecAnalystException.Create (c_cannotdecompress);
      // zError (code)
      {$ELSE}
      ZDecompressStream2 (inStream, FStream, -15);
      {$ENDIF}

      Result := True;
    except
      on EScAnalystException do
        raise;
      on EReadError do
        raise EScAnalystException.Create (c_cannotreadsection);
      on EFOpenError do
        raise EScAnalystException.CreateFmt (c_cannotopenfile, [FileName]);
      on EOutOfMemory do
        raise EScAnalystException.Create (c_outofmemory);
      {$IFNDEF FPC}
      on EZDecompressionError do
        raise EScAnalystException.Create (c_cannotdecompress);
      {$ENDIF}
      else
        raise EScAnalystException.Create (c_unknown);
    end;
  finally
    FreeAndNil (ms);
    FreeAndNil (inStream);
  end;
end;

function TScAnalyst.AnalyzeStream: Boolean;
var
  originial_filename_len, instructions_len, hints_len: Word;
  victory_len, defeat_len, history_len, scouts_len, wlen: Word;
  buff65536: array[0..65535] of Char;
  subver: Single;
  is_bitmap, ilen, all_techs, i: Integer;
  conquest, relics, exploration, victory_all, victory_mode, score_limit, time_limit: Integer;
  map_size_x, map_size_y, x, y: Integer;
  terrain_id, elevation: Byte;
  BitmapInfoHeader: TBitmapInfoHeader;
begin
  Result := False;

  with FStream do
  begin
    Seek (0, soFromBeginning);
    { Compressed Header }
    Seek (4, soFromCurrent);
    Read (subver, SizeOf (subver));

    if (Round (subver * 100) = 122) then
      FVersion := sv1_22
    else if (Round (subver * 100) = 121) then
      FVersion := sv1_21
    else if (Round (subver * 100) = 120) then
      FVersion := sv1_20;

    Seek (4425, soFromCurrent);
    Read (originial_filename_len, SizeOf (originial_filename_len));
    if (originial_filename_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      Read (buff65536[0], originial_filename_len);
      FOriginalFileName := buff65536;
    end;
    { Messages and Cimenatics }
    Seek (24, soFromCurrent);
    if (FVersion <> sv1_22) then
      Seek (-4, soFromCurrent);
    Read (instructions_len, SizeOf (instructions_len));
    if (instructions_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      Read (buff65536[0], instructions_len);
//      if (FInstructions = '') then
        FInstructions := buff65536;
    end;
    Read (hints_len, SizeOf (hints_len));
    if (hints_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      Read (buff65536[0], hints_len);
      FHints := buff65536;
    end;
    Read (victory_len, SizeOf (victory_len));
    if (victory_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      Read (buff65536[0], victory_len);
      FVictory := buff65536;
    end;
    Read (defeat_len, SizeOf (defeat_len));
    if (defeat_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      Read (buff65536[0], defeat_len);
      FDefeat := buff65536;
    end;
    Read (history_len, SizeOf (history_len));
    if (history_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      Read (buff65536[0], history_len);
      FHistory := buff65536;
    end;
    if (FVersion = sv1_22) then
    begin
      Read (scouts_len, SizeOf (scouts_len));
      if (scouts_len > 0) then
      begin
        FillChar (buff65536, SizeOf (buff65536), #0);
        Read (buff65536[0], scouts_len);
        FScouts := buff65536;
      end;
    end;
    for i := 0 to 3 do
    begin
      Read (wlen, SizeOf (wlen));
      Seek (wlen, soFromCurrent);
    end;
    Read (is_bitmap, SizeOf (is_bitmap));
    Seek (10, soFromCurrent);
    if (is_bitmap = 1) then
    begin
      Read (BitmapInfoHeader, SizeOf (TBitmapInfoHeader));
      Seek (SizeOf (RGBQUAD) * BitmapInfoHeader.biClrUsed, soFromCurrent);
      Seek (BitmapInfoHeader.biSizeImage, soFromCurrent);
    end;
    { Player Data 2 }
    Seek (64, soFromCurrent);
    for i := 0 to 15 do
    begin
      Read (wlen, SizeOf (wlen));
      Seek (wlen, soFromCurrent);
    end;
    for i := 0 to 15 do
    begin
      Seek (8, soFromCurrent);
      Read (ilen, SizeOf (ilen));
      Seek (ilen, soFromCurrent);
    end;
    Seek (404, soFromCurrent);
    { Victory }
    Seek (4, soFromCurrent);
    Read (conquest, SizeOf (conquest));
    Seek (4, soFromCurrent);
    Read (relics, SizeOf (relics));
    Seek (4, soFromCurrent);
    Read (exploration, SizeOf (exploration));
    Seek (4, soFromCurrent);
    Read (victory_all, SizeOf (victory_all));
    Read (victory_mode, SizeOf (victory_mode));
    Read (score_limit, SizeOf (score_limit));
    Read (time_limit, SizeOf (time_limit));
    Victory.Mode := TVictoryMode(victory_mode);
    Victory.All := (victory_all = 1);
    Victory.Conquest := (conquest = 1);
    Victory.Relics := relics;
    Victory.Exploration := exploration;
    Victory.ScoreLimit := score_limit;
    Victory.TimeLimit := time_limit div 10;
    { Diplomacy }
    Seek (12612, soFromCurrent);
    { Disables }
    Seek (5388-4-64, soFromCurrent);
    Read (all_techs, SizeOf (all_techs));
    FAllTechs := (all_techs = 1);
    Seek (64, soFromCurrent);
    { Map }
    Seek (12, soFromCurrent);
    if (FVersion >= sv1_21) then
      Seek (4, soFromCurrent);

    Read (map_size_x, SizeOf (map_size_x));
    Read (map_size_y, SizeOf (map_size_y));
    FMapWidth := map_size_x;
    FMapHeight := map_size_y;
    SetLength (FMapData, map_size_x, map_size_y);
    for y := 0 to map_size_y - 1 do
    begin
      for x := 0 to map_size_x - 1 do
      begin
        Read (terrain_id, SizeOf (terrain_id));
        Read (elevation, SizeOf (elevation));
        Seek (1, soFromCurrent);
        FMapData[x, y] := terrain_id + 1000 * (elevation + 1);
      end;
    end;
    { Units }
    Result := True;
    Exit;
  end;
end;

function TScAnalyst.Analyze: Boolean;
var
  StartTime: DWORD;
begin
  Result := False;
  StartTime := GetTickCount;

  if not ExtractStream then
    Exit;
  try
    if AnalyzeStream then
    begin
      FAnalyzeTime := GetTickCount - StartTime;
      Result := True;
    end;
  finally
    FStream.Clear;
  end;
end;

function TScAnalyst.GenerateMap(var Bitmap: TBitmap; const Width: Integer; const Height: Integer; bgColor: TColor): Boolean;
var
  x, y, terrain_id, elevation: Integer;
  Bmp: TBitmapEx;
begin
  Result := False;
  if not Assigned (FMapData) or not Assigned (Bitmap) then
    Exit;
    
  Bmp := TBitmapEx.Create;
  try
    Bmp.Width := FMapWidth;
    Bmp.Height := FMapHeight;
    for x := 0 to FMapWidth - 1 do
    begin
      for y := 0 to FMapHeight - 1 do
      begin
        terrain_id := FMapData[x, y] mod 1000;
        elevation := (FMapData[x, y] - terrain_id) div 1000;
        Dec (elevation);

        if terrain_id in [Low (TERRAIN_COLORS)..High (TERRAIN_COLORS)] then
          Windows.SetPixel (Bmp.Canvas.Handle, x, y, TERRAIN_COLORS[terrain_id])
        else
          Windows.SetPixel (Bmp.Canvas.Handle, x, y, $ff00ff);  { fuchsia }
      end;
    end;

    Bmp.Rotate (45, bgColor);
    Bmp.Resize2 (Width, Height);
    Bitmap.Assign (Bmp);
    
    Result := True;
  finally
    Bmp.Free;
  end;
end;

procedure TScAnalyst.Reset;
begin
  FStream.Clear;
  FFileName := '';
  FAnalyzeTime := 0;
  FOriginalFileName := '';
  FInstructions := '';
  FHints := '';
  FVictory := '';
  FDefeat := '';
  FHistory := '';
  FScouts := '';
  SetLength (FMapData, 0);
  FMapWidth := 0;
  FMapHeight := 0;
  FAllTechs := False;
  FLastSave := 0;
  FVersion := Low (TScenarioVersion);
end;

function time_tToDateTime(vtime_t: Integer): TDateTime;
type
  PComp = ^Comp;
var
  baseDate: TDateTime;
  systemBaseDate: TSystemTime;
  filetimeBaseDate: TFileTime;
  timet_comp,c : comp;
begin
  baseDate := EncodeDate (1970, 1, 1);
  DateTimeToSystemTime (baseDate, systemBaseDate);
  SystemTimeToFileTime (systemBaseDate, filetimeBaseDate);
  timet_comp := PComp (@filetimeBaseDate)^;

  c := vtime_t;
  c := c * 10000000 + timet_comp;
  filetimeBaseDate := PFileTime (@c)^;
  FileTimeToSystemTime (filetimeBaseDate, systemBaseDate);
  Result := SystemTimeToDateTime (systemBaseDate);
end;

end.
