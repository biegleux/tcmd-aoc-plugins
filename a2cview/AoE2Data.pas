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
unit AoE2Data;

{$mode objfpc}{$H+}

interface

uses
  Classes;

var
  fGameType,
  fMapStyle,
  fMap,
  fDuration,
  fPlayers,
  fPOV,
  fVersion,
  fPlayerCount: String;
  fPlayerList: TStringList;

function GetData(const FileName: String): Boolean;

implementation

uses
  SysUtils, RecAnalystWrap, Dataset, MD5, Context;

var
  fLastFilename: String = '';
  fLastResult: Boolean = False;
  RecAnalyst: TRecAnalyst;
  SQLiDataset: TDataset;

procedure ZeroData();
begin
  fGameType := '';
  fMapStyle := '';
  FMap := '';
  fDuration := '';
  fPlayers := '';
  fPOV := '';
  fVersion := '';
  fPlayerCount := '';
  fPlayerList.Clear();
end;

function LoadData(const FileName: String): Boolean;
var
  Fieldset: TFieldset;
  FileHash: array[0..15] of Byte;
  i: Integer;
  P: TPlayer;
begin
  Result := False;
  if (FileName = '') then Exit;

  { Calculate md5 hash of the file }
  FillChar(FileHash, SizeOf(FileHash), 0);
  try
    MD5File(FileName, FileHash);
  except
    on E: Exception do
      Exit;
  end;

  Fieldset := TFieldset.Create();

  Fieldset.Hash := '';
  for i := Low(FileHash) to High(FileHash) do
    Fieldset.Hash := Fieldset.Hash + Chr(FileHash[i]);

  try
    if not SQLiDataset.GetFieldset(Fieldset) then
    begin
      try
        RecAnalyst.Analyze(FileName);
        Fieldset.GameType := Ord(RecAnalyst.GameSettings.GameType);
        Fieldset.MapStyle := Ord(RecAnalyst.GameSettings.MapStyle);
        if RecAnalyst.GameSettings.IsScenario then
          Fieldset.Map := Trim(ChangeFileExt(RecAnalyst.GameSettings.ScFileName, ''))
        else
          Fieldset.Map := Trim(RecAnalyst.GameSettings.Map);
        Fieldset.Duration := RecAnalyst.GameSettings.PlayTime;
        Fieldset.Players := RecAnalyst.GameSettings.PlayersType;
        for i := 0 to RecAnalyst.Players.Count - 1 do
        begin
          P := RecAnalyst.Players[i] as TPlayer;
          if P.Owner then
          begin
            Fieldset.POV := i;
            Break;
          end;
        end;
        Fieldset.Version := Ord(RecAnalyst.GameSettings.GameVersion);
        for i := 0 to RecAnalyst.Players.Count - 1 do
        begin
          P := RecAnalyst.Players[i] as TPlayer;
          Fieldset.PlayerList.Add(P.Name);
        end;

        SQLiDataset.AddFieldset(Fieldset);
      except
        on E: Exception do
          Exit;
      end;
    end;

    RecAnalyst.GameSettings.GameType := TGameType(Fieldset.GameType);
    fGameType := RecAnalyst.GameSettings.GameTypeString;
    RecAnalyst.GameSettings.GameVersion := TGameVersion(Fieldset.Version);
    fVersion := RecAnalyst.GameSettings.GameVersionString;

    if (RecAnalyst.GameSettings.GameVersion > gvAOK20a) then { AOC only }
    begin
      RecAnalyst.GameSettings.MapStyle := TMapStyle(Fieldset.MapStyle);
      fMapStyle := RecAnalyst.GameSettings.MapStyleString;
    end else
      fMapStyle := '';

    fMap := Fieldset.Map;
    fDuration := TRecAnalyst.GameTimeToString(Fieldset.Duration);
    fPlayers := Fieldset.Players;
    fPlayerList.Text := Fieldset.PlayerList.Text;
    fPlayerCount := IntToStr(fPlayerList.Count);
    try
      fPOV := Fieldset.PlayerList[Fieldset.POV];
    except
      fPOV := '';
    end;

    Result := True;
  finally
    Fieldset.Free();
  end;
end;

function GetData(const FileName: String): Boolean;
begin
  if (FileName = fLastFilename) then // file is already read
  begin
    Result := fLastResult;
    Exit;
  end;
  fLastFilename := FileName;
  ZeroData();
  Result := LoadData(FileName);
  fLastResult := Result;
end;

initialization
  RecAnalyst := TRecAnalyst.Create();
  fPlayerList := TStringList.Create();
  SQLiDataset := TDataset.Create(TConfig.GetDatasetFileName());

finalization
  FreeAndNil(RecAnalyst);
  FreeAndNil(fPlayerList);
  SQLiDataset.Free();
end.

