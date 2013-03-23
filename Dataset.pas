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
unit Dataset;

{$mode objfpc}{$H+}

{$DEFINE SQLITE_DYNLINK}

interface

uses
  Classes, SqliteWrap;

type
  TFieldset = class(TObject)
  public
    GameType: Integer;
    MapStyle: Integer;
    Map: String;
    Duration: Integer;
    Players: String;
    POV: Integer;
    Version: Integer;
    Hash: AnsiString;
    PlayerList: TStringList;
    constructor Create();
    destructor Destroy(); override;
  end;

type
  TDataset = class(TObject)
  private
    Db: TSqliteDatabase;
    procedure CreateTables();
  public
    constructor Create(const ADatasetFileName: String);
    destructor Destroy(); override;
    function AddFieldset(const Fieldset: TFieldset): Boolean;
    function GetFieldset(var Fieldset: TFieldset): Boolean;
  end;

implementation

uses
  SysUtils;

constructor TFieldset.Create();
begin
  inherited Create();
  PlayerList := TStringList.Create();
end;

destructor TFieldset.Destroy();
begin
  PlayerList.Free();
  inherited Destroy();
end;

constructor TDataset.Create(const ADatasetFileName: String);
var
  HasTables: Boolean;
begin
  if (ADatasetFileName = '') then Exit;

  HasTables := FileExists(ADatasetFileName);
  try
    Db := TSqliteDatabase.Create(ADatasetFileName);
    if HasTables then Exit;
    CreateTables();
  except
    on E: Exception do
      Exit;
  end;
end;

procedure TDataset.CreateTables();
var
  sSQL: String;
begin
  if not Assigned(Db){$IFDEF SQLITE_DYNLINK} or not Db.SQLiteLibLoaded{$ENDIF} then
    Exit;

  { Begin transaction }
  Db.BeginTransaction();
  try
    { Create table Games }
    sSQL := 'CREATE TABLE IF NOT EXISTS Games('
          + ' Id INTEGER PRIMARY KEY,'
          + ' GameType INTEGER,'
          + ' MapStyle INTEGER,'
          + ' Map TEXT,'
          + ' Duration INTEGER,'
          + ' Players TEXT,'
          + ' POV INTEGER,'
          + ' Version INTEGER,'
          + ' Hash BLOB);';
    Db.ExecSQL(sSQL);

    sSQL := 'CREATE INDEX IF NOT EXISTS idx_Hash ON Games (Hash)';
    Db.ExecSQL(sSQL);

    { Create table Players }
    sSQL := 'CREATE TABLE IF NOT EXISTS Players('
          + ' Gid INTEGER,'
          + ' PlayerName TEXT);';
    Db.ExecSQL(sSQL);

    sSQL := 'CREATE INDEX IF NOT EXISTS idx_Gid ON Players (Gid)';
    Db.ExecSQL(sSQL);

    { Commit }
    Db.Commit();
  except
    { Rollback on exception }
    Db.Rollback();
    raise;
  end;
end;

destructor TDataset.Destroy();
begin
  Db.Free();
  inherited Destroy();
end;

function TDataset.AddFieldset(const Fieldset: TFieldset): Boolean;
var
  sSQL: String;
  Gid, i: Integer;
begin
  Result := False;
  if not Assigned(Db){$IFDEF SQLITE_DYNLINK} or not Db.SQLiteLibLoaded{$ENDIF} then
    Exit;

  try
    { Begin transaction }
    Db.BeginTransaction();
    try
      Db.AddParamInt (':gametype', Fieldset.GameType);
      Db.AddParamInt (':mapstyle', Fieldset.MapStyle);
      Db.AddParamText(':map', Fieldset.Map);
      Db.AddParamInt (':duration', Fieldset.Duration);
      Db.AddParamText(':players', Fieldset.Players);
      Db.AddParamInt (':pov', Fieldset.POV);
      Db.AddParamInt (':version', Fieldset.Version);
      Db.AddParamText(':hash', Fieldset.Hash);

      sSQL := 'INSERT INTO Games (GameType, MapStyle, Map, Duration, Players, POV, Version, Hash) '
            + 'VALUES (:gametype, :mapstyle, :map, :duration, :players, :pov, :version, :hash)';
      Db.ExecSQL(sSQL);

      Gid := Db.LastInsertRowID();
      for i := 0 to Fieldset.PlayerList.Count - 1 do
      begin
        Db.AddParamInt (':gid', Gid);
        Db.AddParamText(':playername', Fieldset.PlayerList[i]);

        sSQL := 'INSERT INTO Players (Gid, PlayerName) '
              + 'VALUES (:gid, :playername)';
        Db.ExecSQL(sSQL);
      end;

      { Commit }
      Db.Commit();

      Result := True;
    except
      on E: Exception do
        { Rollback on exception }
        Db.Rollback();
    end;
  except
    on E: Exception do
      Exit;
  end;
end;

function TDataset.GetFieldset(var Fieldset: TFieldset): Boolean;
var
  Table: TSqliteTable;
  sSQL: String;
begin
  Result := False;
  if not Assigned(Db){$IFDEF SQLITE_DYNLINK} or not Db.SQLiteLibLoaded{$ENDIF} then
    Exit;

  try
    Db.AddParamText(':hash', Fieldset.Hash);
    sSQL := 'SELECT * '
          + 'FROM Games '
          + 'JOIN Players ON Players.Gid = Games.Id '
          + 'WHERE hash = :hash';
    Table := Db.GetTable(sSQL);

    try
      if (Table.Row > 0) then
      begin
        Fieldset.GameType := Table.FieldAsInteger(Table.FieldIndex['GameType']);
        Fieldset.MapStyle := Table.FieldAsInteger(Table.FieldIndex['MapStyle']);
        Fieldset.Map      := Table.FieldAsString (Table.FieldIndex['Map']);
        Fieldset.Duration := Table.FieldAsInteger(Table.FieldIndex['Duration']);
        Fieldset.Players  := Table.FieldAsString (Table.FieldIndex['Players']);
        Fieldset.POV      := Table.FieldAsInteger(Table.FieldIndex['POV']);
        Fieldset.Version  := Table.FieldAsInteger(Table.FieldIndex['Version']);
        Result := True;
      end;

      while not Table.EOF do
      begin
        Fieldset.PlayerList.Add(Table.FieldAsString(Table.FieldIndex['PlayerName']));
        Table.Next();
      end;

    finally
      Table.Free();
    end;
  except
    on E: Exception do
      Exit;
  end;
end;

end.

