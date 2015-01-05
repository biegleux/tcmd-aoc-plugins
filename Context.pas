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
unit Context;

{$mode objfpc}{$H+}

interface

type
  TConfig = class(TObject)
  public
    { Reads path to dataset from an ini file }
    class function GetDatasetFileName(): String;
  end;

implementation

uses
  IniFiles, SysUtils, Classes;

const
  DatasetFileName: String = 'dataset.db';
  PluginName: String = 'A2CView';

class function TConfig.GetDatasetFileName(): String;
var
  IniFileName: String;
  IniFile: TIniFile;
  DbFileName: String;
begin
  Result := '';
  IniFileName := ChangeFileExt(GetModuleName(HInstance), '.ini');
  IniFile := TIniFile.Create(IniFileName);
  try
    try
      DbFileName := IniFile.ReadString(PluginName, 'DatasetFile', '');
      if (DbFileName = '') then
      begin
        DbFileName := IncludeTrailingPathDelimiter
          (ExtractFilePath(GetModuleName(HInstance))) + DatasetFileName;
        IniFile.WriteString(PluginName, 'DatasetFile', DbFileName);
      end;
      Result := DbFileName;
    except
      on E: Exception do
        Exit;
    end;
  finally
    IniFile.Free();
  end;
end;

end.

