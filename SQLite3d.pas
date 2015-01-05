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
{ Based on SQLite3.pas, SQLite3 dynamic linking, biegleux }

unit SQLite3d; { d as dynamic linking }

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}            (* use AnsiString *)
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

const
{$IF Defined(MSWINDOWS)}
  SQLiteDLL = 'sqlite3.dll';
{$ELSEIF Defined(DARWIN)}
  SQLiteDLL = 'libsqlite3.dylib';
  {$linklib libsqlite3}
{$ELSEIF Defined(UNIX)}
  SQLiteDLL = 'sqlite3.so';
{$IFEND}

// Return values for sqlite3_exec() and sqlite3_step()

const
  SQLITE_OK          =  0; // Successful result
  (* beginning-of-error-codes *)
  SQLITE_ERROR       =  1; // SQL error or missing database
  SQLITE_INTERNAL    =  2; // An internal logic error in SQLite
  SQLITE_PERM        =  3; // Access permission denied
  SQLITE_ABORT       =  4; // Callback routine requested an abort
  SQLITE_BUSY        =  5; // The database file is locked
  SQLITE_LOCKED      =  6; // A table in the database is locked
  SQLITE_NOMEM       =  7; // A malloc() failed
  SQLITE_READONLY    =  8; // Attempt to write a readonly database
  SQLITE_INTERRUPT   =  9; // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10; // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11; // The database disk image is malformed
  SQLITE_NOTFOUND    = 12; // (Internal Only) Table or record not found
  SQLITE_FULL        = 13; // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14; // Unable to open the database file
  SQLITE_PROTOCOL    = 15; // Database lock protocol error
  SQLITE_EMPTY       = 16; // Database is empty
  SQLITE_SCHEMA      = 17; // The database schema changed
  SQLITE_TOOBIG      = 18; // Too much data for one row of a table
  SQLITE_CONSTRAINT  = 19; // Abort due to contraint violation
  SQLITE_MISMATCH    = 20; // Data type mismatch
  SQLITE_MISUSE      = 21; // Library used incorrectly
  SQLITE_NOLFS       = 22; // Uses OS features not supported on host
  SQLITE_AUTH        = 23; // Authorization denied
  SQLITE_FORMAT      = 24; // Auxiliary database format error
  SQLITE_RANGE       = 25; // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26; // File opened that is not a database file
  SQLITE_ROW         = 100; // sqlite3_step() has another row ready
  SQLITE_DONE        = 101; // sqlite3_step() has finished executing

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  SQLITE_UTF8     = 1;
  SQLITE_UTF16    = 2;
  SQLITE_UTF16BE  = 3;
  SQLITE_UTF16LE  = 4;
  SQLITE_ANY      = 5;

  SQLITE_STATIC    {: TSQLite3Destructor} = Pointer(0);
  SQLITE_TRANSIENT {: TSQLite3Destructor} = Pointer(-1);

  SQLITE_FCNTL_LOCKSTATE       = 1;
  SQLITE_GET_LOCKPROXYFILE     = 2;
  SQLITE_SET_LOCKPROXYFILE     = 3;
  SQLITE_LAST_ERRNO            = 4;
  SQLITE_FCNTL_SIZE_HINT       = 5;
  SQLITE_FCNTL_CHUNK_SIZE      = 6;
  SQLITE_FCNTL_FILE_POINTER    = 7;
  SQLITE_FCNTL_SYNC_OMITTED    = 8;

type
  TSQLiteDB = Pointer;
  TSQLiteResult = ^PAnsiChar;
  TSQLiteStmt = Pointer;

type
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array[0 .. (MaxInt div SizeOf(PAnsiChar))-1] of PAnsiChar;

type
  TSQLiteExecCallback = function(UserData: Pointer; NumCols: integer; ColValues:
    PPAnsiCharArray; ColNames: PPAnsiCharArray): integer; cdecl;
  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;

  //function prototype for define own collate
  TCollateXCompare = function(UserData: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;

var
  SQLite3_Initialize: function(): integer; cdecl;
  SQLite3_Shutdown: function(): integer; cdecl;
                                                
  SQLite3_Open: function(filename: PAnsiChar; var db: TSQLiteDB): integer; cdecl;
  SQLite3_Close: function(db: TSQLiteDB): integer; cdecl;
  SQLite3_Exec: function(db: TSQLiteDB; SQLStatement: PAnsiChar; CallbackPtr: TSQLiteExecCallback; UserData: Pointer; var ErrMsg: PAnsiChar): integer; cdecl;
  SQLite3_Version: function(): PAnsiChar; cdecl;
  SQLite3_ErrMsg: function(db: TSQLiteDB): PAnsiChar; cdecl;
  SQLite3_ErrCode: function(db: TSQLiteDB): integer; cdecl;
  SQlite3_Free: procedure(P: PAnsiChar); cdecl;
  SQLite3_GetTable: function(db: TSQLiteDB; SQLStatement: PAnsiChar; var ResultPtr: TSQLiteResult; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PAnsiChar): integer; cdecl;
  SQLite3_FreeTable: procedure(Table: TSQLiteResult); cdecl;
  SQLite3_Complete: function(P: PAnsiChar): boolean; cdecl;
  SQLite3_LastInsertRowID: function(db: TSQLiteDB): int64; cdecl;
  SQLite3_Interrupt: procedure(db: TSQLiteDB); cdecl;
  SQLite3_BusyHandler: procedure(db: TSQLiteDB; CallbackPtr: TSQLiteBusyHandlerCallback; UserData: Pointer); cdecl;
  SQLite3_BusyTimeout: procedure(db: TSQLiteDB; TimeOut: integer); cdecl;
  SQLite3_Changes: function(db: TSQLiteDB): integer; cdecl;
  SQLite3_TotalChanges: function(db: TSQLiteDB): integer; cdecl;
  SQLite3_Prepare: function(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl;
  SQLite3_Prepare_v2: function(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl;
  SQLite3_ColumnCount: function(hStmt: TSqliteStmt): integer; cdecl;
  SQLite3_ColumnName: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl;
  SQLite3_ColumnDeclType: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl;
  SQLite3_Step: function(hStmt: TSqliteStmt): integer; cdecl;
  SQLite3_DataCount: function(hStmt: TSqliteStmt): integer; cdecl;

  SQLite3_ColumnBlob: function(hStmt: TSqliteStmt; ColNum: integer): pointer; cdecl;
  SQLite3_ColumnBytes: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
  SQLite3_ColumnDouble: function(hStmt: TSqliteStmt; ColNum: integer): double; cdecl;
  SQLite3_ColumnInt: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
  SQLite3_ColumnText: function(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl;
  SQLite3_ColumnType: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
  SQLite3_ColumnInt64: function(hStmt: TSqliteStmt; ColNum: integer): Int64; cdecl;
  SQLite3_Finalize: function(hStmt: TSqliteStmt): integer; cdecl;
  SQLite3_Reset: function(hStmt: TSqliteStmt): integer; cdecl;
  SQLite3_Get_Autocommit: function(db: TSQLiteDB): integer; cdecl;
  SQLite3_enable_load_extension: function(db: TSQLiteDB; OnOff: integer): integer; cdecl;
  SQLite3_file_control: function(db: TSQLiteDB; filename: PAnsiChar; option: integer; data: pointer): integer; cdecl;

// 
// In the SQL strings input to sqlite3_prepare() and sqlite3_prepare16(),
// one or more literals can be replace by a wildcard "?" or ":N:" where
// N is an integer.  These value of these wildcard literals can be set
// using the routines listed below.
// 
// In every case, the first parameter is a pointer to the sqlite3_stmt
// structure returned from sqlite3_prepare().  The second parameter is the
// index of the wildcard.  The first "?" has an index of 1.  ":N:" wildcards
// use the index N.
// 
// The fifth parameter to sqlite3_bind_blob(), sqlite3_bind_text(), and
//sqlite3_bind_text16() is a destructor used to dispose of the BLOB or
//text after SQLite has finished with it.  If the fifth argument is the
// special value SQLITE_STATIC, then the library assumes that the information
// is in static, unmanaged space and does not need to be freed.  If the
// fifth argument has the value SQLITE_TRANSIENT, then SQLite makes its
// own private copy of the data.
//
// The sqlite3_bind_* routine must be called before sqlite3_step() after
// an sqlite3_prepare() or sqlite3_reset().  Unbound wildcards are interpreted
// as NULL.
// 

type
  TSQLite3Destructor = procedure(Ptr: Pointer); cdecl;

var
  sqlite3_bind_blob: function(hStmt: TSqliteStmt; ParamNum: integer;
  ptrData: pointer; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
  sqlite3_bind_text: function(hStmt: TSqliteStmt; ParamNum: integer;
  Text: PAnsiChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;
  sqlite3_bind_double: function(hStmt: TSqliteStmt; ParamNum: integer; Data: Double): integer; cdecl;
  sqlite3_bind_int: function(hStmt: TSqLiteStmt; ParamNum: integer; Data: integer): integer; cdecl;
  sqlite3_bind_int64: function(hStmt: TSqliteStmt; ParamNum: integer; Data: int64): integer; cdecl;
  sqlite3_bind_null: function(hStmt: TSqliteStmt; ParamNum: integer): integer; cdecl;

  sqlite3_bind_parameter_index: function(hStmt: TSqliteStmt; zName: PAnsiChar): integer; cdecl;
  sqlite3_clear_bindings: function(hStmt: TSqliteStmt): integer; cdecl;

  sqlite3_enable_shared_cache: function(Value: integer): integer; cdecl;

//user collate definiton
  SQLite3_create_collation: function(db: TSQLiteDB; Name: PAnsiChar; eTextRep: integer;
  UserData: pointer; xCompare: TCollateXCompare): integer; cdecl;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): String;
function SQLiteErrorStr(SQLiteErrorCode: Integer): String;

function SQLiteLoadLib(const LibName: String): Boolean;

var
  SQLiteLibLoaded: Boolean = False;

implementation

uses
  SysUtils{$IFNDEF FPC}{$IFDEF WIN32}, Windows{$ENDIF}{$ELSE}, Dynlibs{$ENDIF};

var
  SQLiteLibHandle: {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF};

procedure ClearPointers; forward;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): String;
begin
  case SQLiteFieldTypeCode of
    SQLITE_INTEGER: Result := 'Integer';
    SQLITE_FLOAT: Result := 'Float';
    SQLITE_TEXT: Result := 'Text';
    SQLITE_BLOB: Result := 'Blob';
    SQLITE_NULL: Result := 'Null';
  else
    Result := 'Unknown SQLite Field Type Code "' + IntToStr(SQLiteFieldTypeCode) + '"';
  end;
end;

function SQLiteErrorStr(SQLiteErrorCode: Integer): AnsiString;
begin
  case SQLiteErrorCode of
    SQLITE_OK: Result := 'Successful result';
    SQLITE_ERROR: Result := 'SQL error or missing database';
    SQLITE_INTERNAL: Result := 'An internal logic error in SQLite';
    SQLITE_PERM: Result := 'Access permission denied';
    SQLITE_ABORT: Result := 'Callback routine requested an abort';
    SQLITE_BUSY: Result := 'The database file is locked';
    SQLITE_LOCKED: Result := 'A table in the database is locked';
    SQLITE_NOMEM: Result := 'A malloc() failed';
    SQLITE_READONLY: Result := 'Attempt to write a readonly database';
    SQLITE_INTERRUPT: Result := 'Operation terminated by sqlite3_interrupt()';
    SQLITE_IOERR: Result := 'Some kind of disk I/O error occurred';
    SQLITE_CORRUPT: Result := 'The database disk image is malformed';
    SQLITE_NOTFOUND: Result := '(Internal Only) Table or record not found';
    SQLITE_FULL: Result := 'Insertion failed because database is full';
    SQLITE_CANTOPEN: Result := 'Unable to open the database file';
    SQLITE_PROTOCOL: Result := 'Database lock protocol error';
    SQLITE_EMPTY: Result := 'Database is empty';
    SQLITE_SCHEMA: Result := 'The database schema changed';
    SQLITE_TOOBIG: Result := 'Too much data for one row of a table';
    SQLITE_CONSTRAINT: Result := 'Abort due to contraint violation';
    SQLITE_MISMATCH: Result := 'Data type mismatch';
    SQLITE_MISUSE: Result := 'Library used incorrectly';
    SQLITE_NOLFS: Result := 'Uses OS features not supported on host';
    SQLITE_AUTH: Result := 'Authorization denied';
    SQLITE_FORMAT: Result := 'Auxiliary database format error';
    SQLITE_RANGE: Result := '2nd parameter to sqlite3_bind out of range';
    SQLITE_NOTADB: Result := 'File opened that is not a database file';
    SQLITE_ROW: Result := 'sqlite3_step() has another row ready';
    SQLITE_DONE: Result := 'sqlite3_step() has finished executing';
  else
    Result := 'Unknown SQLite Error Code "' + IntToStr(SQLiteErrorCode) + '"';
  end;
end;

function ColValueToStr(Value: PAnsiChar): AnsiString;
begin
  if (Value = nil) then
    Result := 'NULL'
  else
    Result := Value;
end;

function SQLiteLoadLib(const LibName: String): Boolean;

  function GetProcAddr (lpProcName: PChar): Pointer;
  begin
    {$IFDEF FPC}
    Result := GetProcedureAddress (SQLiteLibHandle, lpProcName);
    {$ELSE}
    Result := GetProcAddress (SQLiteLibHandle, lpProcName);
    {$ENDIF}
    if not Assigned (Result) then
      raise Exception.CreateFmt ('The function %s was not found in the current installed Sqlite library',
                          [lpProcName]);
  end;

begin
  Result := SQLiteLibLoaded;
  if Result then
    Exit;

  {$IFDEF FPC}
  SQLiteLibHandle := LoadLibrary (LibName);
  {$ELSE}
  SQLiteLibHandle := LoadLibrary (PChar (LibName));
  {$ENDIF}

  if SQLiteLibHandle <> 0 then
  begin
    try
      @SQLite3_Initialize := GetProcAddr ('sqlite3_initialize');
      @SQLite3_Shutdown := GetProcAddr ('sqlite3_shutdown');

      @SQLite3_Open := GetProcAddr ('sqlite3_open');
      @SQLite3_Close := GetProcAddr ('sqlite3_close');
      @SQLite3_Exec := GetProcAddr ('sqlite3_exec');
      @SQLite3_Version := GetProcAddr ('sqlite3_libversion');
      @SQLite3_ErrMsg := GetProcAddr ('sqlite3_errmsg');
      @SQLite3_ErrCode := GetProcAddr ('sqlite3_errcode');
      @SQlite3_Free := GetProcAddr ('sqlite3_free');
      @SQLite3_GetTable := GetProcAddr ('sqlite3_get_table');
      @SQLite3_FreeTable := GetProcAddr ('sqlite3_free_table');
      @SQLite3_Complete := GetProcAddr ('sqlite3_complete');
      @SQLite3_LastInsertRowID := GetProcAddr ('sqlite3_last_insert_rowid');
      @SQLite3_Interrupt := GetProcAddr ('sqlite3_interrupt');
      @SQLite3_BusyHandler := GetProcAddr ('sqlite3_busy_handler');
      @SQLite3_BusyTimeout := GetProcAddr ('sqlite3_busy_timeout');
      @SQLite3_Changes := GetProcAddr ('sqlite3_changes');
      @SQLite3_TotalChanges := GetProcAddr ('sqlite3_total_changes');
      @SQLite3_Prepare := GetProcAddr ('sqlite3_prepare');
      @SQLite3_Prepare_v2 := GetProcAddr ('sqlite3_prepare_v2');
      @SQLite3_ColumnCount := GetProcAddr ('sqlite3_column_count');
      @SQLite3_ColumnName := GetProcAddr ('sqlite3_column_name');
      @SQLite3_ColumnDeclType := GetProcAddr ('sqlite3_column_decltype');
      @SQLite3_Step := GetProcAddr ('sqlite3_step');
      @SQLite3_DataCount := GetProcAddr ('sqlite3_data_count');

      @SQLite3_ColumnBlob := GetProcAddr ('sqlite3_column_blob');
      @SQLite3_ColumnBytes := GetProcAddr ('sqlite3_column_bytes');
      @SQLite3_ColumnDouble := GetProcAddr ('sqlite3_column_double');
      @SQLite3_ColumnInt := GetProcAddr ('sqlite3_column_int');
      @SQLite3_ColumnText := GetProcAddr ('sqlite3_column_text');
      @SQLite3_ColumnType := GetProcAddr ('sqlite3_column_type');
      @SQLite3_ColumnInt64 := GetProcAddr ('sqlite3_column_int64');
      @SQLite3_Finalize := GetProcAddr ('sqlite3_finalize');
      @SQLite3_Reset := GetProcAddr ('sqlite3_reset');
      @SQLite3_Get_Autocommit := GetProcAddr ('sqlite3_get_autocommit');
      @SQLite3_enable_load_extension := GetProcAddr ('sqlite3_enable_load_extension');
      @SQLite3_file_control := GetProcAddr ('sqlite3_file_control');

      @sqlite3_bind_blob := GetProcAddr ('sqlite3_bind_blob');
      @sqlite3_bind_text := GetProcAddr ('sqlite3_bind_text');
      @sqlite3_bind_double := GetProcAddr ('sqlite3_bind_double');
      @sqlite3_bind_int := GetProcAddr ('sqlite3_bind_int');
      @sqlite3_bind_int64 := GetProcAddr ('sqlite3_bind_int64');
      @sqlite3_bind_null := GetProcAddr ('sqlite3_bind_null');

      @sqlite3_bind_parameter_index := GetProcAddr ('sqlite3_bind_parameter_index');
      @sqlite3_clear_bindings := GetProcAddr ('sqlite3_clear_bindings');

      @sqlite3_enable_shared_cache := GetProcAddr ('sqlite3_enable_shared_cache');

      @SQLite3_create_collation := GetProcAddr ('sqlite3_create_collation');
      
      Result := True;
    except
      Result := False;
      {$IFNDEF FPC}FreeLibrary (SQLiteLibHandle){$ELSE}UnloadLibrary (SQLiteLibHandle){$ENDIF};
      SQLiteLibHandle := 0;
      ClearPointers;
    end;
  end;
end;

procedure ClearPointers;
begin
  SQLite3_Initialize := nil;
  SQLite3_Shutdown := nil;

  SQLite3_Open := nil;
  SQLite3_Close := nil;
  SQLite3_Exec := nil;
  SQLite3_Version := nil;
  SQLite3_ErrMsg := nil;
  SQLite3_ErrCode := nil;
  SQlite3_Free := nil;
  SQLite3_GetTable := nil;
  SQLite3_FreeTable := nil;
  SQLite3_Complete := nil;
  SQLite3_LastInsertRowID := nil;
  SQLite3_Interrupt := nil;
  SQLite3_BusyHandler := nil;
  SQLite3_BusyTimeout := nil;
  SQLite3_Changes := nil;
  SQLite3_TotalChanges := nil;
  SQLite3_Prepare := nil;
  SQLite3_Prepare_v2 := nil;
  SQLite3_ColumnCount := nil;
  SQLite3_ColumnName := nil;
  SQLite3_ColumnDeclType := nil;
  SQLite3_Step := nil;
  SQLite3_DataCount := nil;

  SQLite3_ColumnBlob := nil;
  SQLite3_ColumnBytes := nil;
  SQLite3_ColumnDouble := nil;
  SQLite3_ColumnInt := nil;
  SQLite3_ColumnText := nil;
  SQLite3_ColumnType := nil;
  SQLite3_ColumnInt64 := nil;
  SQLite3_Finalize := nil;
  SQLite3_Reset := nil;
  SQLite3_Get_Autocommit := nil;
  SQLite3_enable_load_extension := nil;
  SQLite3_file_control := nil;

  sqlite3_bind_blob := nil;
  sqlite3_bind_text := nil;
  sqlite3_bind_double := nil;
  sqlite3_bind_int := nil;
  sqlite3_bind_int64 := nil;
  sqlite3_bind_null := nil;

  sqlite3_bind_parameter_index := nil;
  sqlite3_clear_bindings := nil;

  sqlite3_enable_shared_cache := nil;

  SQLite3_create_collation := nil;
end;

initialization
  SQLiteLibLoaded := SQLiteLoadLib (IncludeTrailingPathDelimiter
          (ExtractFilePath (GetModuleName (HInstance))) + SQLiteDLL);

finalization
  if SQLiteLibHandle <> 0 then
    {$IFNDEF FPC}FreeLibrary (SQLiteLibHandle){$ELSE}UnloadLibrary (SQLiteLibHandle){$ENDIF};

end.
