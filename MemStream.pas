{*
 * $Id$
 * This file is part of the A2CView project.
 *
 * Copyright (c) 2009-2013 biegleux <biegleux[at]gmail[dot]com>
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
 *}
unit MemStream;

interface

uses
  Classes;

type
  TMemStream = class(TMemoryStream)
  public
    function Seek(Offset: Longint): Longint; overload;
    procedure ReadInt32(var Buffer: Int32);
    procedure ReadWord(var Buffer: Word);
    procedure ReadChar(var Buffer: Byte);
    procedure ReadFloat(var Buffer: Single);
    procedure ReadBool(var Buffer: Boolean);
    procedure ReadString(var Buffer; Len: Integer = 4);
  end;

implementation

function TMemStream.Seek(Offset: Longint): Longint;
begin
  Result := Seek(Offset, soFromCurrent);
end;

procedure TMemStream.ReadInt32(var Buffer: Int32);
begin
  ReadBuffer(Buffer, SizeOf(Int32));
end;

procedure TMemStream.ReadWord(var Buffer: Word);
begin
  ReadBuffer(Buffer, SizeOf(Word));
end;

procedure TMemStream.ReadChar(var Buffer: Byte);
begin
  ReadBuffer(Buffer, SizeOf(Byte));
end;

procedure TMemStream.ReadFloat(var Buffer: Single);
begin
  ReadBuffer(Buffer, SizeOf(Single));
end;

procedure TMemStream.ReadBool(var Buffer: Boolean);
var
  Bool: Int32;
begin
  ReadInt32(Bool);
  Buffer := (Bool <> 0);
end;

procedure TMemStream.ReadString(var Buffer; Len: Integer = 4);
var
  iLen: Int32;
  wLen: Word;
begin
  case Len of
    2: begin
      ReadWord(wLen);
      ReadBuffer(Buffer, wLen);
    end;
    4: begin
      ReadInt32(iLen);
      ReadBuffer(Buffer, iLen);
    end;
  end;
end;

end.

