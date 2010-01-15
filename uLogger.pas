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
unit uLogger;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TLoggerMessageType = (mtMessage, mtWarning, mtError, mtException);
  TLoggerMessageState = (msNew, msInLogFile);

  { logger message }
  TLoggerMessage = class(TObject)
  private
    FMessageType: TLoggerMessageType;
    FMessageState: TLoggerMessageState;
    FMessageText: String;
    FTimeStamp: TDateTime;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    property MessageType: TLoggerMessageType read FMessageType write FMessageType;
    property State: TLoggerMessageState read FMessageState write FMessageState;
    property MessageText: String read FMessageText write FMessageText;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
  end;

  { logger message list }
  TLoggerMessageList = class(TObject)
  private
    FList: TThreadList;
  protected
    function GetCount: Integer;
    function GetMessage(Index: Integer): TLoggerMessage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Msg: TLoggerMessage): Integer;
    procedure Insert(Msg: TLoggerMessage);
    procedure Delete(Index: Integer);

    property Count: Integer read GetCount;
    property Messages[Index: Integer]: TLoggerMessage read GetMessage; default;
  end;

  TLogger = class(TObject)
  private
    FActive: Boolean;
    FLoggerMessage: TLoggerMessage;
    FLoggerMessageList: TLoggerMessageList;
  protected
    procedure InternalSend(var Msg: TLoggerMessage);
    procedure SendString(MsgType: TLoggerMessageType; Msg: String; const KeepInList: Boolean = False);
    function MessageToString(const Msg: TLoggerMessage): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Send(const Msg: String; const KeepInList: Boolean = False); overload;
    procedure Send(const Msg: string; const Args: array of const;
      const KeepInList: Boolean = False); overload;

    procedure SendWarning(const Msg: String; const KeepInList: Boolean = False); overload;
    procedure SendWarning(const Msg: string; const Args: array of const;
      const KeepInList: Boolean = False); overload;

    procedure SendError(const Msg: String; const KeepInList: Boolean = False); overload;
    procedure SendError(const Msg: string; const Args: array of const;
      const KeepInList: Boolean = False); overload;

    procedure SendException(E: Exception; const KeepInList: Boolean = False); overload;
    procedure SendException(const Msg: String; E: Exception;
      const KeepInList: Boolean = False); overload;
    procedure SendException(const Msg: String; const Args: array of const;
      const KeepInList: Boolean = False); overload;
    procedure SendException(const Msg: String; const Args: array of const; E: Exception;
      const KeepInList: Boolean = False); overload;

    procedure LoadFromFile(const FileName: String); overload;
    procedure LoadFromFile; overload;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: String = '');

    property Active: Boolean read FActive write FActive;
    property MessageList: TLoggerMessageList read FLoggerMessageList;
  end;

  { logger singleton }
  function Logger: TLogger;

const
  MESSAGETYPES: array[mtMessage..mtException] of String = (
    'Message',
    'Warning',
    'Error',
    'Exception'
  );

implementation

uses
  Windows;

var
  InternalLogger: TLogger;
  LoggerCriticalSection: TRTLCriticalSection;

function Logger: TLogger;
begin
  EnterCriticalSection(LoggerCriticalSection);

  if not Assigned(InternalLogger) then
    InternalLogger := TLogger.Create;

  Result := InternalLogger;

  LeaveCriticalSection(LoggerCriticalSection);
end;

{ TLoggerMessage }

constructor TLoggerMessage.Create;
begin
  inherited Create;
  FMessageType := mtMessage;
  FMessageState := msNew;
  FMessageText := '';
  FTimeStamp := Now;
end;

destructor TLoggerMessage.Destroy;
begin
  inherited Destroy;
end;

{ TLoggerMessageList }

constructor TLoggerMessageList.Create;
begin
  inherited Create;
  FList := TThreadList.Create;
end;

destructor TLoggerMessageList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TLoggerMessageList.GetCount: Integer;
var
  List: TList;
begin
  List := FList.LockList;
  try
    Result := List.Count;
  finally
    FList.UnlockList;
  end;
end;

function TLoggerMessageList.GetMessage(Index: Integer): TLoggerMessage;
var
  List: TList;
begin
  List := FList.LockList;
  try
    Result := TLoggerMessage(List[Index]);
  finally
    FList.UnlockList;
  end;
end;

procedure TLoggerMessageList.Clear;
var
  i: Integer;
  List: TList;
begin
  List := FList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      TObject(List[i]).Free;
      List[i] := nil;
    end;
    List.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TLoggerMessageList.Add(Msg: TLoggerMessage): Integer;
var
  List: TList;
begin
  List := FList.LockList;
  try
    Result := List.Add(Msg);
  finally
    FList.UnlockList;
  end;
end;

procedure TLoggerMessageList.Insert(Msg: TLoggerMessage);
var
  List: TList;
begin
  List := FList.LockList;
  try
    List.Insert(0, Msg);
  finally
    FList.UnlockList;
  end;
end;

procedure TLoggerMessageList.Delete(Index: Integer);
var
  List: TList;
begin
  List := FList.LockList;
  try
    TObject(List[Index]).Free;
    List.Delete(Index);
  finally
    FList.UnlockList;
  end;
end;

{ TLogger }

constructor TLogger.Create;
begin
  FActive := True;
  FLoggerMessageList := TLoggerMessageList.Create;
  FLoggerMessage := TLoggerMessage.Create;
  inherited Create;
end;

destructor TLogger.Destroy;
begin
  FLoggerMessageList.Free;
  FLoggerMessage.Free;
  inherited Destroy;
end;

procedure TLogger.InternalSend(var Msg: TLoggerMessage);
var
  FileName, sMsg: String;
  FileStream: TFileStream;
begin
  FileName := ChangeFileExt(GetModuleName(HInstance), '.log');

  try
    if FileExists(FileName) then
      FileStream := TFileStream.Create(FileName, fmOpenReadWrite, fmShareDenyWrite)
    else
      FileStream := TFileStream.Create(FileName, fmCreate, fmShareDenyWrite);
    try
      FileStream.Seek(0, soFromEnd);

      sMsg := MessageToString(Msg);

//      FileStream.WriteBuffer(sMsg[1], Length(sMsg));
      FileStream.WriteBuffer(Pointer(sMsg)^, Length(sMsg));
      Msg.State := msInLogFile;
    finally
      FileStream.Free;
    end;
  except
    // Silently discard exceptions
  end;
end;

procedure TLogger.SendString(MsgType: TLoggerMessageType; Msg: String; const KeepInList: Boolean = False);
var
  LoggerMessage: TLoggerMessage;
begin
  if not FActive then
    Exit;

  if KeepInList then
    LoggerMessage := TLoggerMessage.Create
  else
    LoggerMessage := FLoggerMessage;

  LoggerMessage.MessageType := MsgType;
  LoggerMessage.MessageText := Msg;

  if KeepInList then
    FLoggerMessageList.Add(LoggerMessage);

  InternalSend(LoggerMessage);
end;

function TLogger.MessageToString(const Msg: TLoggerMessage): String;
begin
  Result := Format('[%s] [%s] %s', [DateTimeToStr(Msg.TimeStamp), MESSAGETYPES[Msg.MessageType],
    Msg.MessageText]) + sLineBreak;
end;

procedure TLogger.Send(const Msg: String; const KeepInList: Boolean = False);
begin
  SendString(mtMessage, Msg, KeepInList);
end;

procedure TLogger.Send(const Msg: string; const Args: array of const;
  const KeepInList: Boolean = False);
begin
  Send(Format(Msg, Args));
end;

procedure TLogger.SendWarning(const Msg: String; const KeepInList: Boolean = False);
begin
  SendString(mtWarning, Msg, KeepInList);
end;

procedure TLogger.SendWarning(const Msg: string; const Args: array of const;
  const KeepInList: Boolean = False);
begin
  SendWarning(Format(Msg, Args));
end;

procedure TLogger.SendError(const Msg: String; const KeepInList: Boolean = False);
begin
  SendString(mtError, Msg, KeepInList);
end;

procedure TLogger.SendError(const Msg: string; const Args: array of const;
  const KeepInList: Boolean = False);
begin
  SendError(Format(Msg, Args));
end;

procedure TLogger.SendException(E: Exception; const KeepInList: Boolean = False);
begin
  SendString(mtException, Format('%s: %s', [E.ClassName, E.Message]), KeepInList);
end;

procedure TLogger.SendException(const Msg: String; E: Exception; 
  const KeepInList: Boolean = False);
begin
  SendString(mtException, Format('%s: %s - %s', [E.ClassName, E.Message, Msg]), KeepInList);
end;

procedure TLogger.SendException(const Msg: String; const Args: array of const;
  const KeepInList: Boolean = False);
begin
  SendString(mtException, Format(Msg, Args), KeepInList)
end;

procedure TLogger.SendException(const Msg: String; const Args: array of const; E: Exception;
  const KeepInList: Boolean = False);
begin
  SendString(mtException, Format('%s: %s - %s', [E.ClassName, E.Message, Format(Msg, Args)]), KeepInList);
end;

procedure TLogger.LoadFromFile(const FileName: String);
begin
end;

procedure TLogger.LoadFromFile;
var
  FileName: String;
begin
  FileName := ChangeFileExt(GetModuleName(HInstance), '.log');
  LoadFromFile(FileName);
end;

procedure TLogger.SaveToStream(Stream: TStream);
var
  S: String;
  i: Integer;
begin
  S := '';
  for i := 0 to FLoggerMessageList.Count - 1 do
    S := S + MessageToString(FLoggerMessageList[i]);
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TLogger.SaveToFile(FileName: String = '');
var
  Stream: TStream;
begin
  if (FileName = '') then
    FileName := ChangeFileExt(GetModuleName(HInstance), '.log');

  if FileExists(FileName) then
    Stream := TFileStream.Create(FileName, fmOpenReadWrite, fmShareDenyWrite)
  else
    Stream := TFileStream.Create(FileName, fmCreate, fmShareDenyWrite);
  try
    Stream.Seek(0, soFromEnd);
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

initialization
  InitializeCriticalSection(LoggerCriticalSection);

finalization
  Logger.Free;
  InternalLogger := nil;
  DeleteCriticalSection(LoggerCriticalSection);

end.
