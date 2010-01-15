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
unit HttpUploadThreadEx;

interface

uses
  IdComponent, Classes, SysUtils, uLogger, threadx;
{ we use threadx unit, because we manipulate threads within DLL } 

type
  THttpUploadThreadEx = class(TThreadEx)
  private
    FOnStatus: TThreadMethod;
    FOnWork: TThreadMethod;
    FOnWorkBegin: TThreadMethod;
    FOnWorkEnd: TThreadMethod;
    FWorkCountMax: Integer;
    FWorkCount: Integer;
    FWorkMode: TWorkMode;
    FURL: String;
    FException: Exception;
    FFileName: String;
    FLogger: TLogger;
    procedure EventOnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: String);
    procedure EventOnWork(ASender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
    procedure EventOnWorkBegin(ASender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Integer);
    procedure EventOnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  protected
    procedure Execute; override;
    procedure HandleThreadException;
  public
    constructor Create;
  published
    property OnStatus: TThreadMethod read FOnStatus write FOnStatus;
    property OnWork: TThreadMethod read FOnWork write FOnWork;
    property OnWorkBegin: TThreadMethod read FOnWorkBegin write FOnWorkBegin;
    property OnWorkEnd: TThreadMethod read FOnWorkEnd write FOnWorkEnd;
    property WorkCountMax: Integer read FWorkCountMax;
    property WorkCount: Integer read FWorkCount;
    property WorkMode: TWorkMode read FWorkMode;
    property URL: String read FURL write FURL;
    property FileName: String read FFileName write FFileName;
    property Logger: TLogger read FLogger write FLogger;
  end;

implementation

uses
  IdHttp, JclCompression, IdMultipartFormData, Windows, Messages, Forms;

constructor THttpUploadThreadEx.Create;
begin
  FWorkCountMax := 0;
  FWorkCount := 0;
  FURL := '';
  FOnStatus := nil;
  FOnWork := nil;
  FOnWorkBegin := nil;
  FOnWorkEnd := nil;
  FException := nil;
  FFileName := '';
  FLogger := nil;
  inherited Create(True);
end;

procedure THttpUploadThreadEx.Execute;
var
  IdHTTP: TIdHTTP;
  srcStream, dstStream: TMemoryStream;
  Archive: TJcl7zCompressArchive;
  postStream: TIdMultipartFormDataStream;
  logStream: TMemoryStream;
begin
  if not Assigned(FLogger) or (FFileName = '') then
    Exit;

  srcStream := TMemoryStream.Create;
  dstStream := TMemoryStream.Create;
  logStream := TMemoryStream.Create;
  Archive := TJcl7zCompressArchive.Create(dstStream);
  postStream := TIdMultipartFormDataStream.Create;
  IdHTTP := TIdHTTP.Create(nil);
  try
    try
      Archive.SetCompressionLevel(0);
      Logger.SaveToStream(logStream);
      logStream.Position := 0;
      srcStream.LoadFromFile(FFileName);
      Archive.AddFile(ExtractFileName(FFileName), srcStream);
      Archive.AddFile('a2lview.log', logStream);
      Archive.Compress;

      postStream.AddObject('file', 'application/x-7z-compressed', dstStream, 'archive.7z');

      IdHTTP.OnStatus := EventOnStatus;
      IdHTTP.OnWork := EventOnWork;
      IdHTTP.OnWorkBegin := EventOnWorkBegin;
      IdHTTP.OnWorkEnd := EventOnWorkEnd;

      IdHTTP.Post('http://www.aoe.cz/services/a2lview/', postStream);
    except
      if not(ExceptObject is EAbort) then
      begin
        FException := Exception(ExceptObject);
        Synchronizer.Synchronize(HandleThreadException);
      end;
    end;
  finally
    FreeAndNil(Archive);
    FreeAndNil(srcStream);
    FreeAndNil(dstStream);
    FreeAndNil(postStream);
    FreeAndNil(logStream);
    FreeAndNil(IdHTTP);
  end;
end;

procedure THttpUploadThreadEx.HandleThreadException;
begin
  try
    // Cancel the mouse capture
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    // Now actually show the exception
    if FException is Exception then
      Application.ShowException(FException)
    else
      SysUtils.ShowException(FException, nil);
  finally
    FException := nil;
  end;
end;

procedure THttpUploadThreadEx.EventOnStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: String);
begin
  if Assigned(FOnStatus) then
    Synchronizer.Synchronize(FOnStatus);
end;

procedure THttpUploadThreadEx.EventOnWork(ASender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
  FWorkMode := AWorkMode;
  FWorkCount := AWorkCount;
  if Assigned(FOnWork) then
    Synchronizer.Synchronize(FOnWork);
end;

procedure THttpUploadThreadEx.EventOnWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  const AWorkCountMax: Integer);
begin
  FWorkMode := AWorkMode;
  FWorkCountMax := AWorkCountMax;
  if Assigned(FOnWorkBegin) then
    Synchronizer.Synchronize(FOnWorkBegin);
end;

procedure THttpUploadThreadEx.EventOnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  FWorkMode := AWorkMode;
  if Assigned(FOnWorkEnd) then
    Synchronizer.Synchronize(FOnWorkEnd);
end;

end.
