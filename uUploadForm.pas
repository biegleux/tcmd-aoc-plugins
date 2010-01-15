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
unit uUploadForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, HttpUploadThreadEx;

type
  TUploadForm = class(TForm)
    ProgressBar: TProgressBar;
    UploadLabel: TLabel;
    ThLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    HttpUploadThread: THttpUploadThreadEx;
    FFileName: String;
    procedure SetFileName(AFileName: String);
    procedure WorkBegin;
    procedure WorkEnd;
    procedure Work;
    procedure DoTerminate(Sender: TObject);
  public
    property FileName: String read FFileName write SetFileName;
  end;

var
  UploadForm: TUploadForm;

implementation

{$R *.dfm}

uses
  IdComponent, uLogger;

resourcestring
  c_uploading = 'Uploading: %d %%';
  c_uploading_done = 'Uploading: Done';
  c_thanks = 'Thank you for uploading!';

procedure TUploadForm.FormCreate(Sender: TObject);
begin
  HttpUploadThread := THttpUploadThreadEx.Create;
  with HttpUploadThread do
  begin
    FreeOnTerminate := True;
    OnTerminate := DoTerminate;
    OnWorkBegin := WorkBegin;
    OnWorkEnd := WorkEnd;
    OnWork := Work;
  end;
  HttpUploadThread.Logger := Logger;

  ThLabel.Caption := c_thanks;
end;

procedure TUploadForm.FormShow(Sender: TObject);
begin
  if Assigned(HttpUploadThread) then
    HttpUploadThread.Resume;
end;

procedure TUploadForm.SetFileName(AFileName: String);
begin
  FFileName := AFileName;
  HttpUploadThread.FileName := FFileName;
end;

procedure TUploadForm.WorkBegin;
begin
  if not Assigned(HttpUploadThread) then
    Exit;
  if HttpUploadThread.WorkMode = wmWrite then
  begin
    ProgressBar.Position := 0;
    ProgressBar.Max := HttpUploadThread.WorkCountMax;
  end;
end;

procedure TUploadForm.WorkEnd;
begin
  if not Assigned(HttpUploadThread) then
    Exit;

  if HttpUploadThread.WorkMode = wmWrite then
    UploadLabel.Caption := c_uploading_done;
end;

procedure TUploadForm.Work;
begin
  if not Assigned(HttpUploadThread) then
    Exit;

  if HttpUploadThread.WorkMode = wmWrite then
  begin
    ProgressBar.Position := HttpUploadThread.WorkCount;
    UploadLabel.Caption := Format(c_uploading, [Round (100 * ProgressBar.Position / ProgressBar.Max)]);
  end;
end;

procedure TUploadForm.DoTerminate(Sender: TObject);
begin
  Self.Close;
end;

procedure TUploadForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{ FreeOnTerminate <- False
  if Assigned(HttpUploadThread) then
  begin
    HttpUploadThread.Terminate;
    HttpUploadThread.Wait;
    HttpUploadThread.Free;
  end;
}
  Action := caFree;
end;

end.
