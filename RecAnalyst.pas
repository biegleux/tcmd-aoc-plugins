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
{ $DEFINE LIB}
unit {$IFDEF LIB}uRecAnalyst{$ELSE}RecAnalyst{$ENDIF};

interface
{ $DEFINE EXTRACT}

{$DEFINE EXTENDED}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  Classes, Windows, {$IFDEF EXTENDED}Graphics, {$IFNDEF FPC}PNGImage, {$ENDIF}{$ENDIF}SysUtils,
  Contnrs, RecAnalystConsts, MemStream;

type
  TBuildingList = class;

  { TInitialState }
  TInitialState = class(TObject)
  private
    function GetStartingAge: String;
  public
    Food: LongInt;
    Wood: LongInt;
    Stone: LongInt;
    Gold: LongInt;
    StartingAge: TStartingAge;
    HouseCapacity: LongInt;
    Population: LongInt;
    CivilianPop: LongInt;
    MilitaryPop: LongInt;
    ExtraPop: LongInt;
    Position: TPoint;
    constructor Create;
    property sStartingAge: String read GetStartingAge;
  end;

  { TPlayer }
  TPlayer = class(TObject)
  public
    Name: String;
    Index: Integer;
    Human: Boolean;
    Team: Integer;
    Owner: Boolean;
    Civ: PChar;
    CivId: TCivilization;
    ColorId: Integer;
    {$IFDEF EXTENDED}Color: Cardinal;{$ENDIF}
    IsCooping: Boolean;
    FeudalTime: Integer;
    CastleTime: Integer;
    ImperialTime: Integer;
    ResignTime: Integer;
    Buildings: TBuildingList;
    InitialState: TInitialState;
    constructor Create;
    destructor Destroy; override;
    procedure SetCiv(const ACiv: TCivilization);
    {$IFDEF EXTENDED}
    procedure SetColor(const AIndex: Integer);
    {$ENDIF}
  end;

  { TPlayerList }
  TPlayerList = class(TObject)
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetPlayer(Index: Integer): TPlayer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddPlayer(Player: TPlayer): Integer;
    function GetPlayerByIndex(Index: Integer): TPlayer;
    procedure Clear;
    property Items[Index: Integer]: TPlayer read GetPlayer; default;
    property Count: Integer read GetCount;
  end;

  { TTeam }
  TTeam = class(TPlayerList)
  private
    FIndex: Integer;
  public
    constructor Create;
    function AddPlayer(Player: TPlayer): Integer;
    property Index: Integer read FIndex;
  end;

  { TTeamList }
  TTeamList = class(TObject)
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetTeam(Index: Integer): TTeam;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTeam(Team: TTeam): Integer;
    function GetTeamByIndex(Index: Integer): TTeam;
    procedure Clear;
    property Items[Index: Integer]: TTeam read GetTeam; default;
    property Count: Integer read GetCount;
  end;

  { TVictory }
  TVictory = class(TObject)
  private
    function GetVictoryString: String;
  public
    TimeLimit: LongInt;
    ScoreLimit: LongInt;
    VictoryCondition: TVictoryCondition;
    constructor Create;
    property VictoryString: String read GetVictoryString;
  end;

  { TGameSettings }
  TGameSettings = class(TObject)
  private
    FIsMgl: Boolean; { see GetDifficultyLevel }
    FGameType: TGameType;
    FMapStyle: TMapStyle;
    FDifficultyLevel: TDifficultyLevel;
    FGameSpeed: TGameSpeed;
    FRevealMap: TRevealMap;
    FMapSize: TMapSize;
    function GetGameType: PChar;
    function GetMapStyle: PChar;
    function GetDifficultyLevel: PChar;
    function GetGameSpeed: PChar;
    function GetRevealMap: PChar;
    function GetMapSize: PChar;
    function GetIsScenario: Boolean;
  public
    Map: String;
    Players: String;
    POV: String;
    POVEx: String;
    ObjectivesString: String;

    MapId: Byte;
    PopLimit: Integer;
    LockDiplomacy: Boolean; { LockTeams }
    bPOV: Byte;
    bPlayers: Byte;
    PlayTime: Integer;

    InGameCoop: Boolean;
    Owner: TPlayer;
    ScFileName: String;
    Victory: TVictory;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property GameType: TGameType read FGameType write FGameType;
    property MapStyle: TMapStyle read FMapStyle write FMapStyle;
    property DifficultyLevel: TDifficultyLevel read FDifficultyLevel write FDifficultyLevel;
    property GameSpeed: TGameSpeed read FGameSpeed write FGameSpeed;
    property RevealMap: TRevealMap read FRevealMap write FRevealMap;
    property MapSize: TMapSize read FMapSize write FMapSize;
    { write access because of back-end, don't like it, but can be useful }

    property sGameType: PChar read GetGameType;
    property sMapStyle: PChar read GetMapStyle;
    property sDifficultyLevel: PChar read GetDifficultyLevel;
    property sGameSpeed: PChar read GetGameSpeed;
    property sRevealMap: PChar read GetRevealMap;
    property sMapSize: PChar read GetMapSize;
    property IsScenario: Boolean read GetIsScenario;
  end;

  { TResearch }
  TResearch = class(TObject)
  private
    FItemId: Integer;
    function GetName: String;
    function GetResName: String;
  public
    Id: Integer;
    Time: LongInt;
    PlayerId: Integer;
    property Name: String read GetName;
    property ResName: String read GetResName;
    constructor Create;
  end;

  { TTribute }
  TTribute = class(TObject)
    Time: Integer;
    PlayerFrom: TPlayer;
    PlayerTo: TPlayer;
    ResourceId: TResourceId;
    Amount: Integer;
    Fee: Single;
  end;

  { TTrainedUnit }
  TTrainedUnit = class(TObject)
  private
    FItemId: Integer;
    function GetName: String;
    function GetResName: String;
  public
    Id: Integer;
    Count: Integer;
    property Name: String read GetName;
    property ResName: String read GetResName;
    constructor Create;
  end;

  { TTrainedUnitList }
  TTrainedUnitList = class(TObjectList)
  private
    function GetItem(Index: Integer): TTrainedUnit;
    procedure SetItem(Index: Integer; ATrainedUnit: TTrainedUnit);
  public
    function GetUnit(const Id: Integer): TTrainedUnit;
    property Items[Index: Integer]: TTrainedUnit read GetItem write SetItem; default;
  end;

  { TBuilding }
  TBuilding = class(TTrainedUnit)
  private
    function GetName: String;
    function GetResName: String;
  public
    property Name: String read GetName;
    property ResName: String read GetResName;
  end;

  { TBuildingList }
  TBuildingList = class(TObjectList)
  private
    function GetItem(Index: Integer): TBuilding;
    procedure SetItem(Index: Integer; ATrainedUnit: TBuilding);
  public
    function GetBuilding(const Id: Integer): TBuilding;
    property Items[Index: Integer]: TBuilding read GetItem write SetItem; default;
  end;

  { TChatMessage }
  TChatMessage = class(TObject)
    Time: Integer;
    Player: TPlayer;
    Msg: String;
    constructor Create;
  end;

  { TImageMapRes }
  TImageMapItem = class(TObject)
    Coordinates: TRect;
    Hint: String;
  end;

  TUnitObject = class(TObject)
    Owner: Integer;
    Id: Integer;
    Position: TPoint;
  end;

  TGaiaObject = class(TUnitObject);

  { TRecAnalyst }
  ERecAnalystException = class(Exception);

  TRecAnalyst = class(TObject)
  protected
    FIsMgl: Boolean;
    FIsMgx: Boolean;
    FHeaderStream: TMemStream;
    FBodyStream: TMemStream;
    FMapData: array of array of Integer;
    FMapWidth: LongInt;
    FMapHeight: LongInt;
    FAnalyzeTime: Integer;

    FShowPositions: Boolean;
    FGameVersion: TGameVersion;

    next_pos: Integer;
    objectives_pos: Int64;

    FAnalyzed: Boolean;
    FKeepStreams: Boolean;  { if false, header and body streams will be cleaned after analysis, default: false }
    GaiaObjects: TObjectList;
    PlayerObjects: TObjectList;
    {$IFDEF LIB}
    FLastError: Integer;
    {$ENDIF}
    {$IFNDEF LIB}
    FZeroHeaderLen: Boolean;
    {$ENDIF}
    Queue: TQueue;

    function ExtractStreams: Boolean; virtual;
    function AnalyzeHeader: Boolean; virtual;
    function AnalyzeBody: Boolean; virtual;
    procedure PostAnalyze; virtual;
    function GetGameVersionStr: PChar;
    function ReadPlayerInfoBlockEx: Boolean;
  public
    FileName: String;
    GameSettings: TGameSettings;
    PlayerList: TPlayerList;
    Teams: TTeamList;
    PreGameChat: TStringList;
    InGameChat: TStringList;
    Tributes: TObjectList;
    Units: TTrainedUnitList;
    Researches: TObjectList;
    PreGameChatMessages: TObjectList;
    InGameChatMessages: TObjectList;
    CommentString: String;
    constructor Create;
    destructor Destroy; override;
    class function GameTimeToString(const Time: Integer): String;
    function Analyze: Boolean;
    procedure BuildTeams;
    {$IFDEF EXTENDED}
    function GenerateMap(var Bitmap: TBitmap; const Width: Integer; const Height: Integer; bgColor: TColor): Boolean;
    {$IFNDEF LIB}
    function GenerateResearches(var Bitmap: TBitmap; var ImageMap: TObjectList): Boolean;
    {$ENDIF}
    {$ENDIF}
    procedure Reset;
    procedure Build(const AFileName: String);
    function AddComment(const Comment: String = ''): Boolean;
    {$IFDEF LIB}
    function GetLastErrorCode: Integer;
    function GetLastErrorMsg: PChar;
    class function ErrorCodeToString(const ErrorCode: Integer): PChar;
    {$ENDIF}
    property AnalyzeTime: Integer read FAnalyzeTime;
    property ShowPositions: Boolean read FShowPositions write FShowPositions;
    property GameVersion: TGameVersion read FGameVersion write FGameVersion;
    { write access because of back-end, don't like it, but can be useful }
    property sGameVersion: PChar read GetGameVersionStr;
    property IsAOK: Boolean read FIsMgl;
    property IsAOC: Boolean read FIsMgx;
    property KeepStreams: Boolean read FKeepStreams write FKeepStreams;
    {$IFNDEF LIB}
    property ZeroHeaderLen: Boolean read FZeroHeaderLen;
    {$ENDIF}
    {$IFDEF LIB}
    property Analyzed: Boolean read FAnalyzed;
    {$ENDIF}
  end;

{$IFDEF LIB}
{ RecAnalyst Error Codes }
const
  RECANALYST_OK          = 0;
  RECANALYST_NOFILE      = 1;
  RECANALYST_FILEEXT     = 2;
  RECANALYST_EMPTYHEADER = 3;
  RECANALYST_DECOMP      = 4;
  RECANALYST_FILEREAD    = 5;
  RECANALYST_FILEOPEN    = 6;
  RECANALYST_UNKNOWN     = 7;
  RECANALYST_HEADLENREAD = 8;
  RECANALYST_NOTRIGG     = 9;
  RECANALYST_NOGAMESETS  = 10;
  RECANALYST_FILECREATE  = 11;
  RECANALYST_COMP        = 12;
{$ENDIF}

implementation

uses
  {$IFDEF FPC}paszlib{$ELSE}ZlibEx{$ENDIF}, {$IFDEF EXTENDED}BitmapEx, {$ENDIF}Math{$IFNDEF LIB}, Context,
  uLogger{$ENDIF};

resourcestring
  c_filenotspecified = 'No file has been specified for analyzing.';
  c_cannotopenfile = {$IFDEF LIB}'Cannot open file.'{$ELSE}'Cannot open file "%s".'{$ENDIF};
  c_cannotreadsection = 'Cannot read sections.';
  c_cannotdecompress = 'Cannot decompress header section.';
  c_cannotcompress = 'Cannot compress header section.';
  c_unknown = 'Unknown error.';
  c_wrongfileext = 'Wrong file extension, file format is not supported.';
  c_headerlenreaderror = 'Unable to read the header length.';
  c_headerlenempty = 'Header length is zero.';
  c_cannotcreatefile = {$IFDEF LIB}'Cannot create file.'{$ELSE}'Cannot create file "%s".'{$ENDIF};
  c_triggerinfonotfound = '"Trigger Info" block has not been found.';
  c_gamesettingsnotfound = '"Game Settings" block has not been found.';

  c_victory_timelimit = '%s (%d years)';
  c_victory_scorelimit = '%s (%d)';
  c_feudal_age_advance = '%s advanced to Feudal Age';
  c_castle_age_advance = '%s advanced to Castle Age';
  c_imperial_age_advance = '%s advanced to Imperial Age';
  c_resigned = '%s resigned';
  c_min = '%d. min';

{$IFDEF LIB}
const
  ErrorMessages: array[RECANALYST_OK..RECANALYST_COMP] of String = (
    '',
    c_filenotspecified,
    c_wrongfileext,
    c_headerlenempty,
    c_cannotdecompress,
    c_cannotreadsection,
    c_cannotopenfile,
    c_unknown,
    c_headerlenreaderror,
    c_triggerinfonotfound,
    c_gamesettingsnotfound,
    c_cannotcreatefile,
    c_cannotcompress
  );
{$ENDIF}

function UnitsCompare(Item1, Item2: Pointer): Integer; forward;
function ResearchesCompare(Item1, Item2: Pointer): Integer; forward;
function ChatCompare(Item1, Item2: Pointer): Integer; forward;
function GaiaObjectsCompare(Item1, Item2: Pointer): Integer; forward;
{TODO: function ChatSortCompare(List: TStringList; Index1, Index2: Integer): Integer; forward;}
function ResearchById(const Id: Integer): Integer; forward;
function UnitById(const Id: Integer): Integer; forward;
function BuildingById(const Id: Integer): Integer; forward;
function MapById(const Id: Integer): Integer; forward;
{$IFDEF LIB}
function InArray(const Ary: array of Integer; Value: Integer): Integer; forward;
{$ENDIF}
{$IFDEF FPC}
function ZDecompressStream2(inStream, outStream: TStream; windowBits: Integer): Integer; forward;
function ZCompressStream2(inStream, outStream: TStream; level, windowBits, memLevel, strategy : Longint): Integer; forward;
{$ENDIF}

{ TResearch }
constructor TResearch.Create;
begin
  FItemId := -1;
  Id := 0;
  PlayerId := 0;
end;

function TResearch.GetName: String;
begin
  if (FItemId <> -1) then
  begin
    Result := RESEARCHES[FItemId].Name;
    Exit;
  end;

  FItemId := ResearchById (Id);
  if (FItemId <> -1) then
    Result := RESEARCHES[FItemId].Name
  else
    Result := '';
end;

function TResearch.GetResName: String;
begin
  if (FItemId <> -1) then
  begin
    Result := RESEARCHES[FItemId].ResName;
    Exit;
  end;

  FItemId := ResearchById (Id);
  if (FItemId <> -1) then
    Result := RESEARCHES[FItemId].ResName
  else
    Result := '';
end;

{ TTrainedUnit }
constructor TTrainedUnit.Create;
begin
  FItemId := -1;
  Id := 0;
  Count := 0;
end;

function TTrainedUnit.GetName: String;
begin
  if (FItemId <> -1) then
  begin
    Result := UNITS[FItemId].Name;
    Exit;
  end;

  FItemId := UnitById (Id);
  if (FItemId <> -1) then
    Result := UNITS[FItemId].Name
  else
    Result := '';
end;

function TTrainedUnit.GetResName: String;
begin
  if (FItemId <> -1) then
  begin
    Result := UNITS[FItemId].ResName;
    Exit;
  end;

  FItemId := UnitById (Id);
  if (FItemId <> -1) then
    Result := UNITS[FItemId].ResName
  else
    Result := '';
end;

{ TTrainedUnitList }
function TTrainedUnitList.GetItem(Index: Integer): TTrainedUnit;
begin
  Result := inherited GetItem (Index) as TTrainedUnit;
end;

procedure TTrainedUnitList.SetItem(Index: Integer; ATrainedUnit: TTrainedUnit);
begin
  inherited SetItem (Index, ATrainedUnit);
end;

function TTrainedUnitList.GetUnit(const Id: Integer): TTrainedUnit;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Id = Id then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

{ TBuilding }
function TBuilding.GetName: String;
begin
  if (FItemId <> -1) then
  begin
    Result := BUILDINGS[FItemId].Name;
    Exit;
  end;

  FItemId := BuildingById (Id);
  if (FItemId <> -1) then
    Result := BUILDINGS[FItemId].Name
  else
    Result := '';
end;

function TBuilding.GetResName: String;
begin
  if (FItemId <> -1) then
  begin
    Result := BUILDINGS[FItemId].ResName;
    Exit;
  end;

  FItemId := BuildingById (Id);
  if (FItemId <> -1) then
    Result := BUILDINGS[FItemId].ResName
  else
    Result := '';
end;

{ TBuildingList }
function TBuildingList.GetItem(Index: Integer): TBuilding;
begin
  Result := inherited GetItem (Index) as TBuilding;
end;

procedure TBuildingList.SetItem(Index: Integer; ATrainedUnit: TBuilding);
begin
  inherited SetItem (Index, ATrainedUnit);
end;

function TBuildingList.GetBuilding(const Id: Integer): TBuilding;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Id = Id then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

{ TChatMessage }
constructor TChatMessage.Create;
begin
  Time := 0;
  Player := nil;
  Msg := '';
end;

{ TInitialState }
constructor TInitialState.Create;
begin
  Food := 0;
  Wood := 0;
  Stone := 0;
  Gold := 0;
  StartingAge := Low (TStartingAge);
  HouseCapacity := 0;
  Population := 0;
  CivilianPop := 0;
  MilitaryPop := 0;
  ExtraPop := 0;
  FillChar (Position, SizeOf (Position), 0);
end;

function TInitialState.GetStartingAge: String;
begin
  Result := '';
  if (StartingAge >= Low (STARTING_AGES)) and (StartingAge <= High (STARTING_AGES)) then
    Result := STARTING_AGES[StartingAge];
end;

{ TPlayer }
constructor TPlayer.Create;
begin
  Name := '';
  Index := -1;
  Human := False;
  Team := -1;
  Owner := False;
  Civ := '';
  CivId := cNone;
  ColorId := -1;
  {$IFDEF EXTENDED}Color := $00FFFFFF;{$ENDIF}
  IsCooping := False;
  FeudalTime := 0;
  CastleTime := 0;
  ImperialTime := 0;
  ResignTime := 0;
  Buildings := TBuildingList.Create;
  InitialState := TInitialState.Create;
end;

destructor TPlayer.Destroy;
begin
  Buildings.Free;
  InitialState.Free;
end;

procedure TPlayer.SetCiv(const ACiv: TCivilization);
begin
  if (ACiv in [Low (CIVS)..High (CIVS)]) then
    Civ := CIVS[ACiv].Name;
end;

{$IFDEF EXTENDED}
procedure TPlayer.SetColor(const AIndex: Integer);
begin
  if AIndex in [Low (COLORS)..High (COLORS)] then
    Color := COLORS[AIndex];
end;
{$ENDIF}
{ TPlayerList }
constructor TPlayerList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TPlayerList.Destroy;
begin
  FList.Free;
end;

function TPlayerList.AddPlayer(Player: TPlayer): Integer;
begin
  Result := FList.Add (Player);
end;

function TPlayerList.GetPlayer(Index: Integer): TPlayer;
begin
  if (Index < 0) or (Index >= FList.Count) then
    Result := nil
  else
    Result := FList[Index] as TPlayer;
end;

function TPlayerList.GetPlayerByIndex(Index: Integer): TPlayer;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if (TPlayer(FList[i]).Index = Index) then
    begin
      Result := TPlayer(FList[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TPlayerList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TPlayerList.Clear;
begin
  FList.Clear;
end;

{ TTeam }
constructor TTeam.Create;
begin
  FList := TObjectList.Create (False);
  FIndex := -1;
end;

function TTeam.AddPlayer(Player: TPlayer): Integer;
begin
  Result := inherited AddPlayer (Player);
  if (FIndex = -1) then
    FIndex := Player.Team;
end;

{ TTeamList }
constructor TTeamList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TTeamList.Destroy;
begin
  FList.Free;
end;

function TTeamList.AddTeam(Team: TTeam): Integer;
begin
  Result := FList.Add (Team);
end;

function TTeamList.GetTeam(Index: Integer): TTeam;
begin
  if (Index < 0) or (Index >= FList.Count) then
    Result := nil
  else
    Result := FList[Index] as TTeam;
end;

function TTeamList.GetTeamByIndex(Index: Integer): TTeam;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if ((FList[i] as TTeam).Index = Index) then
    begin
      Result := FList[i] as TTeam;
      Exit;
    end;
  end;
  Result := nil;
end;

function TTeamList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TTeamList.Clear;
begin
  FList.Clear;
end;

{ TVictory }
constructor TVictory.Create;
begin
  VictoryCondition := Low (TVictoryCondition);
  TimeLimit := 0;
  ScoreLimit := 0;
end;

function TVictory.GetVictoryString: String;
begin
  Result := '';
  if not (VictoryCondition in [Low (VICTORY_CONDITIONS)..High (VICTORY_CONDITIONS)]) then
    Exit;
  Result := VICTORY_CONDITIONS[VictoryCondition];

  case VictoryCondition of
    vcTimeLimit:
      begin
        if (TimeLimit <> 0) then
          Result := Format (c_victory_timelimit, [Result, TimeLimit]);
      end;
    vcScoreLimit:
      begin
        if (ScoreLimit <> 0) then
          Result := Format (c_victory_scorelimit, [Result, ScoreLimit]);
      end;
  end;
end;

{ TGameSettings }
constructor TGameSettings.Create;
begin
  Clear;
  Victory := TVictory.Create;
end;

destructor TGameSettings.Destroy;
begin
  Owner := nil;
  Victory.Free;
end;

procedure TGameSettings.Clear;
begin
  FIsMgl := False;
  Map     := '';
  Players := '';
  POV     := '';
  POVEx   := '';
  ObjectivesString := '';

  FGameType        := Low (TGameType);
  FMapStyle        := Low (TMapStyle);
  FDifficultyLevel := Low (TDifficultyLevel);
  FGameSpeed       := Low (TGameSpeed);
  FRevealMap       := Low (TRevealMap);
  FMapSize         := Low (TMapSize);

  MapId         := 0;
  PopLimit      := 0;
  bPOV          := 0;
  bPlayers      := 0;
  PlayTime      := 0;
  LockDiplomacy := False;
  InGameCoop := False;
  Owner      := nil;
  ScFileName := '';
end;

function TGameSettings.GetGameType: PChar;
begin
  Result := '';
  if (FGameType in [Low (GAME_TYPES)..High (GAME_TYPES)]) then
  Result := GAME_TYPES[FGameType];
end;

function TGameSettings.GetMapStyle: PChar;
begin
  Result := '';
  if (FMapStyle in [Low (MAP_STYLES)..High (MAP_STYLES)]) then
    Result := MAP_STYLES[FMapStyle];
end;

function TGameSettings.GetDifficultyLevel: PChar;
begin
  Result := '';
  if FIsMgl then
  begin
    if FDifficultyLevel in [Low (AOK_DIFFICULTY_LEVELS)..High (AOK_DIFFICULTY_LEVELS)] then
      Result := AOK_DIFFICULTY_LEVELS[FDifficultyLevel];
  end else
  begin
    if FDifficultyLevel in [Low (AOC_DIFFICULTY_LEVELS)..High (AOC_DIFFICULTY_LEVELS)] then
      Result := AOC_DIFFICULTY_LEVELS[FDifficultyLevel];
  end;
end;

function TGameSettings.GetGameSpeed: PChar;
begin
  Result := '';
  if (Ord (FGameSpeed) in [100, 150, 200]) then
    Result := GAME_SPEEDS[(Ord (FGameSpeed) - 100) div 50];
end;

function TGameSettings.GetRevealMap: PChar;
begin
  Result := '';
  if (FRevealMap in [Low (REVEAL_SETTINGS)..High (REVEAL_SETTINGS)]) then
    Result := REVEAL_SETTINGS[FRevealMap];
end;

function TGameSettings.GetMapSize: PChar;
begin
  Result := '';
  if (FMapSize in [Low (MAP_SIZES)..High (MAP_SIZES)]) then
    Result := MAP_SIZES[FMapSize];
end;

function TGameSettings.GetIsScenario: Boolean;
begin
  Result := (FGameType = gtScenario);
end;

{ TRecAnalyst }
constructor TRecAnalyst.Create;
begin
  with Self do
  begin
    FIsMgl := False;
    FIsMgx := False;
    FHeaderStream := TMemStream.Create;
    FBodyStream := TMemStream.Create;
    FMapWidth := 0;
    FMapHeight := 0;
    FileName := '';
    FMapData := nil;
    GameSettings := TGameSettings.Create;
    PlayerList := TPlayerList.Create;
    Teams := TTeamList.Create;

    PreGameChat := TStringList.Create;
    InGameChat := TStringList.Create;
    Tributes := TObjectList.Create;
    FAnalyzeTime := 0;
    FShowPositions := True;
    FGameVersion := gvUnknown;
    Researches := TObjectList.Create;
    Units := TTrainedUnitList.Create;
    PreGameChatMessages := TObjectList.Create;
    InGameChatMessages := TObjectList.Create;

    next_pos := 0;
    objectives_pos := 0;
    {$IFNDEF LIB}FZeroHeaderLen := False;{$ENDIF}
    FAnalyzed := False;
    FKeepStreams := False;
    CommentString := '';

    GaiaObjects := TObjectList.Create;
    PlayerObjects := TObjectList.Create;
    Queue := TQueue.Create;
    {$IFDEF LIB}FLastError := RECANALYST_OK;{$ENDIF}
  end;
end;

destructor TRecAnalyst.Destroy;
begin
  FHeaderStream.Free;
  FBodyStream.Free;
  GameSettings.Free;
  PlayerList.Free;
  Teams.Free;
  PreGameChat.Free;
  InGameChat.Free;
  Tributes.Free;
  Units.Free;
  Researches.Free;
  PreGameChatMessages.Free;
  InGameChatMessages.Free;
  GaiaObjects.Free;
  PlayerObjects.Free;
  Queue.Free;
  SetLength (FMapData, 0);
end;

class function TRecAnalyst.GameTimeToString(const Time: Integer): String;
var
  hour, minute, second: Integer;
  s: String;
begin
  if (Time = 0) then
  begin
    Result := '';
    Exit;
  end;
  hour := Time div 1000 div 3600;
  minute := Time div 1000 div 60 mod 60;
  second := Time div 1000 mod 60;

  Result := '';
  s := IntToStr (hour);
  if (hour < 10) then
    s := '0' + s;
  Result := Result + s + ':';
  s := IntToStr (minute);
  if (minute < 10) then
    s := '0' + s;
  Result := Result + s + ':';
  s := IntToStr (second);
  if (second < 10) then
    s := '0' + s;
  Result := Result + s;
end;

function TRecAnalyst.ExtractStreams: Boolean;
var
  ms, inStream: TMemoryStream;
  header_len: Integer;
const
  MGL_EXT = '.mgl';
  MGX_EXT = '.mgx';
begin
  Result := False;

  if (FileName = '') then
  {$IFDEF LIB}
  begin
    FLastError := RECANALYST_NOFILE;
    Exit;
  end;
  {$ELSE}
    raise ERecAnalystException.Create (c_filenotspecified);
  {$ENDIF}

  if (LowerCase (ExtractFileExt (FileName)) = MGL_EXT) then
    FIsMgl := True
  else if (LowerCase (ExtractFileExt (FileName)) = MGX_EXT) then
    FIsMgx := True
  else
  {$IFDEF LIB}
  begin
    FLastError := RECANALYST_FILEEXT;
    Exit;
  end;
  {$ELSE}
    raise ERecAnalystException.Create (c_wrongfileext);
  {$ENDIF}

  ms := TMemoryStream.Create;
  inStream := TMemoryStream.Create;
  try
    try
      ms.LoadFromFile (FileName);
      ms.Seek (0, soFromBeginning);

      if (ms.Read (header_len, SizeOf (header_len)) < SizeOf (header_len)) then
      {$IFDEF LIB}
      begin
        FLastError := RECANALYST_HEADLENREAD;
        Exit;
      end;
      {$ELSE}
        raise ERecAnalystException.Create (c_headerlenreaderror);
      {$ENDIF}

      if (header_len = 0) then
      begin
        {$IFDEF LIB}
        FLastError := RECANALYST_EMPTYHEADER;
        Exit;
        {$ELSE}
        FZeroHeaderLen := True;
        raise ERecAnalystException.Create (c_headerlenempty);
        {$ENDIF}
      end;

      { skip next_pos }
      if FIsMgx then
        ms.Read (next_pos, SizeOf (next_pos));

      if FIsMgx then
        Dec (header_len, SizeOf (next_pos) + SizeOf (header_len))
      else
        Dec (header_len, SizeOf (header_len));

      inStream.CopyFrom (ms, header_len);
      instream.Seek (0, soFromBeginning);

      {$IFDEF FPC}
      if (ZDecompressStream2 (inStream, FHeaderStream, -15) < 0) then
      {$IFDEF LIB}
      begin
        FLastError := RECANALYST_DECOMP;
        Exit;
      end;
      {$ELSE}
        raise ERecAnalystException.Create (c_cannotdecompress);
      {$ENDIF}
      // zError (code)
      {$ELSE}
      ZDecompressStream2 (inStream, FHeaderStream, -15);
      {$ENDIF}

      if FIsMgx then
        FBodyStream.CopyFrom (ms, ms.Size - header_len - SizeOf (header_len) - 4 {next_pos})
      else
        FBodyStream.CopyFrom (ms, ms.Size - header_len - SizeOf (header_len));

      Result := True;
    except
      {$IFNDEF LIB}
      on ERecAnalystException do
        raise;
      {$ENDIF}
      on EReadError do
        {$IFDEF LIB}FLastError := RECANALYST_FILEREAD{$ELSE}raise ERecAnalystException.Create (c_cannotreadsection){$ENDIF};
      on EFOpenError do
        {$IFDEF LIB}FLastError := RECANALYST_FILEOPEN{$ELSE}raise ERecAnalystException.CreateFmt (c_cannotopenfile, [FileName]){$ENDIF};
      {$IFNDEF FPC}
      on EZDecompressionError do
        {$IFDEF LIB}FLastError := RECANALYST_DECOMP{$ELSE}raise ERecAnalystException.Create (c_cannotdecompress){$ENDIF};
      {$ENDIF}
      else
        {$IFDEF LIB}FLastError := RECANALYST_UNKNOWN{$ELSE}raise ERecAnalystException.Create (c_unknown){$ENDIF};
    end;
  finally
    FreeAndNil (ms);
    FreeAndNil (inStream);
  end;
  {$IFDEF EXTRACT}
  try
    FHeaderStream.SaveToFile (FileName + '.header');
    FBodyStream.SaveToFile (FileName + '.body');
  except
    //
  end;
  {$ENDIF}
end;

function TRecAnalyst.AnalyzeHeader: Boolean;
type
  TLangRec = record
    lang: String;
    data: array[0..13] of Byte;
    len: Byte;
  end;

const
  constant2: array[0..7] of Char = (#$9A, #$99, #$99, #$99, #$99, #$99, #$F9, #$3F);
  separator: array[0..3] of Char = (#$9D, #$FF, #$FF, #$FF);
  unknown_const2: array[0..5] of Char = (#$98, #$9E, #$00, #$00, #$02, #$0B); // not used
  con1: array[0..1] of Char = (#$3A, #$20);
  con2: array[0..1] of Char = (#$A1, #$47);
  scenario_constant: array[0..3] of Char = (#$F6, #$28, #$9C, #$3F);
  aok_separator: array[0..3] of Char = (#$9A, #$99, #$99, #$3F);
//  mgl_magic_constant: array[0..6] of Char = (#$00, #$16, #$BD, #$00, #$00, #$00, #$21);
//  mgx_magic_constant: array[0..6] of Char = (#$00, #$16, #$C6, #$00, #$00, #$00, #$21);
  player_info_end_separator: array[0..11] of Char = (
    #$00, #$0B, #$00, #$02, #$00, #$00, #$00, #$02, #$00, #$00, #$00, #$0B);
//  mgl_pi_separator: array[0..3] of Char = (#$98, #$9E, #$00, #$00);
//  mgx_pi_separator: array[0..3] of Char = (#$4C, #$27, #$00, #$00);

  LANG_NUM = 12;
  LANGUAGES: array[0..LANG_NUM - 1] of TLangRec = (
    (lang: 'en'; data: ($4D, $61, $70, $20, $54, $79, $70, $65, $00, $00, $00, $00, $00, $00); len: 8),
    (lang: 'cz'; data: ($54, $79, $70, $20, $6D, $61, $70, $79, $00, $00, $00, $00, $00, $00); len: 8),
		(lang: 'jp'; data: ($83, $7D, $83, $62, $83, $76, $82, $CC, $8E, $ED, $97, $DE, $00, $00); len: 12),
		(lang: 'cn'; data: ($B5, $D8, $CD, $BC, $C0, $E0, $D0, $CD, $00, $00, $00, $00, $00, $00); len: 8),
		(lang: 'sp'; data: ($54, $69, $70, $6F, $20, $64, $65, $20, $6D, $61, $70, $61, $00, $00); len: 12),
		(lang: 'de'; data: ($4B, $61, $72, $74, $65, $6E, $74, $79, $70, $00, $00, $00, $00, $00); len: 9),
		(lang: 'cn2'; data: ($A6, $61, $B9, $CF, $C3, $FE, $A7, $4F, $00, $00, $00, $00, $00, $00); len: 8),
		(lang: 'kr'; data: ($C7, $A5, $C1, $D8, $0A, $C0, $DA, $BF, $F8, $00, $00, $00, $00, $00); len: 9),
		(lang: 'fr'; data: ($54, $79, $70, $65, $20, $64, $65, $20, $63, $61, $72, $74, $65, $A0); len: 14),
		(lang: 'it'; data: ($54, $69, $70, $6F, $20, $64, $69, $20, $6D, $61, $70, $70, $61, $00); len: 13),
		(lang: 'sp2'; data: ($54, $69, $70, $6F, $20, $64, $65, $20, $4D, $61, $70, $61, $00, $00); len: 12),
		(lang: 'ur'; data: ($D2, $E8, $EF, $20, $CA, $E0, $F0, $F2, $FB, $00, $00, $00, $00, $00); len: 9));
var
  buff: array[0..7] of Byte;
  version: array[0..7] of Char;
  trigger_info_pos: Longint;
  game_settings_pos: Longint;
  scenario_header_pos: Longint;
  map_id: Longint;
  difficulty: Longint;
  i, j, x, y: Integer;
  player_data_index, human, name_len: Longint;
  buff256: array[0..255] of Char;
  buff65536: array[0..65535] of Char;
  Player, Player_: TPlayer;
  num_trigger: Longint;
  reveal_map, map_size, pop_limit: Longint;
  game_type, lock_diplomacy: Byte;
  lock_teams: Boolean;
  num_chat, chat_len, throw_chat: Longint;
  include_ai: Boolean;
  string_length: Longint;
  num_string, num_rule: Word;
  game_speed: Longint;
  rec_player_ref: Word;
  num_player: Byte;
  map_size_x, map_size_y: Longint;
  num_unknown_data, num_float: Longint;
  terrain_id, elevation: Byte;
  init_camera_pos_x, init_camera_pos_y: Single;
  civilization, player_color: Byte;
  map_found: Boolean;
  desc_len, num_effect, num_selected_object, text_len, sound_len: Longint;
  ChatMessage: TChatMessage;
  data6: Single;
  num_condition: Longint;
  team_indexes: array[0..7] of Byte;
  original_sc_filename_len, instruction_len: Word;
  separator_ptr: Pointer;
  food, wood, stone, gold, headroom, population, civilian_pop, military_pop: Single;
  unknown25, victory_condition: Longint;
  is_timelimit: Byte;
  time_limit: Single;
  len: Word;
  num_data, num_couples, map_size_x2, map_size_y2, num_unknown_data2: Longint;
begin
  Result := False;
  FillChar(buff, SizeOf(buff), $00);
  FillChar(buff256, SizeOf(buff256), #0);

  with FHeaderStream do
  begin
    Seek(0, soFromBeginning);

    { getting version }
    FillChar(version, SizeOf(version), #0);
    ReadBuffer(version, SizeOf(version));
    if (version = VER_94) then
      FGameVersion := gvAOC
    else if (version = VER_93) then
      FGameVersion := gvAOK
    else if (version = TRL_93) and FIsMgx then
      FGameVersion := gvAOCTrial
    else if (version = TRL_93) and FIsMgl then
      FGameVersion := gvAOKTrial
    else
      FGameVersion := gvUnknown;

    case FGameVersion of
      gvAOK, gvAOKTrial:
        begin
          FIsMgl := True;
          FIsMgx := False;
        end;
      gvAOC, gvAOCTrial:
        begin
          FIsMgl := False;
          FIsMgx := True;
        end;
    end;
    GameSettings.FIsMgl := FIsMgl;

    { getting Trigger_info position }
    Seek(-SizeOf(constant2), soFromEnd);
    trigger_info_pos := 0;
    repeat
      ReadBuffer(buff, SizeOf(constant2));
      if CompareMem(@buff, @constant2, SizeOf(constant2)) then
      begin
        trigger_info_pos := Position;
        Break;
      end;
      Seek(-(SizeOf(constant2) + 1));
    until (Position < 0);

    if (trigger_info_pos = 0) then
    {$IFDEF LIB}
    begin
      FLastError := RECANALYST_NOTRIGG;
      Exit;
    end;
    {$ELSE}
      raise ERecAnalystException.Create(c_triggerinfonotfound);
    {$ENDIF}

    { getting Game_settings position }
    game_settings_pos := 0;
    repeat
      ReadBuffer(buff, SizeOf(separator));
      if CompareMem(@buff, @separator, SizeOf(separator)) then
      begin
        game_settings_pos := Position;
        Break;
      end;
      Seek(-(SizeOf(separator) + 1));
    until (Position < 0);

    if (game_settings_pos = 0) then
    {$IFDEF LIB}
    begin
      FLastError := RECANALYST_NOGAMESETS;
      Exit;
    end;
    {$ELSE}
      raise ERecAnalystException.Create(c_gamesettingsnotfound);
    {$ENDIF}

    { getting Scenario_header position }
    scenario_header_pos := 0;
    if FIsMgx then
      separator_ptr := @scenario_constant
    else
      separator_ptr := @aok_separator;
    { note: SizeOf(scenario_constant) = SizeOf(aok_separator) }

    repeat
      ReadBuffer(buff, SizeOf(scenario_constant));
      if CompareMem(@buff, separator_ptr, SizeOf(scenario_constant)) then
      begin
        scenario_header_pos := Position - SizeOf(scenario_constant) - 4 {next_unit_id};
        Break;
      end;
      Seek(-(SizeOf(scenario_constant) + 1));
    until (Position < 0);

    { getting Game_Settings data }
    { skip negative[2] }
    Seek(game_settings_pos + 8, soFromBeginning);
    if FIsMgx then
      ReadInt32(map_id);

    ReadInt32(difficulty);
    ReadBool(lock_teams); { duplicated data, see lock_diplomacy }

    if FIsMgx then
    begin
      i := MapById(map_id);
      if (i <> -1) then
      begin
        GameSettings.Map := MAPS[i].Name;
        if (map_id in RealWorldMapSet) then
          GameSettings.FMapStyle := msRealWorld
        else if (map_id = miCustom) then
          GameSettings.FMapStyle := msCustom
        else
          GameSettings.FMapStyle := msStandard;
        GameSettings.MapId := map_id;
      end;
    end;

    GameSettings.FDifficultyLevel := TDifficultyLevel(difficulty);
    GameSettings.LockDiplomacy := lock_teams;

    { getting Player_info data }
    for i := 0 to 8 do
    begin
      ReadInt32(player_data_index);
      ReadInt32(human);
      Read(name_len, SizeOf(name_len));
      Read(buff256, name_len);
      buff256[name_len] := #0;

      { sometimes very rarely index is 1 }
      if (human = 0) or (human = 1) then
        Continue;
      if (i <> 0) then
      begin
        Player := TPlayer.Create;
        Player.Name := {$IFDEF FPC}PChar(buff256){$ELSE}buff256{$ENDIF};
        Player.Index := player_data_index;
        Player.Human := (human = $02);
        PlayerList.AddPlayer (Player);
      end;
    end;

    { getting game type for aok }
    if FIsMgl then
    begin
      Seek(trigger_info_pos - SizeOf(constant2), soFromBeginning);
      Seek(-6);
      { unknown25 }
      ReadInt32(unknown25);
      case unknown25 of
          1: GameSettings.FGameType := gtDeathMatch;
        256: GameSettings.FGameType := gtRegicide;
      end;
    end;

    { getting victory }
    Seek(trigger_info_pos - SizeOf (constant2), soFromBeginning);
    if FIsMgx then
      Seek(-7);
    Seek(-110);
    ReadInt32(victory_condition);
    Seek(8);
    ReadChar(is_timelimit);
    if (is_timelimit <> 0) then
      ReadFloat(time_limit);

    with GameSettings.Victory do
    begin
      VictoryCondition := TVictoryCondition(victory_condition);
      if (is_timelimit <> 0) then
        TimeLimit := Round(time_limit) div 10;
    end;

    { Trigger_info }
    Seek(trigger_info_pos + 1, soFromBeginning);

    { always zero in mgl? or not a really trigger_info here for aok }
    ReadInt32(num_trigger);

    if (num_trigger <> 0) then
    begin
      { skip Trigger_info data }
      for i := 0 to num_trigger - 1 do
      begin
        Seek(18);
        ReadInt32(desc_len);
        Seek(desc_len);
        ReadInt32(name_len);
        Seek(name_len);
        ReadInt32(num_effect);
        for j := 0 to num_effect - 1 do
        begin
          Seek(24);
          ReadInt32(num_selected_object);
          if num_selected_object = -1 then
            num_selected_object := 0;
          Seek(72);
          ReadInt32(text_len);
          Seek(text_len);
          ReadInt32(sound_len);
          Seek(sound_len);
          Seek(num_selected_object shl 2);
        end;
        Seek(num_effect shl 2);
        ReadInt32(num_condition);
        Seek(76 * num_condition);
      end;
      Seek(num_trigger shl 2);

      GameSettings.Map := '';
      GameSettings.FGameType := gtScenario;  { obsolete? }
    end;

    { Other_data }
    ReadBuffer(team_indexes, SizeOf(team_indexes));

    for i := 0 to PlayerList.Count - 1 do
      PlayerList[i].Team := team_indexes[i] - 1;

    Seek(1);  { always 1? }
    ReadInt32(reveal_map);
    Seek(4);  { always 1? }
    ReadInt32(map_size);
    ReadInt32(pop_limit);
    if FIsMgx then
    begin
      ReadChar(game_type);
      ReadChar(lock_diplomacy);
    end;

    with GameSettings do
    begin
      FRevealMap := TRevealMap(reveal_map);
      FMapSize := TMapSize(map_size);
      PopLimit := pop_limit;
      if FIsMgx then
      begin
        LockDiplomacy := (lock_diplomacy = $01);
        FGameType := TGameType(game_type);
      end;
    end;

    { here comes pre-game chat (mgl doesn't hold this information }
    if FIsMgx then
    begin
      ReadInt32(num_chat);
      for i := 0 to num_chat - 1 do
      begin
        throw_chat := 0;
        ReadInt32(chat_len);

        { zero-length chat exists }
        if (chat_len = 0) then
          Continue;

        // prevent overflow... it is possible to bypass message length checking in the game
        // we wont accept messages longer than 65535 its quite enough as valid maximum is about 247 chars (without prefixed string here)
        if (chat_len > High(buff65536)) then
        begin
          throw_chat := chat_len - High(buff65536);
          chat_len := High(buff65536);
        end;

        ReadBuffer(buff65536, chat_len);
        if (throw_chat > 0) then
          Seek(throw_chat);

        if (buff65536[0] = '@') and (buff65536[1] = '#') and (buff65536[2] >= '1') and (buff65536[2] <= '8') then
        begin
          buff65536[chat_len] := #0;
                                  
          ChatMessage := TChatMessage.Create;
          ChatMessage.Player := PlayerList.GetPlayerByIndex(StrToIntDef(buff65536[2], 0));
          ChatMessage.Msg := Copy(buff65536, 4, Length(buff65536));
          PreGameChatMessages.Add(ChatMessage);

          PreGameChat.Add(buff65536);
        end;
      end;
    end;

    { skip AI_info if exists }
    Seek($0C, soFromBeginning);
    ReadBool(include_ai);

    if (include_ai) then
    begin
      Seek(2);
      ReadWord(num_string);
      Seek(4);
      for i := 0 to num_string - 1 do
      begin
        ReadInt32(string_length);
        Seek(string_length);
      end;
      Seek(6);
      for i := 0 to 7 do
      begin
        Seek(10);
        ReadWord(num_rule);
        Seek(4 + 400 * num_rule);
      end;
      Seek(5544);
    end;

    { getting data }
    Seek(4);
    ReadInt32(game_speed);
    Seek(37);
    ReadWord(rec_player_ref);
    ReadChar(num_player);

    { 0 is GAIA, not appears in PlayerList }
    Dec(rec_player_ref);
    Dec(num_player);

    GameSettings.FGameSpeed := TGameSpeed(game_speed);

    Player := PlayerList[rec_player_ref];
    if Assigned (Player) then
    begin
      Player.Owner := True;
      with GameSettings do
      begin
        Owner := Player;
        POV := Player.Name;
        POVEx := Player.Name;
        bPOV := rec_player_ref;
        bPlayers := num_player;
      end;
    end;

    GameSettings.InGameCoop := (num_player < PlayerList.Count);

    Inc (num_player);

    { getting map }
    Seek(62);
    if FIsMgl then
      Seek(-2);
    ReadInt32(map_size_x);
    ReadInt32(map_size_y);
    FMapWidth := map_size_x;
    FMapHeight := map_size_y;

    ReadInt32(num_unknown_data);
    { unknown data }
    for i := 0 to num_unknown_data - 1 do
    begin
      Seek(1275 + map_size_x * map_size_y);
      ReadInt32(num_float);
      Seek((num_float shl 2) + 4);
    end;
    Seek(2);

    SetLength(FMapData, map_size_x, map_size_y);
    { map data }
    for y := 0 to map_size_y - 1 do
      for x := 0 to map_size_x - 1 do
      begin
        ReadChar(terrain_id);
        ReadChar(elevation);
        FMapData[x, y] := terrain_id + 1000 * (elevation + 1);
      end;

    ReadInt32(num_data);
    Seek(4 + (num_data shl 2));
    for i := 0 to num_data - 1 do
    begin
      ReadInt32(num_couples);
      Seek(num_couples shl 2);
    end;
    ReadInt32(map_size_x2);
    ReadInt32(map_size_y2);
    Seek((map_size_x2 * map_size_y2 shl 2) + 4);
    ReadInt32(num_unknown_data2);
    Seek(27 * num_unknown_data2 + 4);

    Queue.Push(Pointer(num_player));
    Queue.Push(Pointer(map_size_x));
    Queue.Push(Pointer(map_size_y));
    Queue.Push(Pointer(Position));
    { getting Player_info }
    if not ReadPlayerInfoBlockEx then
    begin
      { something gone wrong with extended analysis, use this older one }
      GaiaObjects.Clear;
      PlayerObjects.Clear;
      Seek(Longint(Queue.Pop), soFromBeginning);      

      { first is GAIA, skip some useless bytes }
      if (FGameVersion = gvAOKTrial) or (FGameVersion = gvAOCTrial) then
        Seek(4);
      Seek(num_player + 70);  // + 2 len of playerlen
      if FIsMgx then Seek(792) else Seek(756);

      if FIsMgx then Seek(41249) else Seek(34277);
      Seek(map_size_x * map_size_y);
      // Explored GAIA Objects
      // Units Data II

      for i := 0 to (PlayerList.Count - 1) do
      begin
        Player := PlayerList[i];
        { skip cooping player, she/he has no data in Player_info }
        Player_ := PlayerList.GetPlayerByIndex(Player.Index);

        if (Assigned (Player_)) and (Player_ <> Player) and (Player_.CivId <> cNone) then
        begin
          Player.CivId := Player_.CivId;
          Player.Civ := Player_.Civ;
          Player.ColorId := Player_.ColorId;
          {$IFDEF EXTENDED}Player.Color := Player_.Color;{$ENDIF}
          Player.Team := Player_.Team; { required }
          Player.IsCooping := True;
          Continue;
        end;

        if Queue.AtLeast(1) then
          Seek(Longint(Queue.Pop), soFromBeginning)
        else
        begin
          repeat
            ReadBuffer(buff256, SizeOf(player_info_end_separator));
            if CompareMem(@buff256, @player_info_end_separator, SizeOf(player_info_end_separator)) then
              Break;
            Seek(-SizeOf(player_info_end_separator) + 1);
          until (Position >= Size);

          if (FGameVersion = gvAOKTrial) or (FGameVersion = gvAOCTrial) then
            Seek(4);
          Seek(num_player + 52 + Length(Player.Name)); // + null-terminator
        end;

        { Civ_header }
        ReadFloat(food);
        ReadFloat(wood);
        ReadFloat(stone);
        ReadFloat(gold);
        { headroom = (house capacity - population) }
        ReadFloat(headroom);
        Seek(4);
        { Starting Age, note: PostImperial Age = Imperial Age here }
        ReadFloat(data6);
        Seek(16);
        ReadFloat(population);
        Seek(100);
        ReadFloat(civilian_pop);
        Seek(8);
        ReadFloat(military_pop);
        if FIsMgx then Seek(629) else Seek(593);
        ReadFloat(init_camera_pos_x);
        ReadFloat(init_camera_pos_y);
        if FIsMgx then Seek(9) else Seek(5);
        ReadChar(civilization);
        { sometimes(?) civilization is zero in scenarios when the first player is briton (only? always? rule?) }
        if (civilization = 0) then
          Inc(civilization);
        { skip unknown9[3] }
        Seek(3);
        ReadChar(player_color);

        with Player do
        begin
          CivId := TCivilization(civilization);
          SetCiv(TCivilization(civilization));
          ColorId := player_color;
          {$IFDEF EXTENDED}SetColor(player_color);{$ENDIF}
          InitialState.Position.X := Round(init_camera_pos_x);
          InitialState.Position.Y := Round(init_camera_pos_y);
          InitialState.Food := Round(food);
          InitialState.Wood := Round(wood);
          InitialState.Stone := Round(stone);
          InitialState.Gold := Round(gold);
          InitialState.StartingAge := TStartingAge(Round(data6));
          // TODO: Huns, Goths, Nomad etc. var...
          InitialState.HouseCapacity := Round(headroom) + Round(population);
          InitialState.Population := Round(population);
          InitialState.CivilianPop := Round(civilian_pop);
          InitialState.MilitaryPop := Round(military_pop);
          InitialState.ExtraPop := InitialState.Population - (InitialState.CivilianPop + InitialState.MilitaryPop);
        end;

        if FIsMgx then Seek(41249) else Seek(34277);
        Seek(map_size_x * map_size_y);
        // Explored GAIA Objects
        // Units Data II
      end;
    end;

    { getting objectives or instructions }
    if (scenario_header_pos > 0) then
    begin
      Seek(scenario_header_pos + 4433, soFromBeginning);
      { original scenario file name }
      ReadWord(original_sc_filename_len);
      if (original_sc_filename_len > 0) then
      begin
        FillChar(buff65536, SizeOf(buff65536), #0);
        ReadBuffer(buff65536, original_sc_filename_len);
        GameSettings.ScFileName := {$IFDEF FPC}PChar(buff65536){$ELSE}buff65536{$ENDIF};
        if FIsMgl then
          GameSettings.FGameType := gtScenario; { this way we detect scenarios in mgl, is there any other way? }
      end;
      if FIsMgx then Seek(24) else Seek(20);

      { scenario instruction or Objectives string, depends on game type }
      objectives_pos := Position;
      ReadWord(instruction_len);
      if (instruction_len > 0) then
      begin
        FillChar(buff65536, SizeOf(buff65536), #0);
        ReadBuffer(buff65536, instruction_len);
        if GameSettings.IsScenario then
        begin
//          { we assume the first line usually indicates the scenario name }
//          instruction_len := FindChar (#13, buff65536);
//          if (instruction_len = 0) then
//            instruction_len := FindChar (#10, buff65536);
//          if (instruction_len > 0) then
//            buff65536[instruction_len - 1] := #0;
//          GameSettings.Map := {$IFDEF FPC}PChar (buff65536){$ELSE}buff65536{$ENDIF};
        end else
          GameSettings.ObjectivesString := {$IFDEF FPC}PChar(buff65536){$ELSE}buff65536{$ENDIF};
      end;
    end;

    Result := True;
    Exit;
    { code for getting map below is obsolete, see PostAnalyze, keeping here in case something goes wrong }

    { getting map name (only if map is custom (44) and game type is not scenario
      if map_id = 32 (Random Land Map), it is possible to obtain map if it's written in english }
    if not (map_id in [9..33, 48]) and not GameSettings.IsScenario then
    begin
      Seek (game_settings_pos - 11520, soFromBeginning);

      map_found := False;

      Read (buff[0], 2);
      { searching up to -100000 bytes, than stop }
      while ((Position > game_settings_pos - 11520 - 100000) and (not map_found)) do
      begin
        if CompareMem (@buff, @con1, SizeOf (con1)) or
           CompareMem (@buff, @con2, SizeOf (con2)) then
        begin
          Seek (-2, soFromCurrent);
          for i := Low (LANGUAGES) to High (LANGUAGES) do
          begin
            Seek (-LANGUAGES[i].len, soFromCurrent);
            Read (buff256[0], LANGUAGES[i].len);

            if CompareMem (@buff256, @LANGUAGES[i].data, LANGUAGES[i].len) then
            begin
              { skip ': ' }
              Seek (2, soFromCurrent);

              for j := 0 to 99 do
              begin
                Read (buff[0], 1);
                if (buff[0] <> $0A) then
                  buff256[j] := Chr (buff[0])
                else
                begin
                  buff256[j] := #0;
                  map_found := True;
                  Break;
                end;
              end;
              buff256[j + 1] := #0;
              Break;
            end; { end if CompareMem }
          end; { end for loop }
        end; { endif }
        Seek (-3, soFromCurrent);
        Read (buff[0], 2);
      end; { endwhile }

      if map_found then
        GameSettings.Map := Trim (buff256)
      else
        GameSettings.Map := MAP_STYLES[msCustom];
      GameSettings.FMapStyle := msCustom;
    end;

    Result := True;
  end;
end;

function TRecAnalyst.AnalyzeBody: Boolean;
var
  time_cnt: Longint;
  age_flag: array[0..7] of Byte;
  m_body_len, i, idx: Integer;
  od_type, command, chat_len, throw_chat, time: Longint;
  unknown, length: Longint;
  cmd, player_number, player_index, ver: Byte;
  buff256: array[0..255] of Char;
  Player, PlayerFrom, PlayerTo: TPlayer;
  player_id_from, player_id_to, resource_id: Byte;
  amount_tributed, market_fee: Single;
  player_id, research_id: Word;
  object_id: LongInt;
  unit_type_id, unit_num, building_type_id: Word;
  Res: TResearch;
  Tribute: TTribute;
  TrainedUnit: TTrainedUnit;
  Building: TBuilding;
  ChatMessage: TChatMessage;
begin
  time_cnt := Ord(GameSettings.GameSpeed);

  m_body_len := FBodyStream.Size;
  FillChar(age_flag, SizeOf(age_flag), 0);
  FillChar(buff256, SizeOf(buff256), 0);

  with FBodyStream do
  begin
    Seek(0, soFromBeginning);
    while (Position < m_body_len - 3) do
    begin
      if (Position = 0) and FIsMgl then
        od_type := $04
      else
        ReadInt32(od_type);

      { ope_data types: 4(Game_start or Chat), 2(Sync), or 1(Command) }
      case od_type of
        $04, $03:
          begin
            ReadInt32(command);
            if (command = $01F4) then
            begin
              { Game_start }
              if FIsMgl then
              begin
                Seek(28);
                ReadChar(ver);
                case ver of
                  0: if (FGameVersion <> gvAOKTrial) then
                        FGameVersion := gvAOK20;
                  1: FGameVersion := gvAOK20a;
                end;
                Seek(3);
              end else
              begin
                case od_type of
                  $03: if (FGameVersion <> gvAOCTrial) then
                         FGameVersion := gvAOC10;
                  $04: FGameVersion := gvAOC10c;
                end;
                Seek(20);
              end;
            end
            else if (command = -1) then
            begin
              { Chat }
              throw_chat := 0;
              ReadInt32(chat_len);
              for i := 0 to PlayerList.Count - 1 do
              begin
                Player := PlayerList[i];
                if not Assigned(Player) then
                  Continue;

                if (Player.FeudalTime <> 0) and (Player.FeudalTime < time_cnt) and (age_flag[i] < 1) then
                begin
                  ChatMessage := TChatMessage.Create;
                  ChatMessage.Time := Player.FeudalTime;
                  ChatMessage.Msg := Format(c_feudal_age_advance, [Player.Name]);
                  InGameChatMessages.Add(ChatMessage);

                  InGameChat.Add(Format('%d@#0' + c_feudal_age_advance, [Player.FeudalTime, Player.Name]));
                  age_flag[i] := 1;
                end;
                if (Player.CastleTime <> 0) and (Player.CastleTime < time_cnt) and (age_flag[i] < 2) then
                begin
                  ChatMessage := TChatMessage.Create;
                  ChatMessage.Time := Player.CastleTime;
                  ChatMessage.Msg := Format(c_castle_age_advance, [Player.Name]);
                  InGameChatMessages.Add(ChatMessage);

                  InGameChat.Add(Format('%d@#0' + c_castle_age_advance, [Player.CastleTime, Player.Name]));
                  age_flag[i] := 2;
                end;
                if (Player.ImperialTime <> 0) and (Player.ImperialTime < time_cnt) and (age_flag[i] < 3) then
                begin
                  ChatMessage := TChatMessage.Create;
                  ChatMessage.Time := Player.ImperialTime;
                  ChatMessage.Msg := Format(c_imperial_age_advance, [Player.Name]);
                  InGameChatMessages.Add(ChatMessage);

                  InGameChat.Add(Format('%d@#0' + c_imperial_age_advance, [Player.ImperialTime, Player.Name]));
                  age_flag[i] := 3;
                end;
              end;

              // see reading chat in header section
              if (chat_len > High(buff256)) then
              begin
                throw_chat := chat_len - High(buff256);
                chat_len := High(buff256);
              end;

              ReadBuffer(buff256, chat_len);
              if (throw_chat > 0) then
                Seek(throw_chat);

              if (buff256[0] = '@') and (buff256[1] = '#') and (buff256[2] >= '1') and (buff256[2] <= '8') then
              begin
                buff256[chat_len] := #0;
                if (buff256[3] = '-') and (buff256[4] = '-') and
                   (buff256[chat_len - 3] = '-') and (buff256[chat_len - 2] = '-') then
                begin
                end else
                begin
                  ChatMessage := TChatMessage.Create;
                  ChatMessage.Time := time_cnt;
                  ChatMessage.Player := PlayerList.GetPlayerByIndex(StrToIntDef (buff256[2], 0));
                  ChatMessage.Msg := Copy (buff256, 4, System.Length(buff256));
                  InGameChatMessages.Add(ChatMessage);

                  InGameChat.Add(Format('%d%s', [time_cnt, buff256]));
                end;
              end;
            end;
          end;
        $02:
          begin
            { Sync }
            ReadInt32(time);
            Inc(time_cnt, time); { time_cnt is in miliseconds }
            ReadInt32(unknown);
            if (unknown = 0) then
              Seek(28);
            Seek(12);
          end;
        $01:
          begin
            { Command }
            ReadInt32(length);
            ReadChar(cmd);
            Seek(-1);
            case cmd of
              $0B:
                begin
                  { player resign }
                  Seek(1);
                  ReadChar(player_index);
                  ReadChar(player_number);
                  // if dropped len = 4, 16 otherwise (not a rule)
                  Player := PlayerList.GetPlayerByIndex(player_index);
                  if Assigned(Player) and (Player.ResignTime = 0) then
                  begin
                    Player.ResignTime := time_cnt;
                    ChatMessage := TChatMessage.Create;
                    ChatMessage.Time := Player.ResignTime;
                    ChatMessage.Msg := Format(c_resigned, [Player.Name]);
                    InGameChatMessages.Add(ChatMessage);

                    InGameChat.Add(Format('%d@#0' + c_resigned, [Player.ResignTime, Player.Name]));
                  end;
                  Seek(length - 3);
                end;
              $65:
                begin
                  { researches }
                  Seek(8);
                  ReadWord(player_id);
                  ReadWord(research_id);
                  Player := PlayerList.GetPlayerByIndex(player_id);

                  case research_id of
                    101:
                      begin
                        { feudal time }
                        if Assigned(Player) then
                          Player.FeudalTime := time_cnt + 130000; { + research time (2:10) }
                      end;
                    102:
                      begin
                        { castle time }
                        if Assigned(Player) then
                        begin
                          if (Player.CivId = cPersians) then
                            { about 10% less, but calculated as 160s / 1.10 despite of -10% = 144s }
                            Player.CastleTime := time_cnt + Round(160000 / 1.10)
                          else
                            Player.CastleTime := time_cnt + 160000;
                        end;
                      end;
                    103:
                      begin
                        { imperial time }
                        if Assigned(Player) then
                        begin
                          if (Player.CivId = cPersians) then
                            { about 15% less, but calculated as 190s / 1.15 despite of -15% = 161,5s }
                            Player.ImperialTime := time_cnt + Round(190000 / 1.15)
                          else
                            Player.ImperialTime := time_cnt + 190000;
                        end;
                      end;
                  end;

                  if Assigned(Player) then
                  begin
                    { remember this is the time player has just started to research the particular technology,
                      repetitious researching may occure,
                      here we are asking about it }
                    idx := -1;
                    for i := Researches.Count - 1 downto 0 do
                    begin
                      Res := Researches[i] as TResearch;
                      if (Res.Id = research_id) and (Res.PlayerId = player_id) then
                      begin
                        idx := i;
                        Break;
                      end;
                    end;

                    if (idx = -1) then
                    begin
                      { just add this research }
                      Res := TResearch.Create;
                      Res.Id := research_id;
                      Res.Time := time_cnt;
                      Res.PlayerId := player_id;
                      Researches.Add(Res);
                    end else
                    begin
                      Res := Researches[idx] as TResearch;
                      { remember data in body section are not necessarily time-sorted
                        (rarely, but may occure) that's why we are comparing times }
                      if (Res.Time < time_cnt) then
                        Res.Time := time_cnt;
                    end;
                  end;
                  { we do sorting in post analysis, see PostAnalyze }
{
                    if not Found and (Researches.Count > 0) then
                    begin
                      i := Researches.Count - 1;
                      repeat
                        Res := Researches[i] as TResearch;
                        if (Res.Time < time_cnt) then
                          Break;
                        Dec (i);
                      until (i < 0);
                    end else i := -1;
                    if not Found then
                      Res := TResearch.Create
                    else
                      Res := Researches[idx] as TResearch;

                    Res.Id := research_id;
                    Res.Time := time_cnt;
                    Res.PlayerId := player_id;

                    if not Found then
                      Researches.Insert (i + 1, Res);
                  end;
}
                  Seek(length - 12);
                end;
              $77:
                begin
                  { training unit }
                  Seek(4);
                  ReadInt32(object_id);
                  ReadWord(unit_type_id);
                  ReadWord(unit_num);

                  TrainedUnit := Units.GetUnit(unit_type_id);
                  if Assigned(TrainedUnit) then
                    Inc(TrainedUnit.Count, unit_num)
                  else
                  begin
                    TrainedUnit := TTrainedUnit.Create;
                    TrainedUnit.Id := unit_type_id;
                    TrainedUnit.Count := unit_num;
                    Units.Add(TrainedUnit);
                  end;
                  Seek(length - 12);
                end;
              $64:
                begin
                  { pc trains unit }
                  Seek(10);
                  ReadWord(unit_type_id);

                  TrainedUnit := Units.GetUnit(unit_type_id);
                  if Assigned(TrainedUnit) then
                    Inc(TrainedUnit.Count)
                  else
                  begin
                    TrainedUnit := TTrainedUnit.Create;
                    TrainedUnit.Id := unit_type_id;
                    TrainedUnit.Count := 1;
                    Units.Add(TrainedUnit);
                  end;
                  Seek(length - 12);
                end;
              $66:
                begin
                  Seek(2);
                  { player_id }
                  ReadWord(player_id);
                  Seek(8);
                  { building_type_id unit_type_id }
                  ReadWord(building_type_id);
                  
                  Player := PlayerList.GetPlayerByIndex(player_id);
                  if Assigned(Player) then
                  begin
                    Building := Player.Buildings.GetBuilding(building_type_id);
                    if Assigned(Building) then
                      Inc(Building.Count)
                    else
                    begin
                      Building := TBuilding.Create;
                      if (InArray(bGates, building_type_id) = -1) then
                        Building.Id := building_type_id
                      else
                        Building.Id := biGate;
                      Building.Count := 1;
                      Player.Buildings.Add(Building);
                    end;
                  end;
                  Seek(length - 14);
                end;
              $6C:
                begin
                  { tributing }
                  Seek(1);
                  ReadChar(player_id_from);
                  ReadChar(player_id_to);
                  ReadChar(resource_id);
                  ReadFloat(amount_tributed);
                  ReadFloat(market_fee);
                  PlayerFrom := PlayerList.GetPlayerByIndex(player_id_from);
                  PlayerTo   := PlayerList.GetPlayerByIndex(player_id_to);
                  if Assigned(PlayerFrom) and Assigned(PlayerTo) then
                  begin
                    Tribute := TTribute.Create;
                    Tribute.Time       := time_cnt;
                    Tribute.PlayerFrom := PlayerFrom;
                    Tribute.PlayerTo   := PlayerTo;
                    Tribute.ResourceId := TResourceId(resource_id);
                    Tribute.Amount     := Floor(amount_tributed);
                    Tribute.Fee        := market_fee;
                    Tributes.Add(Tribute);
                  end;
                  Seek(length - 12);
                end;
              $03, $78, $00:
                begin
                  Seek(length);
                end;
              else
                begin
                  Seek(length);
                end;
            end;
            Seek(4);
          end;
        else
          begin
            { shouldn't occure, just to prevent unexpected endless cycling }
            Seek(1);
          end;
      end;
    end;  { endwhile }

    GameSettings.PlayTime := time_cnt;
  end;
  Result := True;
end;

function TRecAnalyst.Analyze: Boolean;
var
  StartTime: DWORD;
begin
  { TODO: aby som nevolal dvakrat analyze? }
  Result := False;
  StartTime := GetTickCount;
  try
    try
      if not ExtractStreams then
        Exit;
      if not AnalyzeHeader then
        Exit;
      if not AnalyzeBody then
        Exit;
      PostAnalyze;
      FAnalyzed := True;
    except
      FHeaderStream.Clear;
      FBodyStream.Clear;
      {$IFNDEF LIB}raise;{$ENDIF}
    end;
  finally
    if not FKeepStreams then
    begin
      FHeaderStream.Clear;
      FBodyStream.Clear;
    end;
  end;

  FAnalyzeTime := GetTickCount - StartTime;
  Result := True;
end;

procedure TRecAnalyst.BuildTeams;
var
  Player, Player_: TPlayer;
  Team: TTeam;
  i, j, k: Integer;
  found: Boolean;
begin
  if (Teams.Count > 0) then
    Exit;

  for i := 0 to PlayerList.Count - 1 do
  begin
    Player := PlayerList[i];
    if not Assigned (Player) then
      Continue;
    if (Player.Team = 0) then
    begin
      found := False;
      for j := 0 to Teams.Count - 1 do
      begin
        Team := Teams[j];
        if not Assigned (Team) then
          Continue;
        if (Team.Index <> Player.Team) then
          Continue;
        for k := 0 to Team.Count - 1 do
        begin
          Player_ := Team[k];
          if (Player_.Index = Player.Index) then
          begin
            Team.AddPlayer (Player);
            found := True;
            Break;
          end;
        end;
        if found then
          Break;
      end;
      if not found then
      begin
        Team := TTeam.Create;
        Team.AddPlayer (Player);
        Teams.AddTeam (Team);
      end;
    end else
    begin
      Team := Teams.GetTeamByIndex (Player.Team);
      if Assigned (Team) then
        Team.AddPlayer (Player)
      else
      begin
        Team := TTeam.Create;
        Team.AddPlayer (Player);
        Teams.AddTeam (Team);
      end;
    end;
  end;
end;
{$IFDEF EXTENDED}
function TRecAnalyst.GenerateMap(var Bitmap: TBitmap; const Width: Integer; const Height: Integer; bgColor: TColor): Boolean;
var
  x, y, i, terrain_id, elevation: Integer;
  Bmp: TBitmapEx;
  Player: TPlayer;
  UO: TUnitObject;
begin
  Result := False;
  if not FAnalyzed or not Assigned (FMapData) or not Assigned (Bitmap) then
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
        begin
          {$IFNDEF LIB}
          Logger.SendWarning ('Color for terrain id %d has not been specified.', [terrain_id], True);
          {$ENDIF}
          Windows.SetPixel (Bmp.Canvas.Handle, x, y, UNKNOWN_TERRAIN_COLOR);
        end;
      end;
    end;

    { draw gaia objects }
    for i := 0 to GaiaObjects.Count - 1 do
    begin
      UO := GaiaObjects[i] as TGaiaObject;
      case UO.Id of
        uiGoldMine:
          begin
            Bmp.Canvas.Brush.Color := GOLD_COLOR;
            Bmp.Canvas.Pen.Color := GOLD_COLOR;
            Bmp.Canvas.FillRect (Rect (UO.Position.X - 1, UO.Position.Y - 1,
              UO.Position.X + 2, UO.Position.Y + 2));
          end;
        uiStoneMine:
          begin
            Bmp.Canvas.Brush.Color := STONE_COLOR;
            Bmp.Canvas.Pen.Color := STONE_COLOR;
            Bmp.Canvas.FillRect (Rect (UO.Position.X - 1, UO.Position.Y - 1,
              UO.Position.X + 2, UO.Position.Y + 2));
          end;
        uiCliff1..uiCliff10:
          begin
            Bmp.Canvas.Brush.Color := CLIFF_COLOR;
            Bmp.Canvas.Pen.Color := CLIFF_COLOR;
            Bmp.Canvas.FillRect (Rect (UO.Position.X - 2, UO.Position.Y - 1,
              UO.Position.X + 2, UO.Position.Y + 2));
          end;
        uiRelic:
          begin
            Bmp.Canvas.Brush.Color := RELIC_COLOR;
            Bmp.Canvas.Pen.Color := RELIC_COLOR;
            Bmp.Canvas.FillRect (Rect (UO.Position.X - 1, UO.Position.Y - 1,
              UO.Position.X + 2, UO.Position.Y + 2));
          end;
        uiForageBush, uiDeer, uiBoar, uiJavelina, uiTurkey, uiSheep:
          begin
            Bmp.Canvas.Brush.Color := FOOD_COLOR;
            Bmp.Canvas.Pen.Color := FOOD_COLOR;
            Bmp.Canvas.FillRect (Rect (UO.Position.X - 1, UO.Position.Y - 1,
              UO.Position.X + 2, UO.Position.Y + 2));
          end;
      end;
    end;

    { draw positions }
    if FShowPositions and not GameSettings.IsScenario and (GameSettings.MapId <> miNomad) then
    begin
      { we do not draw positions in scenarios as they may be set anywhere }
      for i := 0 to PlayerList.Count - 1 do
      begin
        Player := PlayerList[i];
        if Player.IsCooping then
          Continue;
        with Bmp.Canvas, Player.InitialState do
        begin
          { do not use darken color in minimap }
          if (Player.ColorId = 6) then
            Pen.Color := ORIG_COLOR6
          else
            Pen.Color := Player.Color;
          Brush.Style := bsClear;
          Ellipse (Position.X - 9, Position.Y - 9, Position.X + 9, Position.Y + 9);

          if (Player.ColorId = 6) then
            Brush.Color := ORIG_COLOR6
          else
            Brush.Color := Player.Color;
          Brush.Style := bsSolid;
          Ellipse (Position.X - 4, Position.Y - 4, Position.X + 4, Position.Y + 4);
        end;
      end;
    end;

    { draw player objects }
    if FShowPositions then
    begin
      for i := 0 to PlayerObjects.Count - 1 do
      begin
        UO := PlayerObjects[i] as TUnitObject;
        Player := PlayerList.GetPlayerByIndex (UO.Owner);
        if not Assigned (Player) then
          Continue;
        if (Player.ColorId = 6) then
          Bmp.Canvas.Pen.Color := ORIG_COLOR6
        else
          Bmp.Canvas.Pen.Color := Player.Color;

        Bmp.Canvas.Brush.Color := Bmp.Canvas.Pen.Color;
        Bmp.Canvas.FillRect (Rect (UO.Position.X - 1, UO.Position.Y - 1,
          UO.Position.X + 1, UO.Position.Y + 1));
      end;
    end;

    Bmp.Rotate (45, bgColor);
//    Bmp.ResizeEx (Width, Height);
    Bmp.Resize2 (Width, Height);
    Bitmap.Assign (Bmp);

    Result := True;
  finally
    Bmp.Free;
  end;
end;
{$IFNDEF LIB}
function TRecAnalyst.GenerateResearches(var Bitmap: TBitmap; var ImageMap: TObjectList): Boolean;
var
  Player: TPlayer;
  Res: TResearch;
  prev_minute, minute, max: Integer;
  max_username_width: Integer;
  res_cnt: array[1..8] of Integer; // player_id in <1, 8>
  dst_x, dst_y, delta, i: Integer;
  bmp: TBitmapEx;
  padding_left: Integer;
const
  RTW = 19; { resource tile width }
  RTH = 19; { resource tile height }
  HSPACING = 2; { horizontal spacing }
  VSPACING = 2; { vertical spacing }
var
  lf: TLogFont;
  OldFont, NewFont: HFont;
  Item: TImageMapItem;
begin
  Result := False;

  if not FAnalyzed then
    Exit;

  FillChar (res_cnt, SizeOf (res_cnt), 0);

  Bitmap.Height := 1000;
  Bitmap.Canvas.Font.Name := 'Georgia';
  Bitmap.Canvas.Font.Color := clBlack;

  { calculate maximum width of the player names (before font size has been changed) }
  max_username_width := 0;
  for i := 0 to PlayerList.Count - 1 do
  begin
    Player := PlayerList[i];
    if Player.IsCooping then
      Continue;
    if (max_username_width < Bitmap.Canvas.TextWidth (Player.Name)) then
      max_username_width := Bitmap.Canvas.TextWidth (Player.Name);
  end;

  Bitmap.Canvas.Font.Size := 7;
  padding_left := Bitmap.Canvas.TextWidth ('999. min');
  { we are not skipping cooping players, nvm }
  { 100 preto, aby sa hint window nezasekaval }
  Bitmap.Width := padding_left + PlayerList.Count * (RTW + HSPACING) + 100;

  prev_minute := -1; delta := 0; max := 0;  { max = res_cnt[i]; forall j<>i: res_cnt[j] <= res_cnt[i] }
  dst_y := max_username_width + VSPACING;// + max_username_width;
  FillChar (res_cnt, SizeOf (res_cnt), 0);  { holds # of resources for each player in current minute }

  for i := 0 to Researches.Count - 1 do
  begin
    Res := Researches[i] as TResearch;
    { minute = current minute in timeline (minute-based sampling) }
    minute := Floor (Res.Time / 1000 / 60);
    { new minute has just started, rember researches are time-sorted }
    if (prev_minute <> minute) then
    begin
      FillChar (res_cnt, SizeOf (res_cnt), 0); { zero }
      Inc (dst_y, max * (RTH + VSPACING));  { shift position on maximum }
      max := 0;  { zero }
    end;
    prev_minute := minute;

    // TODO
    if Bitmap.Height < dst_y + 200 then
      Bitmap.Height := Bitmap.Height + 200;

    delta := res_cnt[Res.PlayerId] * (RTH + VSPACING);
    dst_x := padding_left + (Res.PlayerId - 1) * (RTW + HSPACING);

    { increase # of resources for its 'owner' }
    Inc (res_cnt[Res.PlayerId]);
    { set maximum if we have new one }
    if (max < res_cnt[Res.PlayerId]) then
      max := res_cnt[Res.PlayerId];

    bmp := TBitmapEx.Create;
    try
      try
        TResourceDll.LoadBitmap (Res.ResName, TBitmap (bmp));
        bmp.Resize2 (RTW, RTH);
      except
        on E: Exception do
          Logger.SendException ('Data: R.Id: %d, R.ResName: %s', [Res.Id, Res.ResName], E, True);
      end;

      if (delta = 0) then
        Bitmap.Canvas.TextOut (0, dst_y, Format (c_min, [minute + 1]));
      if Assigned (bmp) and bmp.HandleAllocated then
      begin
        Bitmap.Canvas.Draw (dst_x, dst_y + delta, bmp);
        Item := TImageMapItem.Create;
        Item.Coordinates.Left := dst_x;
        Item.Coordinates.Top := dst_y + delta;
        Item.Coordinates.Right := dst_x + RTW;
        Item.Coordinates.Bottom := dst_y + delta + RTH;
        Item.Hint := Format ('%s (%s)', [Res.Name, TRecAnalyst.GameTimeToString (Res.Time)]);
        ImageMap.Add (Item);
      end;
    finally
      bmp.Free;
    end;
  end;

  Inc (dst_y, max * (RTH + VSPACING));
  Bitmap.Height := dst_y + delta + 10;

  { to be able to draw usernames vertically }

  Bitmap.Canvas.Font.Size := 8;

  { draw player names first }
  delta := 0;
  for i := 0 to PlayerList.Count - 1 do
  begin
    Player := PlayerList[i];
    if Player.IsCooping then
      Continue;

    Bitmap.Canvas.Font.Color := Player.Color;

    OldFont := Bitmap.Canvas.Font.Handle;
    GetObject (OldFont, SizeOf (lf), @lf);
    lf.lfEscapement  := 900;
    lf.lfOrientation := 0;
    NewFont := CreateFontIndirect (lf);
    try
      SelectObject (Bitmap.Canvas.Handle, NewFont);
      Bitmap.Canvas.TextOut (padding_left + (RTW + HSPACING) * delta, max_username_width, Player.Name);
    finally
      DeleteObject (SelectObject (Bitmap.Canvas.Handle, OldFont));
    end;
    Inc (delta);
  end;
  Result := True;
end;
{$ENDIF}
{$ENDIF}
procedure TRecAnalyst.Reset;
begin
  with Self do
  begin
    FHeaderStream.Clear;
    FBodyStream.Clear;
    FMapWidth := 0;
    FMapHeight := 0;
    FileName := '';
    SetLength (FMapData, 0);
    FMapData := nil;
    FGameVersion := gvUnknown;
    FIsMgl := False;
    FIsMgx := False;
    GameSettings.Clear;
    Teams.Clear;
    PlayerList.Clear;

    PreGameChat.Clear;
    InGameChat.Clear;
    Tributes.Clear;
    FAnalyzeTime := 0;
    Units.Clear;
    Researches.Clear;
    PreGameChatMessages.Clear;
    InGameChatMessages.Clear;
    GaiaObjects.Clear;
    PlayerObjects.Clear;

    {$IFNDEF LIB}FZeroHeaderLen := False;{$ENDIF}
    FAnalyzed := False;
    next_pos := 0;
    objectives_pos := 0;
    CommentString := '';
    {$IFDEF LIB}FLastError := RECANALYST_OK;{$ENDIF}
  end;
end;

procedure TRecAnalyst.PostAnalyze;
var
  Player: TPlayer;
  Team: TTeam;
  i, j, idx: Integer;
  team_ary: array[0..7] of Integer;
  Lines, CoopList: TStringList;
  MapFound: Boolean;
  CP: String;
begin
  with GameSettings do
    if not IsScenario then
    begin
      Lines := TStringList.Create;
      try
        Lines.Text := ObjectivesString;
        { get map }
        if FIsMgl or (MapId = miCustom) then
        begin
          if (Lines.Count > 2) then
          begin
            idx := Pos (': ', Lines[2]);
            if (idx <> 0) then
              Map := Copy (Lines[2], idx + Length (': '), Length (Lines[2]));

            if FIsMgl then
            begin
              MapFound := False;
              for i := 0 to LANGUAGES_NUM - 1 do
              begin
                for j := 0 to MAPS_NUM - 1 do
                  if (LOC_MAP_NAMES[i][j] = Map) then
                  begin
                    Map := MAPS[j].Name;
                    MapFound := True;
                    Break;
                  end;
                if MapFound then
                  Break;
              end;
            end;
          end;
        end;
        { extract comment }
        idx := Lines.IndexOf ('');  { #$0A#$0A separator }
        if (idx <> -1) then
        begin
          { comment exists }
          if (idx < Lines.Count) then
          begin
            ObjectivesString := '';
            for i := 0 to idx - 1 do
              ObjectivesString := ObjectivesString + Lines[i] + #$0A;
            ObjectivesString := Copy (ObjectivesString, 1, Length (ObjectivesString) - 1);

            CommentString := '';
            for i := idx + 1 to Lines.Count - 1 do
              CommentString := CommentString + Lines[i] + #$0A;
            CommentString := Copy (CommentString, 1, Length (CommentString) - 1);
          end;
        end;
      finally
        Lines.Free;
      end;
    end;

  BuildTeams;

  { Players }
  idx := 0;
  FillChar (team_ary, SizeOf (team_ary), 0);
  for i := 0 to Teams.Count - 1 do
  begin
    Team := Teams[i];
    { tmp_ary[idx] = Team.Count - Cooping Players }
    for j := 0 to Team.Count - 1 do
    begin
      Player := Team[j];
      if not Player.IsCooping then
        Inc (team_ary[idx]);
    end;
    Inc (idx);
  end;
  idx := 0; { ArraySum }
  for i := Low (team_ary) to High (team_ary) do
    Inc (idx, team_ary[i]);
  if (idx = Teams.Count) and (Teams.Count > 2) then
    GameSettings.Players := 'FFA'
  else
  begin
    GameSettings.Players := '';
    for i := Low (team_ary) to High (team_ary) do
    begin
      if (team_ary[i] = 0) then
        Continue;
      GameSettings.Players := GameSettings.Players + 'v' + IntToStr (team_ary[i]);
    end;
    Delete (GameSettings.Players, 1, Length ('v'));
  end;

  { PovEx }
  Player := GameSettings.Owner;
  if not Assigned (Player) then
    Exit;

  CoopList := TStringList.Create;
  try
    for i := 0 to PlayerList.Count - 1 do
    begin
      Player := PlayerList[i];
      if Player = GameSettings.Owner then
        Continue;
      if (Player.Index <> GameSettings.Owner.Index) then
        Continue;
      CoopList.Add (Player.Name);
    end;
    if (CoopList.Count > 0) then
    begin
      // GameSettings.POVEx := GameSettings.POVEx + Format (' (%s)', [CoopList.CommaText]);
      CP := StringReplace (CoopList.Text, #13#10, ', ', [rfReplaceAll]);
      CP := Copy (CP, 1, Length (CP) - Length (', '));
      GameSettings.POVEx := GameSettings.POVEx + Format (' (%s)', [CP]);
    end;
  finally
    CoopList.Free;
  end;

  { fix: player could click age advance, but game finished before reaching specific age }
  for i := 0 to PlayerList.Count - 1 do
  begin
    Player := PlayerList[i];
    if not Assigned (Player) then
      Continue;
    if (Player.FeudalTime > GameSettings.PlayTime) then
      Player.FeudalTime := 0;
    if (Player.CastleTime > GameSettings.PlayTime) then
      Player.CastleTime := 0;
    if (Player.ImperialTime > GameSettings.PlayTime) then
      Player.ImperialTime := 0;
  end;

  if (Units.Count > 0) then
    Units.Sort (@UnitsCompare);

  if (Researches.Count > 0) then
    Researches.Sort (@ResearchesCompare);

  { sort in-game chat }
  if (InGameChatMessages.Count > 0) then
    InGameChatMessages.Sort (@ChatCompare);

  if (GaiaObjects.Count > 0) then
    GaiaObjects.Sort (@GaiaObjectsCompare);

//  if InGameChat.Count > 0 then
//    InGameChat.CustomSort (@ChatSortCompare);
end;

function TRecAnalyst.GetGameVersionStr: PChar;
begin
  Result := '';
  if not (FGameVersion in [Low (GAME_VERSIONS)..High (GAME_VERSIONS)]) then
    Exit;
  Result := GAME_VERSIONS[FGameVersion];
end;

procedure TRecAnalyst.Build(const AFileName: String);
var
  outStream: TMemoryStream;
  header_len: longint;
  hs: TMemoryStream;
begin
  if not FKeepStreams or not FAnalyzed then
    Exit;

  hs := TMemoryStream.Create;
  outStream := TMemoryStream.Create;
  try
    try
      FHeaderStream.Seek (0, soFromBeginning);

      {$IFDEF FPC}
      if (ZCompressStream2 (FHeaderStream, hs, Z_DEFAULT_COMPRESSION, -15, 9, Z_DEFAULT_STRATEGY) < 0) then
        raise ERecAnalystException.Create (c_cannotcompress);
      // zError (code)
      {$ELSE}
      ZCompressStream2 (FHeaderStream, hs, zcDefault, -15, 9, zsDefault);
      {$ENDIF}
      header_len := hs.Size + SizeOf (header_len);
      if FIsMgx then
        Inc (header_len, SizeOf (next_pos));
      hs.Seek (0, soFromBeginning);
      FBodyStream.Seek (0, soFromBeginning);
      outStream.Write (header_len, SizeOf (header_len));
      if FIsMgx then
        outStream.Write (next_pos, SizeOf (next_pos));
      outStream.CopyFrom (hs, hs.Size);
      outStream.CopyFrom (FBodyStream, FBodyStream.Size);

      outStream.SaveToFile (AFileName);
    except
      {$IFNDEF LIB}
      on ERecAnalystException do
        raise;
      {$ENDIF}
      on EReadError do
        {$IFDEF LIB}FLastError := RECANALYST_FILEREAD{$ELSE}raise ERecAnalystException.Create (c_cannotreadsection){$ENDIF};
      on EFCreateError do
        {$IFDEF LIB}FLastError := RECANALYST_FILECREATE{$ELSE}raise ERecAnalystException.CreateFmt (c_cannotcreatefile, [FileName]){$ENDIF};
      {$IFNDEF FPC}
      on EZCompressionError do
        {$IFDEF LIB}FLastError := RECANALYST_COMP{$ELSE}raise ERecAnalystException.Create (c_cannotcompress){$ENDIF};
      {$ENDIF}
      else
        {$IFDEF LIB}FLastError := RECANALYST_UNKNOWN{$ELSE}raise ERecAnalystException.Create (c_unknown){$ENDIF};
    end;
  finally
    hs.Free;
    outStream.Free;
  end;
end;

function TRecAnalyst.AddComment(const Comment: String = ''): Boolean;
var
  objectives_len: Word;
  buff65536: array[0..65535] of Char;
  ObjectivesString: PChar;
  temp: TMemoryStream;
const
  CommentSeparator = #$0A#$0A;
begin
  Result := False;

  if not FAnalyzed then
    Exit;
  if not FKeepStreams then
    Exit;
  if (objectives_pos = 0) then
    Exit;
  if GameSettings.IsScenario then { scenarios are not supported for now }
    Exit;
  if (CommentString = Comment) then
    Exit;

  temp := TMemoryStream.Create;
  try
    FHeaderStream.Seek (0, soFromBeginning);
    if (temp.CopyFrom (FHeaderStream, objectives_pos) <> objectives_pos) then
      Exit;

    FHeaderStream.Read (objectives_len, SizeOf (objectives_len));
    if (objectives_len > 0) then
    begin
      FillChar (buff65536, SizeOf (buff65536), #0);
      FHeaderStream.Read (buff65536[0], objectives_len);
//      ObjectivesString := PChar (buff65536 + CommentSeparator + Comment);
      ObjectivesString := PChar (GameSettings.ObjectivesString + CommentSeparator + Comment);
    end else
      ObjectivesString := PChar (CommentSeparator + Comment);

    if (Comment = '') then
      ObjectivesString := PChar (GameSettings.ObjectivesString);

    objectives_len := Length (ObjectivesString) + 1;  { null-terminator character }

    temp.Write (objectives_len, SizeOf (objectives_len));
    temp.Write (ObjectivesString[0], objectives_len);
    temp.CopyFrom (FHeaderStream, FHeaderStream.Size - FHeaderStream.Position);

    temp.Seek (0, soFromBeginning);
    FHeaderStream.Clear;
    FHeaderStream.CopyFrom (temp, temp.Size);

    Result := True;
  finally
    temp.Free;
  end;
end;

function TRecAnalyst.ReadPlayerInfoBlockEx: Boolean;
const
  exist_object_separator: array[0..8] of Char = (
    #$0B, #$00, #$08, #$00, #$00, #$00, #$02, #$00, #$00);
  object_end_separator: array[0..29] of Byte = (
    $FF, $FF, $FF, $FF, $00, $00, $80, $BF, $00, $00, $80, $BF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $00, $00);
  aok_object_end_separator: array[0..16] of Byte = (
   $FF, $FF, $FF, $FF, $00, $00, $80, $BF, $00, $00, $80, $BF, $00, $00, $00,
   $00, $00);
  player_info_end_separator: array[0..11] of Char = (
    #$00, #$0B, #$00, #$02, #$00, #$00, #$00, #$02, #$00, #$00, #$00, #$0B);
  objects_mid_separator_gaia: array[0..9] of Char = (
    #$00, #$0B, #$00, #$40, #$00, #$00, #$00, #$20, #$00, #$00);
var
  i, j: Integer;
  exist_object_pos: Longint;
  buff256: array[0..255] of Char;
  object_type{$IFNDEF LIB}, prev_object_type{$ENDIF}: Byte;
  unit_id{$IFNDEF LIB}, prev_unit_id{$ENDIF}: Word;
  owner, b: Byte;
  pos_x, pos_y: Single;
  Player, P: TPlayer;
  player_name_len: Word;
  food, wood, stone, gold, headroom, population, civilian_pop, military_pop, data6: Single;
  init_camera_pos_x, init_camera_pos_y: Single;
  civilization, player_color: Byte;
  GO: TGaiaObject;
  UO: TUnitObject;
  separator_ptr: Pointer;
  separator_len: Integer;
  num_player: Byte;
  map_size_x, map_size_y: Longint;
begin
  Result := False;
  num_player := Byte(Queue.Pop);
  map_size_x := Longint(Queue.Pop);
  map_size_y := Longint(Queue.Pop);
  try
    with FHeaderStream do
    begin
      for i := 0 to PlayerList.Count do { first is GAIA }
      begin
        if (i <> 0) then
        begin
          { skip GAIA player }
          Player := PlayerList[i - 1];
          { skip cooping player, she/he has no data in Player_info }
          P := PlayerList.GetPlayerByIndex(Player.Index);

          if (Assigned(P)) and (P <> Player) and (P.CivId <> cNone) then
          begin
            Player.CivId := P.CivId;
            Player.Civ := P.Civ;
            Player.ColorId := P.ColorId;
            {$IFDEF EXTENDED}Player.Color := P.Color;{$ENDIF}
            Player.Team := P.Team; { required }
            Player.IsCooping := True;
            Continue;
          end;
          if (FGameVersion = gvAOKTrial) or (FGameVersion = gvAOCTrial) then
            Seek(4);
          Seek(num_player + 43);

          { skip player name }
          ReadWord(player_name_len);
          Seek(player_name_len + 6);

          Queue.Push(Pointer(Position));
          { Civ_header }
          ReadFloat(food);
          ReadFloat(wood);
          ReadFloat(stone);
          ReadFloat(gold);
          { headroom = (house capacity - population) }
          ReadFloat(headroom);
          Seek(4);
          { Starting Age, note: PostImperial Age = Imperial Age here }
          ReadFloat(data6);
          Seek(16);
          ReadFloat(population);
          Seek(100);
          ReadFloat(civilian_pop);
          Seek(8);
          ReadFloat(military_pop);
          if FIsMgx then Seek(629) else Seek(593);
          ReadFloat(init_camera_pos_x);
          ReadFloat(init_camera_pos_y);
          if FIsMgx then Seek(9) else Seek(5);
          ReadChar(civilization);
          { sometimes(?) civilization is zero in scenarios when the first player is briton (only? always? rule?) }
          if (civilization = 0) then
            Inc(civilization);
          { skip unknown9[3] }
          Seek(3);
          ReadChar(player_color);

          with Player do
          begin
            CivId := TCivilization(civilization);
            SetCiv(TCivilization(civilization));
            ColorId := player_color;
            {$IFDEF EXTENDED}SetColor(player_color);{$ENDIF}
            InitialState.Position.X := Round(init_camera_pos_x);
            InitialState.Position.Y := Round(init_camera_pos_y);
            InitialState.Food := Round(food);
            InitialState.Wood := Round(wood);
            InitialState.Stone := Round(stone);
            InitialState.Gold := Round(gold);
            InitialState.StartingAge := TStartingAge(Round(data6));
            // TODO: Huns, Goths, Nomad etc. var...
            InitialState.HouseCapacity := Round(headroom) + Round(population);
            InitialState.Population := Round(population);
            InitialState.CivilianPop := Round(civilian_pop);
            InitialState.MilitaryPop := Round(military_pop);
            InitialState.ExtraPop := InitialState.Population - (InitialState.CivilianPop + InitialState.MilitaryPop);
          end;
        end;

        if (i = 0) then
        begin
          { GAIA }
          if (FGameVersion = gvAOKTrial) or (FGameVersion = gvAOCTrial) then
            Seek(4);
          Seek(num_player + 70);
          if FIsMgx then Seek(792) else Seek(756);
        end;

        if FIsMgx then Seek(41249) else Seek(34277);
        Seek(map_size_x * map_size_y);

        { Getting exist_object_pos }
        exist_object_pos := 0;
        repeat
          ReadBuffer(buff256, SizeOf(exist_object_separator));
          if CompareMem(@buff256, @exist_object_separator, SizeOf(exist_object_separator)) then
          begin
            exist_object_pos := Position;
            Break;
          end;
          Seek(-SizeOf(exist_object_separator) + 1);
        until (Position >= Size);

        if (exist_object_pos = 0) then
        begin
          {$IFNDEF LIB}
          { we don't raise exception here }
          Logger.SendError('Exist_Object block has not been found.', True);
          {$ENDIF}
          Exit;
        end;

        while True do
        begin
          {$IFNDEF LIB}
          prev_object_type := object_type;
          prev_unit_id := unit_id;
          {$ENDIF}

          ReadChar(object_type);
          ReadChar(owner);
          ReadWord(unit_id);
          case object_type of
            10:
              begin
                case unit_id of
                  uiGoldMine, uiStoneMine, uiCliff1..uiCliff10, uiForageBush:
                    begin
                      Seek(19);
                      ReadFloat(pos_x);
                      ReadFloat(pos_y);
                      GO := TGaiaObject.Create;
                      GO.Id := unit_id;
                      GO.Position.X := Round(pos_x);
                      GO.Position.Y := Round(pos_y);
                      GaiaObjects.Add(GO);
                      Seek(- 19 - SizeOf(pos_x) - SizeOf(pos_y));
                    end;
                end;
                Seek(63 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                if FIsMgl then
                  Seek(1);
              end;
            20:
              begin
                // not guaranteed
                if FIsMgx then
                begin
                  Seek(59);
                  ReadChar(b);
                  Seek(-59 - SizeOf(b));
                  Seek(68 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  if (b = 2) then
                    Seek(34);
                end else
                  Seek(103 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id))
              end;
            30:
              begin
                if FIsMgx then
                begin
                  Seek(59);
                  ReadChar(b);
                  Seek(-59 - SizeOf(b));
                  Seek(204 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  if (b = 2) then
                    Seek(17);
                end else
                begin
                  Seek(60);
                  ReadChar(b);
                  Seek(-60 - SizeOf(b));
                  Seek(205 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  if (b = 2) then
                    Seek(17);
                end;
             end;
            60:
              begin
                // not guaranteed
                Seek(204);
                ReadChar(b);
                Seek(-204 - SizeOf(b));
                Seek(233 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                if (b <> 0) then
                  Seek(67);
              end;
            70:
              begin
                case unit_id of
                  uiRelic, uiDeer, uiBoar, uiJavelina, uiTurkey, uiSheep:
                    begin
                      Seek(19);
                      ReadFloat(pos_x);
                      ReadFloat(pos_y);
                      GO := TGaiaObject.Create;
                      GO.Id := unit_id;
                      GO.Position.X := Round(pos_x);
                      GO.Position.Y := Round(pos_y);
                      GaiaObjects.Add(GO);
                    end;
                end;
                if (owner <> 0) and (unit_id <> uiTurkey) and (unit_id <> uiSheep) then
                begin
                  { exclude convertable objects }
                  Seek(19);
                  ReadFloat(pos_x);
                  ReadFloat(pos_y);
                  UO := TUnitObject.Create;
                  UO.Id := unit_id;
                  UO.Owner := owner;
                  UO.Position.X := Round(pos_x);
                  UO.Position.Y := Round(pos_y);
                  PlayerObjects.Add(UO);
                end;
                if FIsMgx then
                begin
                  separator_ptr := @object_end_separator;
                  separator_len := SizeOf(object_end_separator);
                end else
                begin
                  separator_ptr := @aok_object_end_separator;
                  separator_len := SizeOf(aok_object_end_separator);
                end;
                { search up to 1000 bytes }
                for j := 0 to 1000 do
                begin
                  ReadBuffer(buff256, separator_len);
                  if CompareMem(@buff256, separator_ptr, separator_len) then
                    Break;
                  Seek(-separator_len + 1);
                end;
                if (j > 1000) then
                begin
                  {$IFNDEF LIB}
                  Logger.SendError('object_end_separator has not been found for object_type=%d, owner=%d',
                    [object_type, owner], True);
                  {$ENDIF}
                  Exit;
                end;
              end;
            80:
              begin
                if (owner <> 0) then
                begin
                  Seek(19);
                  ReadFloat(pos_x);
                  ReadFloat(pos_y);
                  UO := TUnitObject.Create;
                  UO.Id := unit_id;
                  UO.Owner := owner;
                  UO.Position.X := Round(pos_x);
                  UO.Position.Y := Round(pos_y);
                  PlayerObjects.Add(UO);
                end;
                if FIsMgx then
                begin
                  separator_ptr := @object_end_separator;
                  separator_len := SizeOf(object_end_separator);
                end else
                begin
                  separator_ptr := @aok_object_end_separator;
                  separator_len := SizeOf(aok_object_end_separator);
                end;
                { search up to 1000 bytes }
                for j := 0 to 1000 do
                begin
                  ReadBuffer(buff256, separator_len);
                  if CompareMem(@buff256, separator_ptr, separator_len) then
                    Break;
                  Seek(-separator_len + 1);
                end;
                if (j > 1000) then
                begin
                  {$IFNDEF LIB}  
                  Logger.SendError('object_end_separator has not been found for object_type=%d, owner=%d',
                    [object_type, owner], True);
                  {$ENDIF}
                  Exit;
                end;
                Seek(126);
                if FIsMgx then
                  Seek(1);
              end;
            00:
              begin
                Seek(-(SizeOf(object_type) + SizeOf(owner) + SizeOf(unit_id))); // -4

                ReadBuffer(buff256, SizeOf(player_info_end_separator));
                Seek(-SizeOf(player_info_end_separator));

                if CompareMem(@buff256, @player_info_end_separator, SizeOf(player_info_end_separator)) then
                begin
                  Seek(SizeOf(player_info_end_separator));
                  Break;
                end;
                if CompareMem(@buff256, @objects_mid_separator_gaia, 2) then
                  Seek(SizeOf(objects_mid_separator_gaia))
                else
                begin
                  {$IFNDEF LIB}
                  Logger.SendError('Incorrect data has been detected at %x: prev_object_type=%d, prev_unit_id=%d, owner=%d',
                    [Position, prev_object_type, prev_unit_id, owner], True);
                  {$ENDIF}
                  Exit;
                end;
              end;

            else
              begin
                {$IFNDEF LIB}
                Logger.SendError('Incorrect data has been detected at %x: prev_object_type=%d, prev_unit_id=%d, owner=%d',
                  [Position, prev_object_type, prev_unit_id, owner], True);
                {$ENDIF}
                Exit;
              end;
          end;
        end;
      end;
    end;
    Result := True;
  except
    {$IFNDEF LIB}
    on E: Exception do
      Logger.SendException(E, True);
    {$ENDIF}
  end;
end;
{$IFDEF LIB}
function TRecAnalyst.GetLastErrorCode: Integer;
begin
  Result := FLastError;
end;

function TRecAnalyst.GetLastErrorMsg: PChar;
begin
  Result := TRecAnalyst.ErrorCodeToString (FLastError);
end;

class function TRecAnalyst.ErrorCodeToString(const ErrorCode: Integer): PChar;
begin
  try
    Result := PChar (ErrorMessages[ErrorCode]);
  except
    Result := '';
  end;
end;
{$ENDIF}
function UnitsCompare(Item1, Item2: Pointer): Integer;
begin
  if TTrainedUnit(Item1).Count < TTrainedUnit(Item2).Count then
    Result := 1
  else if TTrainedUnit(Item1).Count > TTrainedUnit(Item2).Count then
    Result := -1
  else
    Result := 0;
end;

function ResearchesCompare(Item1, Item2: Pointer): Integer;
begin
  if TResearch (Item1).Time < TResearch (Item2).Time then
    Result := -1
  else if TResearch (Item1).Time > TResearch (Item2).Time then
    Result := 1
  else
    Result := 0;
end;

function ChatCompare(Item1, Item2: Pointer): Integer;
begin
  if TChatMessage(Item1).Time < TChatMessage(Item2).Time then
    Result := -1
  else if TChatMessage(Item1).Time > TChatMessage(Item2).Time then
    Result := 1
  else
    Result := 0;
end;

function GaiaObjectsCompare(Item1, Item2: Pointer): Integer;
begin
  if (TGaiaObject(Item1).Id = uiRelic) and (TGaiaObject(Item2).Id <> uiRelic) then
    Result := 1
  else if (InArray (CliffsAry, TGaiaObject(Item1).Id) <> -1)
      and (InArray (CliffsAry, TGaiaObject(Item2).Id) = -1) then
    Result := -1
  else if (TGaiaObject(Item2).Id = uiRelic) and (TGaiaObject(Item1).Id <> uiRelic) then
    Result := -1
  else if (InArray (CliffsAry, TGaiaObject(Item2).Id) <> -1)
      and (InArray (CliffsAry, TGaiaObject(Item1).Id) = -1) then
    Result := 1
  else
    Result := 0;
end;

function ItemById(const Ary: array of TResearchRec; const Id: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low (Ary) to High (Ary) do
  begin
    if (Ary[i].Id = Id) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function ResearchById(const Id: Integer): Integer;
begin
  Result := ItemById (RESEARCHES, Id);
end;

function UnitById(const Id: Integer): Integer;
begin
  Result := ItemById (UNITS, Id);
end;

function BuildingById(const Id: Integer): Integer;
begin
  Result := ItemById (BUILDINGS, Id);
end;

function MapById(const Id: Integer): Integer;
begin
  Result := ItemById (MAPS, Id);
end;
{$IFDEF LIB}
function InArray(const Ary: array of Integer; Value: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Ary) to High(Ary) do
    if (Ary[i] = Value) then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}
{$IFDEF FPC}
function ZDecompressStream2(inStream, outStream: TStream; windowBits: Integer): Integer;
const
  bufferSize = 32768;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array [0..bufferSize-1] of Byte;
  outBuffer: array [0..bufferSize-1] of Byte;
  outSize: Integer;
begin
  Result := Z_OK;
  FillChar (zstream, SizeOf (zstream), 0);

  zresult := InflateInit2 (zstream, windowBits);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;

  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read (inBuffer, bufferSize);

  while zstream.avail_in > 0 do
  begin
    zstream.next_in := inBuffer;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      zresult := inflate (zstream, Z_NO_FLUSH);
      if (zresult < 0) then
      begin
        Result := zresult;
        Exit;
      end;

      outSize := bufferSize - zstream.avail_out;

      outStream.Write (outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    if zresult <> Z_STREAM_END then
    begin
      zstream.avail_in := inStream.Read (inBuffer, bufferSize);
    end
    else if zstream.avail_in > 0 then
    begin
      inStream.Position := inStream.Position - zstream.avail_in;
      zstream.avail_in := 0;
    end;
  end;

  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := inflate (zstream, Z_FINISH);
    if (zresult < 0) then
    begin
      { TODO: check why this sometimes flushes an error for fpc }
      //Result := zresult;
      Result := Z_OK;
      Exit;
    end;

    outSize := bufferSize - zstream.avail_out;

    outStream.Write (outBuffer, outSize);
  end;

  zresult := inflateEnd (zstream);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;
end;

function ZCompressStream2(inStream, outStream: TStream; level, windowBits, memLevel, strategy : Longint): Integer;
const
  bufferSize = 32768;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array [0..bufferSize-1] of Byte;
  outBuffer: array [0..bufferSize-1] of Byte;
  outSize: Integer;
begin
  Result := Z_OK;
  FillChar (zstream, SizeOf (zstream), 0);

  zresult := DeflateInit2 (zstream, level, Z_DEFLATED, windowBits, memLevel, strategy);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;

  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read (inBuffer, bufferSize);

  while zstream.avail_in > 0 do
  begin
    zstream.next_in := inBuffer;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      zresult := deflate (zstream, Z_NO_FLUSH);
      if (zresult < 0) then
      begin
        Result := zresult;
        Exit;
      end;

      outSize := bufferSize - zstream.avail_out;

      outStream.Write (outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    zstream.avail_in := inStream.Read (inBuffer, bufferSize);
  end;

  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := deflate (zstream, Z_FINISH);
    if (zresult < 0) then
    begin
      { TODO: check, ci robi to iste ako pri inflate }
      //Result := zresult;
      Result := Z_OK;
      Exit;
    end;

    outSize := bufferSize - zstream.avail_out;

    outStream.Write (outBuffer, outSize);
  end;

  zresult := inflateEnd (zstream);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;
end;
{$ENDIF}
end.
