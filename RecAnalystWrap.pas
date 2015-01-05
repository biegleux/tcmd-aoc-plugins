{*
 * $Id$
 * This file is part of the A2LView project.
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
unit RecAnalystWrap;

interface

uses
  SysUtils, Classes, Contnrs, recanalystd;

type
  { TGameSettings }
  TGameSettings = class(TObject)
  private
    fGameType: String;
    fMapStyle: String;
    fDifficultyLevel: String;
    fGameSpeed: String;
    fRevealMap: String;
    fMapSize: String;
    fMap: String;
    fPlayersType: String;
    fPOV: String;
    fPopLimit: Integer;
    fPlayTime: Integer;
    fInGameCoop: Boolean;
    fIsScenario: Boolean;
    fIsFFA: Boolean;
    fScFileName: String;
    fGameVersion: String;
    fVictory: String;
  public
    property GameType: String read fGameType;
    property MapStyle: String read fMapStyle;
    property DifficultyLevel: String read fDifficultyLevel;
    property GameSpeed: String read fGameSpeed;
    property RevealMap: String read fRevealMap;
    property MapSize: String read fMapSize;
    property Map: String read fMap;
    property PlayersType: String read fPlayersType;
    property POV: String read fPOV;
    property PopLimit: Integer read fPopLimit;
    property PlayTime: Integer read fPlayTime;
    property InGameCoop: Boolean read fInGameCoop;
    property IsScenario: Boolean read fIsScenario;
    property IsFFA: Boolean read fIsFFA;
    property ScFileName: String read fScFileName;
    property GameVersion: String read fGameVersion;
    property Victory: String read fVictory;
  end;

  { TPlayer }
  TPlayer = class(TObject)
  private
    fName: String;
    fIndex: Integer;
    fHuman: Boolean;
    fTeam: Integer;
    fOwner: Boolean;
    fCiv: String;
    fCivId: Integer;
    fColorId: Integer;
    fColor: Cardinal;
    fIsCooping: Boolean;
    fFeudalTime: Integer;
    fCastleTime: Integer;
    fImperialTime: Integer;
    fResignTime: Integer;
    fDisconnectTime: Integer;
  public
    property Name: String read fName;
    property Index: Integer read fIndex;
    property Human: Boolean read fHuman;
    property Team: Integer read fTeam;
    property Owner: Boolean read fOwner;
    property Civ: String read fCiv;
    property CivId: Integer read fCivId;
    property ColorId: Integer read fColorId;
    property Color: Cardinal read fColor;
    property IsCooping: Boolean read fIsCooping;
    property FeudalTime: Integer read fFeudalTime;
    property CastleTime: Integer read fCastleTime;
    property ImperialTime: Integer read fImperialTime;
    property ResignTime: Integer read fResignTime;
    property DisconnectTime: Integer read fDisconnectTime;
  end;

  { TChatMessage }
  TChatMessage = class(TObject)
  private
    fTime: Integer;
    fPlayer: TPlayer;
    fMsg: String;
  public
    property Time: Integer read fTime;
    property Player: TPlayer read fPlayer;
    property Msg: String read fMsg;
  end;

  { TTribute }
  TTribute = class(TObject)
  private
    fTime: Integer;
    fPlayerFrom: TPlayer;
    fPlayerTo: TPlayer;
    fResource: String;
    fAmount: Integer;
    fFee: Single;
  public
    property Time: Integer read fTime;
    property PlayerFrom: TPlayer read fPlayerFrom;
    property PlayerTo: TPlayer read fPlayerTo;
    property Resource: String read fResource;
    property Amount: Integer read fAmount;
    property Fee: Single read fFee;
  end;

  {TResearch }
  TResearch = class(TObject)
  private
    fTime: Integer;
    fId: Integer;
    fPlayer: TPlayer;
    fName: String;
    function GetResourceName(): String;
  public
    property Time: Integer read fTime;
    property Id: Integer read fId;
    property Player: TPlayer read fPlayer;
    property Name: String read fName;
    property ResourceName: String read GetResourceName;
  end;

  { TRecAnalyst }
  ERecAnalystException = Exception;

  TRecAnalyst = class(TObject)
  private
    lpRecAnalyst: PRecAnalyst;
    fGameSettings: TGameSettings;
    fPlayers: TObjectList;
    procedure FillGameSettings(const GameSettingsStruct: TGameSettingsStruct);
    procedure AddChatMessage(const ChatMessageStruct: TChatMessageStruct;
      ChatMessages: TObjectList);
    procedure RaiseExceptionIfError(Code: Integer);
  public
    PreGameChatMessages: TObjectList;
    InGameChatMessages: TObjectList;
    Tributes: TObjectList;
    Researches: TObjectList;
    constructor Create();
    destructor Destroy(); override;
    procedure Analyze(const FileName: String);
    function GenerateMap(Width, Height: Integer): TMemoryStream;
    function GetPlayerByIndex(Index: Integer): TPlayer;
    class function GameTimeToString(Time: Integer): String;
    property GameSettings: TGameSettings read fGameSettings;
    property Players: TObjectList read fPlayers;
  end;

const
  PLAYER_COLORS: array[0..7] of Cardinal = (
    $00ff0000,
    $000000ff,
    $0000c800,
    $0000d0d0,
    $00c8c800,
    $00ff00ff,
    $00808080,
    $000182ff);

  RESOURCES: array[0..3] of String = (
    'food', 'wood', 'stone', 'gold'
  );

implementation

uses
  Windows;

type
  TResearchRec = record
    Id: Integer;
    ResName: String;
  end;

{$I id_researches.inc}

const
  RESEARCHES_NUM = 139 + 35;
  RESEARCHES: array[0..RESEARCHES_NUM - 1] of TResearchRec = (
    // town center
    (Id: riFeudalAge;           ResName: 'feudal_age'),
    (Id: riCastleAge;           ResName: 'castle_age'),
    (Id: riImperialAge;         ResName: 'imperial_age'),
    (Id: riLoom;                ResName: 'loom'),
    (Id: riWheelBarrow;         ResName: 'wheel_barrow'),
    (Id: riHandCart;            ResName: 'hand_cart'),
    (Id: riTownWatch;           ResName: 'town_watch'),
    (Id: riTownPatrol;          ResName: 'town_patrol'),
    // mill
    (Id: riHorseCollar;         ResName: 'horse_collar'),
    (Id: riHeavyPlow;           ResName: 'heavy_plow'),
    (Id: riCropRotation;        ResName: 'crop_rotation'),
    // lumber camp
    (Id: riDoubleBitAxe;        ResName: 'double_bit_axe'),
    (Id: riBowSaw;              ResName: 'bow_saw'),
    (Id: riTwoManSaw;           ResName: 'two_man_saw'),
    // mining camp
    (Id: riStoneMining;         ResName: 'stone_mining'),
    (Id: riGoldMining;          ResName: 'gold_mining'),
    (Id: riStoneShaftMining;    ResName: 'stone_shaft_mining'),
    (Id: riGoldShaftMining;     ResName: 'gold_shaft_mining'),
    // blacksmith
    (Id: riPaddedArcherArmor;   ResName: 'padded_archer_armor'),
    (Id: riLeatherArcherArmor;  ResName: 'leather_archer_armor'),
    (Id: riRingArcherArmor;     ResName: 'ring_archer_armor'),
    (Id: riFletching;           ResName: 'fletching'),
    (Id: riBodkinArrow;         ResName: 'bodkin_arrow'),
    (Id: riBracer;              ResName: 'bracer'),
    (Id: riForging;             ResName: 'forging'),
    (Id: riIronCasting;         ResName: 'iron_casting'),
    (Id: riBlastFurnace;        ResName: 'blast_furnace'),
    (Id: riScaleBardingArmor;   ResName: 'scale_barding'),
    (Id: riChainBardingArmor;   ResName: 'chain_barding'),
    (Id: riPlateBardingArmor;   ResName: 'plate_barding'),
    (Id: riScaleMailArmor;      ResName: 'scale_mail'),
    (Id: riChainMailArmor;      ResName: 'chain_mail'),
    (Id: riPlateMailArmor;      ResName: 'plate_mail'),
    // university
    (Id: riMasonry;             ResName: 'masonry'),
    (Id: riFortifiedWall;       ResName: 'fortified_wall'),
    (Id: riBallistics;          ResName: 'ballistics'),
    (Id: riGuardTower;          ResName: 'guard_tower'),
    (Id: riHeatedShot;          ResName: 'heated_shot'),
    (Id: riMurderHoles;         ResName: 'murder_holes'),
    (Id: riTreadmillCrane;      ResName: 'treadmill_crane'),
    (Id: riArchitecture;        ResName: 'architecture'),
    (Id: riChemistry;           ResName: 'chemistry'),
    (Id: riSiegeEngineers;      ResName: 'siege_engineers'),
    (Id: riKeep;                ResName: 'keep'),
    (Id: riBombardTower;        ResName: 'bombard_tower'),
    // monastery
    (Id: riRedemption;          ResName: 'redemption'),
    (Id: riFervor;              ResName: 'fervor'),
    (Id: riSanctity;            ResName: 'sanctity'),
    (Id: riAtonement;           ResName: 'atonement'),
    (Id: riHerbalMedicine;      ResName: 'herbal_medicine'),
    (Id: riHeresy;              ResName: 'heresy'),
    (Id: riBlockPrinting;       ResName: 'block_printing'),
    (Id: riIllumination;        ResName: 'illumination'),
    (Id: riFaith;               ResName: 'faith'),
    (Id: riTheocracy;           ResName: 'theocracy'),
    // market
    (Id: riCartography;         ResName: 'cartography'),
    (Id: riCaravan;             ResName: 'caravan'),
    (Id: riGuilds;              ResName: 'guilds'),
    (Id: riCoinage;             ResName: 'coinage'),
    (Id: riBanking;             ResName: 'banking'),
    // castle
    (Id: riHoardings;           ResName: 'hoardings'),
    (Id: riSappers;             ResName: 'sappers'),
    (Id: riConscription;        ResName: 'conscription'),
    (Id: riSpiesTreason;        ResName: 'spy'),
    // barrack
    (Id: riManAtArms;           ResName: 'man_at_arms'),
    (Id: riLongSwordsman;       ResName: 'long_swordsman'),
    (Id: riTwoHandedSwordsman;  ResName: 'two_handed_swordsman'),
    (Id: riChampion;            ResName: 'champion'),
    (Id: riPikeman;             ResName: 'pikeman'),
    (Id: riHalberdier;          ResName: 'halberdier'),
    (Id: riEliteEagleWarrior;   ResName: 'eagle_warrior'),
    (Id: riTracking;            ResName: 'tracking'),
    (Id: riSquires;             ResName: 'squires'),
    // archery range
    (Id: riCrossbow;            ResName: 'crossbow'),
    (Id: riArbalest;            ResName: 'arbalest'),
    (Id: riEliteSkirmisher;     ResName: 'elite_skirmisher'),
    (Id: riHeavyCavalryArcher;  ResName: 'heavy_cavalry_archer'),
    (Id: riThumbRing;           ResName: 'thumb_ring'),
    (Id: riParthianTactics;     ResName: 'parthian_tactics'),
    // stable
    (Id: riLightCavalry;        ResName: 'light_cavalry'),
    (Id: riHussar;              ResName: 'hussar'),
    (Id: riCavalier;            ResName: 'cavalier'),
    (Id: riPaladin;             ResName: 'paladin'),
    (Id: riHeavyCamel;          ResName: 'heavy_camel'),
    (Id: riBloodlines;          ResName: 'bloodlines'),
    (Id: riHusbandry;           ResName: 'husbandry'),
    // siege workshop
    (Id: riOnager;              ResName: 'onager'),
    (Id: riSiegeOnager;         ResName: 'siege_onager'),
    (Id: riCappedRam;           ResName: 'capped_ram'),
    (Id: riSiegeRam;            ResName: 'siege_ram'),
    (Id: riHeavyScorpion;       ResName: 'heavy_scorpion'),
    // dock
    (Id: riWarGalley;           ResName: 'war_galley'),
    (Id: riGalleon;             ResName: 'galleon'),
    (Id: riFastFireShip;        ResName: 'fast_fire_ship'),
    (Id: riHeavyDemolitionShip; ResName: 'heavy_demolition_ship'),
    (Id: riCannonGalleon;       ResName: 'cannon_galleon'),
    (Id: riEliteCannonGalleon;  ResName: 'cannon_galleon'),
    (Id: riCareening;           ResName: 'careening'),
    (Id: riDryDock;             ResName: 'dry_dock'),
    (Id: riShipwright;          ResName: 'shipwright'),
    // unique-unit-upgrade
    (Id: riEliteJaguarMan;      ResName: 'jaguar_man'),
    (Id: riEliteCataphract;     ResName: 'cataphract'),
    (Id: riEliteWoadRaider;     ResName: 'woad_raider'),
    (Id: riEliteChuKoNu;        ResName: 'chu_ko_nu'),
    (Id: riEliteLongbowman;     ResName: 'longbowman'),
    (Id: riEliteThrowingAxeman; ResName: 'throwing_axeman'),
    (Id: riEliteHuskarl;        ResName: 'huskarl'),
    (Id: riEliteTarkan;         ResName: 'tarkan'),
    (Id: riEliteSamurai;        ResName: 'samurai'),
    (Id: riEliteWarWagon;       ResName: 'war_wagon'),
    (Id: riEliteTurtleShip;     ResName: 'turtle_ship'),
    (Id: riElitePlumedArcher;   ResName: 'plumed_archer'),
    (Id: riEliteMangudai;       ResName: 'mangudai'),
    (Id: riEliteWarElephant;    ResName: 'war_elephant'),
    (Id: riEliteMameluke;       ResName: 'mameluke'),
    (Id: riEliteConquistador;   ResName: 'conquistador'),
    (Id: riEliteTeutonicKnight; ResName: 'teutonic_knight'),
    (Id: riEliteJanissary;      ResName: 'janissary'),
    (Id: riEliteBerserk;        ResName: 'berserk'),
    (Id: riEliteLongboat;       ResName: 'longboat'),
    // unique-research
    (Id: riGarlandWars;         ResName: 'unique_tech'),
    (Id: riLogistica;           ResName: 'unique_tech'),
    (Id: riFurorCeltica;        ResName: 'unique_tech'),
    (Id: riRocketry;            ResName: 'unique_tech'),
    (Id: riYeomen;              ResName: 'unique_tech'),
    (Id: riBeardedAxe;          ResName: 'unique_tech'),
    (Id: riAnarchy;             ResName: 'unique_tech'),
    (Id: riPerfusion;           ResName: 'unique_tech'),
    (Id: riAtheism;             ResName: 'unique_tech'),
    (Id: riKataparuto;          ResName: 'unique_tech'),
    (Id: riShinkichon;          ResName: 'unique_tech'),
    (Id: riElDorado;            ResName: 'unique_tech'),
    (Id: riDrill;               ResName: 'unique_tech'),
    (Id: riMahouts;             ResName: 'unique_tech'),
    (Id: riZealotry;            ResName: 'unique_tech'),
    (Id: riSupremacy;           ResName: 'unique_tech'),
    (Id: riCrenellations;       ResName: 'unique_tech'),
    (Id: riArtillery;           ResName: 'unique_tech'),
    (Id: riBerserkergang;       ResName: 'unique_tech'),
    // AoFE
    (Id: riHuntingDogs;         ResName: 'hunting_dogs'),
    (Id: riImperialCamel;       ResName: 'imperial_camel'),
    (Id: riCouriers;            ResName: 'unique_tech'),
    (Id: riAndeanSling;         ResName: 'unique_tech2'),
    (Id: riRecurveBow;          ResName: 'unique_tech'),
    (Id: riMercenaries;         ResName: 'unique_tech2'),
    (Id: riDruzhina;            ResName: 'unique_tech'),
    (Id: riOrthodoxy;           ResName: 'unique_tech2'),
    (Id: riShatagni;            ResName: 'unique_tech'),
    (Id: riSultans;             ResName: 'unique_tech2'),
    (Id: riSilkRoad;            ResName: 'unique_tech'),
    (Id: riPavise;              ResName: 'unique_tech2'),
    (Id: riChivalry;            ResName: 'unique_tech2'),
    (Id: riInquisition;         ResName: 'unique_tech2'),
    (Id: riSipahi;              ResName: 'unique_tech2'),
    (Id: riMadrasah;            ResName: 'unique_tech2'),
    (Id: riIronclad;            ResName: 'unique_tech2'),
    (Id: riBoilingOil;          ResName: 'unique_tech2'),
    (Id: riNomads;              ResName: 'unique_tech2'),
    (Id: riPanokseon;           ResName: 'unique_tech2'),
    (Id: riTlatoani;            ResName: 'unique_tech2'),
    (Id: riMarauders;           ResName: 'unique_tech2'),
    (Id: riStronghold;          ResName: 'unique_tech2'),
    (Id: riGreekFire;           ResName: 'unique_tech2'),
    (Id: riChieftains;          ResName: 'unique_tech2'),
    (Id: riGreatWall;           ResName: 'unique_tech2'),
    (Id: riWarwolf;             ResName: 'unique_tech2'),
    (Id: riAtlatl;              ResName: 'unique_tech2'),
    (Id: riEagleWarrior;        ResName: 'heavy_eagle_warrior'),
    (Id: riGillnets;            ResName: 'gillnets'),
    (Id: riEliteKamayuk;        ResName: 'kamayuk'),
    (Id: riEliteBoyar;          ResName: 'boyar'),
    (Id: riEliteElephantArcher; ResName: 'elephant_archer'),
    (Id: riEliteMagyarHuszar;   ResName: 'magyar_huszar'),
    (Id: riEliteGenoeseCrossbowman; ResName: 'genoese_crossbowman'));

function ResearchById(const Id: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(RESEARCHES) to High(RESEARCHES) do
  begin
    if (RESEARCHES[i].Id = Id) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function PlayersCompare(Item1, Item2: Pointer): Integer;
begin
  if (TPlayer(Item1).Team < TPlayer(Item2).Team) then Result := -1
  else if (TPlayer(Item1).Team > TPlayer(Item2).Team) then Result := 1
  else if (TPlayer(Item1).Index < TPlayer(Item2).Index) then Result := -1
  else if (TPlayer(Item1).Index > TPlayer(Item2).Index) then Result := 1
  else if (not TPlayer(Item1).IsCooping and TPlayer(Item2).IsCooping) then Result := -1
  else if (TPlayer(Item1).IsCooping and not TPlayer(Item2).IsCooping) then Result := 1
  else Result := 0;
end;

function EnumPlayers(lpPlayer: PPlayerStruct; lParam: LPARAM): BOOL; stdcall;
var
  Player: TPlayer;
  PlayerList: TObjectList;
begin
  PlayerList := TObjectList(lParam);
  Player := TPlayer.Create();
  with Player, lpPlayer^ do
  begin
    fName := String(AnsiString(szName));
    fIndex := dwIndex;
    fTeam := dwTeam;
    fCiv := String(AnsiString(szCivilization));
    fCivId := dwCivId;
    fColor := PLAYER_COLORS[dwColorId];
    fColorId := dwColorId;
    fIsCooping := bIsCooping;
    fFeudalTime := dwFeudalTime;
    fCastleTime := dwCastleTime;
    fImperialTime := dwImperialTime;
    fResignTime := dwResignTime;
    fDisconnectTime := dwDisconnectTime;
  end;
  PlayerList.Add(Player);
  Result := True;
end;

{ enumeration procedure callbacks }
function EnumPreGameChatMessages(lpChatMessage: PChatMessageStruct;
  lParam: LPARAM): BOOL; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  RecAnalyst := TRecAnalyst(lParam);
  RecAnalyst.AddChatMessage(lpChatMessage^, RecAnalyst.PreGameChatMessages);
  Result := True;
end;

function EnumInGameChatMessages(lpChatMessage: PChatMessageStruct;
  lParam: LPARAM): BOOL; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  RecAnalyst := TRecAnalyst(lParam);
  RecAnalyst.AddChatMessage(lpChatMessage^, RecAnalyst.InGameChatMessages);
  Result := True;
end;

function EnumTributes(lpTribute: PTributeStruct; lParam: LPARAM): BOOL; stdcall;
var
  RecAnalyst: TRecAnalyst;
  Tribute: TTribute;
begin
  RecAnalyst := TRecAnalyst(lParam);
  Tribute := TTribute.Create();
  with Tribute, lpTribute^ do
  begin
    fTime := dwTime;
    fPlayerFrom := RecAnalyst.GetPlayerByIndex(dwPlayerFrom);
    fPlayerTo := RecAnalyst.GetPlayerByIndex(dwPlayerTo);
    fResource := RESOURCES[byResourceId];
    fAmount := dwAmount;
    fFee := fFee;
  end;
  RecAnalyst.Tributes.Add(Tribute);
  Result := True;
end;

function EnumResearches(lpResearch: PResearchStruct; lParam: LPARAM): BOOL; stdcall;
var
  RecAnalyst: TRecAnalyst;
  Research: TResearch;
begin
  RecAnalyst := TRecAnalyst(lParam);
  Research := TResearch.Create();
  with Research, lpResearch^ do
  begin
    fTime := dwTime;
    fId := dwId;
    fPlayer := RecAnalyst.GetPlayerByIndex(dwPlayerId);
    fName := String(AnsiString(szName));
  end;
  RecAnalyst.Researches.Add(Research);
  Result := True;
end;

{ TResearch }
function TResearch.GetResourceName(): String;
var
  idx: Integer;
begin
  idx := ResearchById(fId);
  if (idx <> -1) then
    Result := RESEARCHES[idx].ResName
  else
    Result := '';
end;

{ TRecAnalyst }
constructor TRecAnalyst.Create();
begin
  inherited Create();
  if not RecAnalystLoadLoaded then
    raise ERecAnalystException.Create('RecAnalyst library has not been loaded.');

  lpRecAnalyst := recanalyst_create();

  if not Assigned(lpRecAnalyst) then
    raise ERecAnalystException.Create('Unable to create RecAnalyst object.');

  fGameSettings := TGameSettings.Create();
  fPlayers := TObjectList.Create();
  PreGameChatMessages := TObjectList.Create();
  InGameChatMessages := TObjectList.Create();
  Tributes := TObjectList.Create();
  Researches := TObjectList.Create();
end;

destructor TRecAnalyst.Destroy();
begin
  if Assigned(lpRecAnalyst) then
    recanalyst_free(lpRecAnalyst);
  fGameSettings.Free();
  fPlayers.Free();
  PreGameChatMessages.Free();
  InGameChatMessages.Free();
  Tributes.Free();
  Researches.Free();
  inherited Destroy();
end;

procedure TRecAnalyst.Analyze(const FileName: String);
var
  Code: Integer;
  GameSettingsStruct: TGameSettingsStruct;
  VictoryStruct: TVictoryStruct;
begin
  Code := recanalyst_analyze(lpRecAnalyst, PAnsiChar(AnsiString(FileName)));
  RaiseExceptionIfError(Code);

  GameSettingsStruct.lpVictory := @VictoryStruct;
  Code := recanalyst_getgamesettings(lpRecAnalyst, @GameSettingsStruct);
  RaiseExceptionIfError(Code);
  FillGameSettings(GameSettingsStruct);

  Code := recanalyst_enumplayers(lpRecAnalyst, EnumPlayers, LPARAM(fPlayers));
  RaiseExceptionIfError(Code);
  fPlayers.Sort(@PlayersCompare);

  Code := recanalyst_enumpregamechat(lpRecAnalyst, EnumPreGameChatMessages, LPARAM(Self));
  RaiseExceptionIfError(Code);

  Code := recanalyst_enumingamechat(lpRecAnalyst, EnumInGameChatMessages, LPARAM(Self));
  RaiseExceptionIfError(Code);

  Code := recanalyst_enumtributes(lpRecAnalyst, EnumTributes, LPARAM(Self));
  RaiseExceptionIfError(Code);

  Code := recanalyst_enumresearches(lpRecAnalyst, EnumResearches, LPARAM(Self));
  RaiseExceptionIfError(Code);
end;

function TRecAnalyst.GenerateMap(Width: Integer; Height: Integer): TMemoryStream;
var
  Size: Cardinal;
  Code: Integer;
  lpImageBuffer: Pointer;
  Stream: TMemoryStream;
begin
  Size := recanalyst_generatemap(lpRecAnalyst, Width, Height, nil);
  RaiseExceptionIfError(Size);
  GetMem(lpImageBuffer, Size);
  try
    Code := recanalyst_generatemap(lpRecAnalyst, Width, Height, lpImageBuffer);
    RaiseExceptionIfError(Code);
    Stream := TMemoryStream.Create();
    try
      Stream.Write(lpImageBuffer^, Size);
      Stream.Position := 0;
      Result := Stream;
    except
      Stream.Free();
      Result := nil;
      raise;
    end;
  finally
    FreeMem(lpImageBuffer);
  end;
end;

procedure TRecAnalyst.RaiseExceptionIfError(Code: Integer);
begin
  if (Code < RECANALYST_OK) then
    raise ERecAnalystException.Create(String(AnsiString(recanalyst_errmsg(Code))));
end;

procedure TRecAnalyst.FillGameSettings(const GameSettingsStruct: TGameSettingsStruct);
begin
  with GameSettings, GameSettingsStruct do
  begin
    fIsScenario := bIsScenario;
    fPopLimit := dwPopLimit;
    fPlayTime := dwPlayTime;
    fInGameCoop := bInGameCoop;
    fIsFFA := bIsFFA;
    fMap := String(AnsiString(szMap));
    fPlayersType := String(AnsiString(szPlayersType));
    fPOV := String(AnsiString(szPOV));
    fGameType := String(AnsiString(szGameType));
    fMapStyle := String(AnsiString(szMapStyle));
    fDifficultyLevel := String(AnsiString(szDifficultyLevel));
    fGameSpeed := String(AnsiString(szGameSpeed));
    fRevealMap := String(AnsiString(szRevealMap));
    fMapSize := String(AnsiString(szMapSize));
    fGameVersion := String(AnsiString(szVersion));
    fScFileName := String(AnsiString(szScFileName));
    fVictory := String(AnsiString(lpVictory^.szVictory));
  end;
end;

procedure TRecAnalyst.AddChatMessage(const ChatMessageStruct: TChatMessageStruct;
  ChatMessages: TObjectList);
var
  ChatMessage: TChatMessage;
begin
  ChatMessage := TChatMessage.Create();
  with ChatMessage, ChatMessageStruct do
  begin
    fTime := dwTime;
    fPlayer := GetPlayerByIndex(dwPlayerId);
    fMsg := String(AnsiString(szMessage));
  end;
  ChatMessages.Add(ChatMessage);
end;

function TRecAnalyst.GetPlayerByIndex(Index: Integer): TPlayer;
var
  i: Integer;
begin
  for i := 0 to fPlayers.Count - 1 do
  begin
    if (TPlayer(fPlayers[i]).Index = Index) then
    begin
      Result := TPlayer(fPlayers[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

class function TRecAnalyst.GameTimeToString(Time: Integer): String;
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
  s := IntToStr(hour);
  if (hour < 10) then s := '0' + s;
  Result := Result + s + ':';
  s := IntToStr(minute);
  if (minute < 10) then s := '0' + s;
  Result := Result + s + ':';
  s := IntToStr(second);
  if (second < 10) then s := '0' + s;
  Result := Result + s;
end;

end.

