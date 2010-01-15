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
unit RecAnalystConsts;

interface

{$DEFINE EXTENDED}

{$IFDEF EXTENDED}
uses
  Graphics;
{$ENDIF}

type
  TCivRec = record
    Name: String;
    ResName: String;
  end;

  TResearchRec = record
    Id: Integer;
    Name: String;
    ResName: String;
  end;

  TMapsRec = TResearchRec;
  TUnitRec = TResearchRec;
  TBuildingRec = TResearchRec;

  TGameVersion = (gvUnknown, gvAOK, gvAOKTrial, gvAOK20, gvAOK20a, gvAOC,
    gvAOCTrial, gvAOC10, gvAOC10c);
  TMapStyle = (msStandard, msRealWorld, msCustom);
  TDifficultyLevel = (dlHardest, dlHard, dlModerate, dlStandard {dlEasy}, dlEasiest);
  TGameType = (gtRandomMap, gtRegicide, gtDeathMatch, gtScenario, gtCampaign,
    gtKingOfTheHill, gtWonderRace, gtDefendWonder, gtTurboRandomMap);
  TGameSpeed = (gsSlow = 100, gsNormal = 150, gsFast = 200);
  TRevealMap = (rmNormal, rmExplored, rmAllVisible);
  TMapSize = (msTiny, msSmall, msMedium, msNormal, msLarge, msGiant);
  TCivilization = (cNone, cBritons, cFranks, cGoths, cTeutons, cJapanese, cChinese,
    cByzantines, cPersians, cSaracens, cTurks, cVikings, cMongols, cCelts, cSpanish,
    cAztecs, cMayans, cHuns, cKoreans);
  TResourceId = (rFood, rWood , rStone, rGold);
  TStartingAge = (saUnknown = -1, saDarkAge = 0, saFeudalAge = 1, saCastleAge = 2,
    saImperialAge = 3, saPostImperialAge = 4);
  TVictoryCondition = (vcStandard, vcConquest, vcTimeLimit, vcScoreLimit, vcCustom);

  {$I id_maps.inc}
  {$I id_researches.inc}
  {$I id_buildings.inc}
  {$I id_units.inc}
  {$I id_units_ex.inc}    

const
  { version strings }
  VER_94 = 'VER 9.4';
  VER_93 = 'VER 9.3';
  TRL_93 = 'TRL 9.3';

  MAPS_NUM = 37;
  MAPS: array[0..MAPS_NUM - 1] of TMapsRec = (
    (Id: miArabia;         Name: 'Arabia'),
    (Id: miArchipelago;    Name: 'Archipelago'),
    (Id: miBaltic;         Name: 'Baltic'),
    (Id: miBlackForest;    Name: 'Black Forest'),
    (Id: miCoastal;        Name: 'Coastal'),
    (Id: miContinental;    Name: 'Continental'),
    (Id: miCraterLake;     Name: 'Crater Lake'),
    (Id: miFortress;       Name: 'Fortress'),
    (Id: miGoldRush;       Name: 'Gold Rush'),
    (Id: miHighland;       Name: 'Highland'),
    (Id: miIslands;        Name: 'Islands'),
    (Id: miMediterranean;  Name: 'Mediterranean'),
    (Id: miMigration;      Name: 'Migration'),
    (Id: miRivers;         Name: 'Rivers'),
    (Id: miTeamIslands;    Name: 'Team Islands'),
    (Id: miRandom;         Name: 'Random'),
    (Id: miScandinavia;    Name: 'Scandinavia'),
    (Id: miMongolia;       Name: 'Mongolia'),
    (Id: miYucatan;        Name: 'Yucatan'),
    (Id: miSaltMarsh;      Name: 'Salt Marsh'),
    (Id: miArena;          Name: 'Arena'),
    (Id: miKingOfTheHill;  Name: 'King of the Hill'), //
    (Id: miOasis;          Name: 'Oasis'),
    (Id: miGhostLake;      Name: 'Ghost Lake'),
    (Id: miNomad;          Name: 'Nomad'),
    (Id: miIberia;         Name: 'Iberia'),
    (Id: miBritain;        Name: 'Britain'),
    (Id: miMideast;        Name: 'Mideast'),
    (Id: miTexas;          Name: 'Texas'),
    (Id: miItaly;          Name: 'Italy'),
    (id: miCentralAmerica; Name: 'Central America'),
    (Id: miFrance;         Name: 'France'),
    (Id: miNorseLands;     Name: 'Norse Lands'),
    (Id: miSeaOfJapan;     Name: 'Sea of Japan (East Sea)'),
    (Id: miByzantinum;     Name: 'Byzantinum'),
    (Id: miCustom;         Name: 'Custom'),
    (Id: miBlindRandom;    Name: 'Blind Random'));

  GAME_VERSIONS: array[TGameVersion] of String = (
    'Unknown', 'AOK', 'AOK Trial', 'AOK 2.0', 'AOK 2.0a', 'AOC', 'AOC Trial', 'AOC 1.0', 'AOC 1.0c');

  MAP_STYLES: array[TMapStyle] of String = (
    'Standard', 'Real World', 'Custom');

  AOC_DIFFICULTY_LEVELS: array[TDifficultyLevel] of String = (
    'Hardest', 'Hard', 'Moderate', 'Standard', 'Easiest');

  AOK_DIFFICULTY_LEVELS: array[TDifficultyLevel] of String = (
    'Hardest', 'Hard', 'Moderate', 'Easy', 'Easiest');

  GAME_TYPES: array[TGameType] of String = (
    'Random Map', 'Regicide', 'Death Match', 'Scenario', 'Campaign',
    'King of the Hill', 'Wonder Race', 'Defend the Wonder', 'Turbo Random Map');

  GAME_SPEEDS: array[0..2] of String = (
    'Slow', 'Normal', 'Fast');

  REVEAL_SETTINGS: array[TRevealMap] of String = (
    'Normal', 'Explored', 'All Visible');

  MAP_SIZES: array[TMapSize] of String = (
    'Tiny (2 players)', 'Small (3 players)', 'Medium (4 players)', 'Normal (6 players)',
    'Large (8 players)', 'Giant');

  STARTING_AGES: array[TStartingAge] of String = (
    'Unknown', 'Dark Age', 'Feudal Age', 'Castle Age', 'Imperial Age', 'Post-Imperial Age');

  VICTORY_CONDITIONS: array[TVictoryCondition] of String = (
    'Standard', 'Conquest', 'Time Limit', 'Score Limit', 'Custom');

  CIVS: array[TCivilization] of TCivRec = (
    (Name: 'None';       ResName: ''),
    (Name: 'Britons';    ResName: 'BRITONS'),
    (Name: 'Franks';     ResName: 'FRANKS'),
    (Name: 'Goths';      ResName: 'GOTHS'),
    (Name: 'Teutons';	   ResName: 'TEUTONS'),
    (Name: 'Japanese';   ResName: 'JAPANESE'),
    (Name: 'Chinese';    ResName: 'CHINESE'),
    (Name: 'Byzantines'; ResName: 'BYZANTINES'),
    (Name: 'Persians';   ResName: 'PERSIANS'),
    (Name: 'Saracens';   ResName: 'SARACENS'),
    (Name: 'Turks';      ResName: 'TURKS'),
    (Name: 'Vikings';    ResName: 'VIKINGS'),
    (Name: 'Mongols';    ResName: 'MONGOLS'),
    (Name: 'Celts';      ResName: 'CELTS'),
    (Name: 'Spanish';    ResName: 'SPANISH'),
    (Name: 'Aztecs';     ResName: 'AZTECS'),
    (Name: 'Mayans';     ResName: 'MAYANS'),
    (Name: 'Huns';       ResName: 'HUNS'),
    (Name: 'Koreans';    ResName: 'KOREANS'));

  HTMLCOLORS: array[0..7] of String = (
    '#0000ff',
    '#ff0000',
    '#00ff00',
    '#ffff00',
    '#00ffff',
    '#ff00ff',
    '#b9b9b9',
    '#ff8201');
{$IFDEF EXTENDED}
  COLORS: array[0..7] of TColor = (
    $00ff0000,
    $000000ff,
    $0000ff00,
    $0000ffff,
    $00ffff00,
    $00ff00ff,
    $00787878, // a bit darken
    $000182ff);
    
  ORIG_COLOR6 = $00b9b9b9;
{$ENDIF}
  TERRAIN_COLORS: array[$00..$28] of Cardinal = (
    $279733,  // Id: $00
    $b65d30,  // Id: $01
    $78b4e8,  // Id: $02
    $52a2e4,  // Id: $03
    $b09254,  // Id: $04
    $279733,  // Id: $05
    $52a2e4,  // Id: $06
    $4d8882,
    $4d8882,
    $279733,  // Id: $09
    $157615,  // Id: $0A
    $52a2e4,  // Id: $0B
    $279733,  // Id: $0C
    $157615,  // Id: $0D
    $78b4e8,  // Id: $0E
    $b65d30,
    $279733,
    $157615,  // Id: $11
    $157615,  // Id: $12
    $157615,  // Id: $13
    $157615,  // Id: $14
    $157615,  // Id: $15
    $a14a00,  // Id: $16
    $bb4a00,  // Id: $17
    $52a2e4,  // Id: $18
    $52a2e4,  // Id: $19
    $49ecff,
    $52a2e4,  // Id: $1B
    $b65d30,
    $4d8882,
    $4d8882,
    $4d8882,
    $ffd8c8,  // Id: $20
    $ffd8c8,  // Id: $21
    $ffd8c8,  // Id: $22
    $f0c098,  // Id: $23
    $ffd8c8,
    $f0c098,  // Id: $25
    $ffd8c8,  // Id: $26
    $ffd8c8,  // Id: $27
    $52a2e4
  );
  UNKNOWN_TERRAIN_COLOR = $ff00ff;

  GOLD_COLOR  = $00c7ff;
  STONE_COLOR = $919191;
  CLIFF_COLOR = $334b71;
  RELIC_COLOR = $ffffff;
  FOOD_COLOR  = $6cc4a5;

  RESOURCES: array[TResourceId] of String = (
    'Food', 'Wood', 'Stone', 'Gold'
  );

  RESEARCHES_NUM = 139;
  RESEARCHES: array[0..RESEARCHES_NUM - 1] of TResearchRec = (
    // town center
    (Id: riFeudalAge;           Name: 'Feudal Age';            ResName: 'feudal_age'),
    (Id: riCastleAge;           Name: 'Castle Age';            ResName: 'castle_age'),
    (Id: riImperialAge;         Name: 'Imperial Age';          ResName: 'imperial_age'),
    (Id: riLoom;                Name: 'Loom';                  ResName: 'loom'),
    (Id: riWheelBarrow;         Name: 'Wheel Barrow';          ResName: 'wheel_barrow'),
    (Id: riHandCart;            Name: 'Hand Cart';             ResName: 'hand_cart'),
    (Id: riTownWatch;           Name: 'Town Watch';            ResName: 'town_watch'),
    (Id: riTownPatrol;          Name: 'Town Patrol';           ResName: 'town_patrol'),
    // mill
    (Id: riHorseCollar;         Name: 'Horse Collar';          ResName: 'horse_collar'),
    (Id: riHeavyPlow;           Name: 'Heavy Plow';            ResName: 'heavy_plow'),
    (Id: riCropRotation;        Name: 'Crop Rotation';         ResName: 'crop_rotation'),
    // lumber camp
    (Id: riDoubleBitAxe;        Name: 'Double-Bit Axe';        ResName: 'double_bit_axe'),
    (Id: riBowSaw;              Name: 'Bow Saw';               ResName: 'bow_saw'),
    (Id: riTwoManSaw;           Name: 'Two-Man Saw';           ResName: 'two_man_saw'),
    // mining camp
    (Id: riStoneMining;         Name: 'Stone Mining';          ResName: 'stone_mining'),
    (Id: riGoldMining;          Name: 'Gold Mining';           ResName: 'gold_mining'),
    (Id: riStoneShaftMining;    Name: 'Stone Shaft Mining';    ResName: 'stone_shaft_mining'),
    (Id: riGoldShaftMining;     Name: 'Gold Shaft Mining';     ResName: 'gold_shaft_mining'),
    // blacksmith
    (Id: riPaddedArcherArmor;   Name: 'Padded Archer Armor';   ResName: 'padded_archer_armor'),
    (Id: riLeatherArcherArmor;  Name: 'Leather Archer Armor';  ResName: 'leather_archer_armor'),
    (Id: riRingArcherArmor;     Name: 'Ring Archer Armor';     ResName: 'ring_archer_armor'),
    (Id: riFletching;           Name: 'Fletching';             ResName: 'fletching'),
    (Id: riBodkinArrow;         Name: 'Bodkin Arrow';          ResName: 'bodkin_arrow'),
    (Id: riBracer;              Name: 'Bracer';                ResName: 'bracer'),
    (Id: riForging;             Name: 'Forging';               ResName: 'forging'),
    (Id: riIronCasting;         Name: 'Iron Casting';          ResName: 'iron_casting'),
    (Id: riBlastFurnace;        Name: 'Blast Furnace';         ResName: 'blast_furnace'),
    (Id: riScaleBardingArmor;   Name: 'Scale Barding Armor';   ResName: 'scale_barding'),
    (Id: riChainBardingArmor;   Name: 'Chain Barding Armor';   ResName: 'chain_barding'),
    (Id: riPlateBardingArmor;   Name: 'Plate Barding Armor';   ResName: 'plate_barding'),
    (Id: riScaleMailArmor;      Name: 'Scale Mail Armor';      ResName: 'scale_mail'),
    (Id: riChainMailArmor;      Name: 'Chain Mail Armor';      ResName: 'chain_mail'),
    (Id: riPlateMailArmor;      Name: 'Plate Mail Armor';      ResName: 'plate_mail'),
    // university
    (Id: riMasonry;             Name: 'Masonry';               ResName: 'masonry'),
    (Id: riFortifiedWall;       Name: 'Fortified Wall';        ResName: 'fortified_wall'),
    (Id: riBallistics;          Name: 'Ballistics';            ResName: 'ballistics'),
    (Id: riGuardTower;          Name: 'Guard Tower';           ResName: 'guard_tower'),
    (Id: riHeatedShot;          Name: 'Heated Shot';           ResName: 'heated_shot'),
    (Id: riMurderHoles;         Name: 'Murder Holes';          ResName: 'murder_holes'),
    (Id: riTrademillCrane;      Name: 'Trademill Crane';       ResName: 'trademill_crane'),
    (Id: riArchitecture;        Name: 'Architecture';          ResName: 'architecture'),
    (Id: riChemistry;           Name: 'Chemistry';             ResName: 'chemistry'),
    (Id: riSiegeEngineers;      Name: 'Siege Engineers';       ResName: 'siege_engineers'),
    (Id: riKeep;                Name: 'Keep';                  ResName: 'keep'),
    (Id: riBombardTower;        Name: 'Bombard Tower';         ResName: 'bombard_tower'),
    // monastery
    (Id: riRedemption;          Name: 'Redemption';            ResName: 'redemption'),
    (Id: riFervor;              Name: 'Fervor';                ResName: 'fervor'),
    (Id: riSanctity;            Name: 'Sanctity';              ResName: 'sanctity'),
    (Id: riAtonement;           Name: 'Atonement';             ResName: 'atonement'),
    (Id: riHerbalMedicine;      Name: 'Herbal Medicine';       ResName: 'herbal_medicine'),
    (Id: riHeresy;              Name: 'Heresy';                ResName: 'heresy'),
    (Id: riBlockPrinting;       Name: 'Block Printing';        ResName: 'block_printing'),
    (Id: riIllumination;        Name: 'Illumination';          ResName: 'illumination'),
    (Id: riFaith;               Name: 'Faith';                 ResName: 'faith'),
    (Id: riTheocracy;           Name: 'Theocracy';             ResName: 'theocracy'),
    // market
    (Id: riCartography;         Name: 'Cartography';           ResName: 'cartography'),
    (Id: riCaravan;             Name: 'Caravan';               ResName: 'caravan'),
    (Id: riGuilds;              Name: 'Guilds';                ResName: 'guilds'),
    (Id: riCoinage;             Name: 'Coinage';               ResName: 'coinage'),
    (Id: riBanking;             Name: 'Banking';               ResName: 'banking'),
    // castle
    (Id: riHoardings;           Name: 'Hoardings';             ResName: 'hoardings'),
    (Id: riSappers;             Name: 'Sappers';               ResName: 'sappers'),
    (Id: riConscription;        Name: 'Conscription';          ResName: 'conscription'),
    (Id: riSpiesTreason;        Name: 'Spies / Treason';       ResName: 'spy'),
    // barrack
    (Id: riManAtArms;           Name: 'Man-at-Arms';           ResName: 'man_at_arms'),
    (Id: riLongSwordsman;       Name: 'Long Swordsman';        ResName: 'long_swordsman'),
    (Id: riTwoHandedSwordsman;  Name: 'Two-Handed Swordsman';  ResName: 'two_handed_swordsman'),
    (Id: riChampion;            Name: 'Champion';              ResName: 'champion'),
    (Id: riPikeman;             Name: 'Pikeman';               ResName: 'pikeman'),
    (Id: riHalberdier;          Name: 'Halberdier';            ResName: 'halberdier'),
    (Id: riEliteEagleWarrior;   Name: 'Elite Eagle Warrior';   ResName: 'eagle_warrior'),
    (Id: riTracking;            Name: 'Tracking';              ResName: 'tracking'),
    (Id: riSquires;             Name: 'Squires';               ResName: 'squires'),
    // archery range
    (Id: riCrossbow;            Name: 'Crossbow';              ResName: 'crossbow'),
    (Id: riArbalest;            Name: 'Arbalest';              ResName: 'arbalest'),
    (Id: riEliteSkirmisher;     Name: 'Elite Skirmisher';      ResName: 'elite_skirmisher'),
    (Id: riHeavyCavalryArcher;  Name: 'Heavy Cavalry Archer';  ResName: 'heavy_cavalry_archer'),
    (Id: riThumbRing;           Name: 'Thumb Ring';            ResName: 'thumb_ring'),
    (Id: riParthianTactics;     Name: 'Parthian Tactics';      ResName: 'parthian_tactics'),
    // stable
    (Id: riLightCavalry;        Name: 'Light Cavalry';         ResName: 'light_cavalry'),
    (Id: riHussar;              Name: 'Hussar';                ResName: 'hussar'),
    (Id: riCavalier;            Name: 'Cavalier';              ResName: 'cavalier'),
    (Id: riPaladin;             Name: 'Paladin';               ResName: 'paladin'),
    (Id: riHeavyCamel;          Name: 'Heavy Camel';           ResName: 'heavy_camel'),
    (Id: riBloodlines;          Name: 'Bloodlines';            ResName: 'bloodlines'),
    (Id: riHusbandry;           Name: 'Husbandry';             ResName: 'husbandry'),
    // siege workshop
    (Id: riOnager;              Name: 'Onager';                ResName: 'onager'),
    (Id: riSiegeOnager;         Name: 'Siege Onager';          ResName: 'siege_onager'),
    (Id: riCappedRam;           Name: 'Capped Ram';            ResName: 'capped_ram'),
    (Id: riSiegeRam;            Name: 'Siege Ram';             ResName: 'siege_ram'),
    (Id: riHeavyScorpion;       Name: 'Heavy Scorpion';        ResName: 'heavy_scorpion'),
    // dock
    (Id: riWarGalley;           Name: 'War Galley';            ResName: 'war_galley'),
    (Id: riGalleon;             Name: 'Galleon';               ResName: 'galleon'),
    (Id: riFastFireShip;        Name: 'Fast Fire Ship';        ResName: 'fast_fire_ship'),
    (Id: riHeavyDemolitionShip; Name: 'Heavy Demolition Ship'; ResName: 'heavy_demolition_ship'),
    (Id: riCannonGalleon;       Name: 'Cannon Galleon';        ResName: 'cannon_galleon'),
    (Id: riEliteCannonGalleon;  Name: 'Elite Cannon Galleon';  ResName: 'cannon_galleon'),
    (Id: riCareening;           Name: 'Careening';             ResName: 'careening'),
    (Id: riDryDock;             Name: 'Dry Dock';              ResName: 'dry_dock'),
    (Id: riShipwright;          Name: 'Shipwright';            ResName: 'shipwright'),
    // unique-unit-upgrade
    (Id: riEliteJaguarMan;      Name: 'Elite Jaguar Man';      ResName: 'jaguar_man'),
    (Id: riEliteCataphract;     Name: 'Elite Cataphract';      ResName: 'cataphract'),
    (Id: riEliteWoadRaider;     Name: 'Elite Woad Raider';     ResName: 'woad_raider'),
    (Id: riEliteChuKoNu;        Name: 'Elite Chu-Ko-Nu';       ResName: 'chu_ko_nu'),
    (Id: riEliteLongbowman;     Name: 'Elite Longbowman';      ResName: 'longbowman'),
    (Id: riEliteThrowingAxeman; Name: 'Elite Throwing Axeman'; ResName: 'throwing_axeman'),
    (Id: riEliteHuskarl;        Name: 'Elite Huskarl';         ResName: 'huskarl'),
    (Id: riEliteTarkan;         Name: 'Elite Tarkan';          ResName: 'tarkan'),
    (Id: riEliteSamurai;        Name: 'Elite Samurai';         ResName: 'samurai'),
    (Id: riEliteWarWagon;       Name: 'Elite War Wagon';       ResName: 'war_wagon'),
    (Id: riEliteTurtleShip;     Name: 'Elite Turtle Ship';     ResName: 'turtle_ship'),
    (Id: riElitePlumedArcher;   Name: 'Elite Plumed Archer';   ResName: 'plumed_archer'),
    (Id: riEliteMangudai;       Name: 'Elite Mangudai';        ResName: 'mangudai'),
    (Id: riEliteWarElephant;    Name: 'Elite War Elephant';    ResName: 'war_elephant'),
    (Id: riEliteMameluke;       Name: 'Elite Mameluke';        ResName: 'mameluke'),
    (Id: riEliteConquistador;   Name: 'Elite Conquistador';    ResName: 'conquistador'),
    (Id: riEliteTeutonicKnight; Name: 'Elite Teutonic Knight'; ResName: 'teutonic_knight'),
    (Id: riEliteJanissary;      Name: 'Elite Janissary';       ResName: 'janissary'),
    (Id: riEliteBerserk;        Name: 'Elite Berserk';         ResName: 'berserk'),
    (Id: riEliteLongboat;       Name: 'Elite Longboat';        ResName: 'longboat'),
    // unique-research
    (Id: riGarlandWars;         Name: 'Garland Wars';          ResName: 'unique_tech'),
    (Id: riLogistica;           Name: 'Logistica';             ResName: 'unique_tech'),
    (Id: riFurorCeltica;        Name: 'Furor Celtica';         ResName: 'unique_tech'),
    (Id: riRocketry;            Name: 'Rocketry';              ResName: 'unique_tech'),
    (Id: riYeomen;              Name: 'Yeomen';                ResName: 'unique_tech'),
    (Id: riBeardedAxe;          Name: 'Bearded Axe';           ResName: 'unique_tech'),
    (Id: riAnarchy;             Name: 'Anarchy';               ResName: 'unique_tech'),
    (Id: riPerfusion;           Name: 'Perfusion';             ResName: 'unique_tech'),
    (Id: riAtheism;             Name: 'Atheism';               ResName: 'unique_tech'),
    (Id: riKataparuto;          Name: 'Kataparuto';            ResName: 'unique_tech'),
    (Id: riShinkichon;          Name: 'Shinkichon';            ResName: 'unique_tech'),
    (Id: riElDorado;            Name: 'El Dorado';             ResName: 'unique_tech'),
    (Id: riDrill;               Name: 'Drill';                 ResName: 'unique_tech'),
    (Id: riMahouts;             Name: 'Mahouts';               ResName: 'unique_tech'),
    (Id: riZealotry;            Name: 'Zealotry';              ResName: 'unique_tech'),
    (Id: riSupremacy;           Name: 'Supremacy';             ResName: 'unique_tech'),
    (Id: riCrenellations;       Name: 'Crenellations';         ResName: 'unique_tech'),
    (Id: riArtillery;           Name: 'Artillery';             ResName: 'unique_tech'),
    (Id: riBerserkergang;       Name: 'Berserkergang';         ResName: 'unique_tech'));

  UNITS_NUM = 96;
  UNITS: array[0..UNITS_NUM - 1] of TUnitRec = (
    (Id: uiArcher;              Name: 'Archer';                ResName: 'archer'),
    (Id: uiHandCannoneer;       Name: 'Hand Cannoneer';        ResName: 'hand_cannoneer'),
    (Id: uiEliteSkirmisher;     Name: 'Elite Skirmisher';      ResName: 'elite_skirmisher'),
    (Id: uiSkirmisher;          Name: 'Skirmisher';            ResName: 'skirmisher'),
    (Id: uiLongbowman;          Name: 'Longbowman';            ResName: 'longbowman'),
    (Id: uiMangudai;            Name: 'Mangudai';              ResName: 'mangudai'),
    (Id: uiFishingShip;         Name: 'Fishing Ship';          ResName: 'fishing_ship'),
    (Id: uiTradeCog;            Name: 'Trade Cog';             ResName: 'trade_cog'),
    (Id: uiWarGalley;           Name: 'War Galley';            ResName: 'war_galley'),
    (Id: uiCrossbowman;         Name: 'Crossbowman';           ResName: 'crossbowman'),
    (Id: uiTeutonicKnight;      Name: 'Teutonic Knight';       ResName: 'teutonic_knight'),
    (Id: uiBatteringRam;        Name: 'Battering Ram';         ResName: 'battering_ram'),
    (Id: uiBombardCannon;       Name: 'Bombard Cannon';        ResName: 'bombard_cannon'),
    (Id: uiKnight;              Name: 'Knight';                ResName: 'knight'),
    (Id: uiCavalryArcher;       Name: 'Cavalry Archer';        ResName: 'cavalry_archer'),
    (Id: uiCataphract;          Name: 'Cataphract';            ResName: 'cataphract'),
    (Id: uiHuskarl;             Name: 'Huskarl';               ResName: 'huskarl'),
//    (Id: uiTrebuchetUnpacked;   Name: 'Trebuchet (Unpacked)';  ResName: 'trebuchet'),
    (Id: uiJanissary;           Name: 'Janissary';             ResName: 'janissary'),
    (Id: uiChuKoNu;             Name: 'Chu Ko Nu';             ResName: 'chu_ko_nu'),
    (Id: uiMilitia;             Name: 'Militia';               ResName: 'militiaman'),
    (Id: uiManAtArms;           Name: 'Man At Arms';           ResName: 'man_at_arms'),
    (Id: uiHeavySwordsman;      Name: 'Heavy Swordsman';       ResName: 'heavy_swordsman'),
    (Id: uiLongSwordsman;       Name: 'Long Swordsman';        ResName: 'long_swordsman'),
    (Id: uiVillager;            Name: 'Villager';              ResName: 'villager'),
    (Id: uiSpearman;            Name: 'Spearman';              ResName: 'spearman'),
    (Id: uiMonk;                Name: 'Monk';                  ResName: 'monk'),
//    (Id: uiTradeCartEmpty;      Name: 'Trade Cart, Empty';     ResName: 'trade_cart'),
    (Id: uiTradeCart;           Name: 'Trade Cart';            ResName: 'trade_cart'),
//    (Id: uiTradeCartFull;       Name: 'Trade Cart, Full';      ResName: 'trade_cart'),
    (Id: uiWoadRaider;          Name: 'Woad Raider';           ResName: 'woad_raider'),
    (Id: uiWarElephant;         Name: 'War Elephant';          ResName: 'war_elephant'),
    (Id: uiLongboat;            Name: 'Longboat';              ResName: 'longboat'),
    (Id: uiScorpion;            Name: 'Scorpion';              ResName: 'scorpion'),
    (Id: uiMangonel;            Name: 'Mangonel';              ResName: 'mangonel'),
    (Id: uiThrowingAxeman;      Name: 'Throwing Axeman';       ResName: 'throwing_axeman'),
    (Id: uiMameluke;            Name: 'Mameluke';              ResName: 'mameluke'),
    (Id: uiCavalier;            Name: 'Cavalier';              ResName: 'cavalier'),
//    (Id: uiMonkWithRelic;       Name: 'Monk With Relic';       ResName: 'monk'),
    (Id: uiSamurai;             Name: 'Samurai';               ResName: 'samurai'),
    (Id: uiCamel;               Name: 'Camel';                 ResName: 'camel'),
    (Id: uiHeavyCamel;          Name: 'Heavy Camel';           ResName: 'heavy_camel'),
//    (Id: uiTrebuchetPacked;     Name: 'Trebuchet, P';          ResName: 'trebuchet'),
    (Id: uiTrebuchet;           Name: 'Trebuchet';             ResName: 'trebuchet'),
    (Id: uiPikeman;             Name: 'Pikeman';               ResName: 'pikeman'),
    (Id: uiHalberdier;          Name: 'Halberdier';            ResName: 'halberdier'),
    (Id: uiCannonGalleon;       Name: 'Cannon Galleon';        ResName: 'cannon_galleon'),
    (Id: uiCappedRam;           Name: 'Capped Ram';            ResName: 'capped_ram'),
    (Id: uiKing;                Name: 'King';                  ResName: 'king'),
    (Id: uiPetard;              Name: 'Petard';                ResName: 'petard'),
    (Id: uiHussar;              Name: 'Hussar';                ResName: 'hussar'),
    (Id: uiGalleon;             Name: 'Galleon';               ResName: 'galleon'),
    (Id: uiScoutCavalry;        Name: 'Scout Cavalry';         ResName: 'scout_cavalry'),
    (Id: uiTwoHandedSwordsman;  Name: 'Two Handed Swordsman';  ResName: 'two_handed_swordsman'),
    (Id: uiHeavyCavalryArcher;  Name: 'Heavy Cavalry Archer';  ResName: 'heavy_cavalry_archer'),
    (Id: uiArbalest;            Name: 'Arbalest';              ResName: 'arbalest'),
//    (Id: uiAdvHeavyCrossbowman; Name: 'Adv Heavy Crossbowman'; ResName: ''),
    (Id: uiDemolitionShip;      Name: 'Demolition Ship';       ResName: 'demolition_ship'),
    (Id: uiHeavyDemolitionShip; Name: 'Heavy Demolition Ship'; ResName: 'heavy_demolition_ship'),
    (Id: uiFireShip;            Name: 'Fire Ship';             ResName: 'fire_ship'),
    (Id: uiEliteLongbowman;     Name: 'Elite Longbowman';      ResName: 'longbowman'),
    (Id: uiEliteThrowingAxeman; Name: 'Elite Throwing Axeman'; ResName: 'throwing_axeman'),
    (Id: uiFastFireShip;        Name: 'Fast Fire Ship';        ResName: 'fast_fire_ship'),
    (Id: uiEliteLongboat;       Name: 'Elite Longboat';        ResName: 'longboat'),
    (Id: uiEliteWoadRaider;     Name: 'Elite Woad Raider';     ResName: 'woad_raider'),
    (Id: uiGalley;              Name: 'Galley';                ResName: 'galley'),
    (Id: uiHeavyScorpion;       Name: 'Heavy Scorpion';        ResName: 'heavy_scorpion'),
    (Id: uiTransportShip;       Name: 'Transport Ship';        ResName: 'transport_ship'),
    (Id: uiLightCavalry;        Name: 'Light Cavalry';         ResName: 'light_cavalry'),
    (Id: uiSiegeRam;            Name: 'Siege Ram';             ResName: 'siege_ram'),
    (Id: uiOnager;              Name: 'Onager';                ResName: 'onager'),
    (Id: uiEliteCataphract;     Name: 'Elite Cataphract';      ResName: 'cataphract'),
    (Id: uiEliteTeutonicKnight; Name: 'Elite Teutonic Knight'; ResName: 'teutonic_knight'),
    (Id: uiEliteHuskarl;        Name: 'Elite Huskarl';         ResName: 'huskarl'),
    (Id: uiEliteMameluke;       Name: 'Elite Mameluke';        ResName: 'mameluke'),
    (Id: uiEliteJanissary;      Name: 'Elite Janissary';       ResName: 'janissary'),
    (Id: uiEliteWarElephant;    Name: 'Elite War Elephant';    ResName: 'war_elephant'),
    (Id: uiEliteChuKoNu;        Name: 'Elite Chu Ko Nu';       ResName: 'chu_ko_nu'),
    (Id: uiEliteSamurai;        Name: 'Elite Samurai';         ResName: 'samurai'),
    (Id: uiEliteMangudai;       Name: 'Elite Mangudai';        ResName: 'mangudai'),
    (Id: uiChampion;            Name: 'Champion';              ResName: 'champion'),
    (Id: uiPaladin;             Name: 'Paladin';               ResName: 'paladin'),
    (Id: uiSiegeOnager;         Name: 'Siege Onager';          ResName: 'siege_onager'),
    (Id: uiBerserk;             Name: 'Berserk';               ResName: 'berserk'),
    (Id: uiEliteBerserk;        Name: 'Elite Berserk';         ResName: 'berserk'),
    (Id: uiJaguarWarrior;       Name: 'Jaguar Warrior';        ResName: 'jaguar_man'),
    (Id: uiEliteJaguarWarrior;  Name: 'Elite Jaguar Warrior';  ResName: 'jaguar_man'),
//    (Id: uiCobraCar;            Name: 'Cobra Car';             ResName: ''),
    (Id: uiEagleWarrior;        Name: 'Eagle Warrior';         ResName: 'eagle_warrior'),
    (Id: uiEliteEagleWarrior;   Name: 'Elite Eagle Warrior';   ResName: 'eagle_warrior'),
    (Id: uiTarkan;              Name: 'Tarkan';                ResName: 'tarkan'),
    (Id: uiEliteTarkan;         Name: 'Elite Tarkan';          ResName: 'tarkan'),
    (Id: uiHuskarl2;            Name: 'Huskarl';               ResName: 'huskarl'),
    (Id: uiEliteHuskarl2;       Name: 'Elite Huskarl';         ResName: 'huskarl'),
    (Id: uiPlumedArcher;        Name: 'Plumed Archer';         ResName: 'plumed_archer'),
    (Id: uiElitePlumedArcher;   Name: 'Elite Plumed Archer';   ResName: 'plumed_archer'),
    (Id: uiConquistador;        Name: 'Conquistador';          ResName: 'conquistador'),
    (Id: uiEliteConquistador;   Name: 'Elite Conquistador';    ResName: 'conquistador'),
    (Id: uiMissionary;          Name: 'Missionary';            ResName: 'missionary'),
//    (Id: uiJaguar;              Name: 'Jaguar';                ResName: ''),
    (Id: uiWarWagon;            Name: 'War Wagon';             ResName: 'war_wagon'),
    (Id: uiEliteWarWagon;       Name: 'Elite War Wagon';       ResName: 'war_wagon'),
    (Id: uiTurtleShip;          Name: 'Turtle Ship';           ResName: 'turtle_ship'),
    (Id: uiEliteTurtleShip;     Name: 'Elite Turtle Ship';     ResName: 'turtle_ship'));

  BUILDINGS_NUM = 31;
  BUILDINGS: array[0..BUILDINGS_NUM - 1] of TBuildingRec = (
    (Id: biBarracks;      Name: 'Barracks';        ResName: 'barracks'),
    (Id: biDock;          Name: 'Dock';            ResName: 'dock'),
    (Id: biSiegeWorkshop; Name: 'Siege Workshop';  ResName: 'siege_workshop'),
    (Id: biFarm;          Name: 'Farm';            ResName: 'farm'),
    (Id: biMill;          Name: 'Mill';            ResName: 'mill'),
    (Id: biHouse;         Name: 'House';           ResName: 'house'),
    (Id: biWallPalisade;  Name: 'Wall, Palisade';  ResName: 'palisade_wall'),
    (Id: biWatchTower;    Name: 'Watch Tower';     ResName: 'watch_tower'),
    (Id: biCastle;        Name: 'Castle';          ResName: 'castle'),
    (Id: biMarket;        Name: 'Market';          ResName: 'market'),
    (Id: biArcheryRange;  Name: 'Archery Range';   ResName: 'archery_range'),
    (Id: biStable;        Name: 'Stable';          ResName: 'stable'),
    (Id: biBlacksmith;    Name: 'Blacksmith';      ResName: 'blacksmith'),
    (Id: biMonastery;     Name: 'Monastery';       ResName: 'monastery'),
    (Id: biTownCenter;    Name: 'Town Center';     ResName: 'town_center'),
    (Id: biWallStone;     Name: 'Wall, Stone';     ResName: 'stone_wall'),
    (Id: biWallFortified; Name: 'Wall, Fortified'; ResName: 'fortified_wall'),
    (Id: biFishTrap;      Name: 'Fish Trap';       ResName: 'fish_trap'),
    (Id: biUniversity;    Name: 'University';      ResName: 'university'),
    (Id: biGuardTower;    Name: 'Guard Tower';     ResName: 'guard_tower'),
    (Id: biKeep;          Name: 'Keep';            ResName: 'keep'),
    (Id: biBombardTower;  Name: 'Bombard Tower';   ResName: 'bombard_tower'),
    (Id: biWonder;        Name: 'Wonder';          ResName: 'wonder'),
    (Id: biGate;          Name: 'Gate';            ResName: 'gate'),
    (Id: biGate2;         Name: 'Gate';            ResName: 'gate'),
    (Id: biLumberCamp;    Name: 'Lumber Camp';     ResName: 'lumber_camp'),
    (Id: biMiningCamp;    Name: 'Mining Camp';     ResName: 'mining_camp'),
    (Id: biOutpost;       Name: 'Outpost';         ResName: 'outpost'),
    (Id: biTownCenter2;   Name: 'Town Center';     ResName: 'town_center'),
    (Id: biGate3;         Name: 'Gate';            ResName: 'gate'),
    (Id: biGate4;         Name: 'Gate';            ResName: 'gate')
  );

{ localized map names (translations) }

  LANGUAGES_NUM = 5;
  LOC_MAP_NAMES: array[0..LANGUAGES_NUM - 1] of array[0..MAPS_NUM - 1] of String = (
    // english
    ('Arabia', 'Archipelago', 'Baltic', 'Black Forest', 'Coastal', 'Continental',
     'Crater Lake', 'Fortress', 'Gold Rush', 'Highland', 'Islands', 'Mediterranean',
     'Migration', 'Rivers', 'Team Islands', 'Random', 'Scandinavia', 'Mongolia',
     'Yucatan', 'Salt Marsh', 'Arena', 'King of the Hill', 'Oasis', 'Ghost Lake',
     'Nomad', 'Iberia', 'Britain', 'Mideast', 'Texas', 'Italy', 'Central America',
     'France', 'Norse Lands', 'Sea of Japan (East Sea)', 'Byzantinum', 'Custom', 'Blind Random'),

    // spanish (european) provided by dauro ibero
    ('Arabia', 'ArchipiÈlago', 'B·ltico', 'Selva negra', 'Costa', 'Continental',
     'Lago de cr·ter', 'Fortaleza', 'Fiebre del oro', 'Montanas', 'Islas', 'Mediterr·neo',
     'MigraciÛn', 'RÌos', 'Islas de equipo', 'Aleatorio', 'Escandinavia', 'Mongolia',
     'Yucat·n', 'Marisma', 'Arena', 'Rey de la colina', 'Oasis', 'Lago fantasma',
     'NÛmada', 'Iberia', 'Inglaterra', 'Oriente Medio', 'Texas', 'Italia', 'AmÈrica Central',
     'Francia', 'Tierras NÛrdicas', 'Mar del JapÛn', 'Bizancio', 'Personalizado', 'Aleatorio a ciegas'),

    // czech from aoe.cz (missing Blind Random)
    ('Arabie', 'Clenity Teren', 'Primori', 'Cerny Les', 'Pobrezi', 'Kontinent',
     'Jezero', 'Les', 'Zlata Horecka', 'Pohori', 'Ostrovy', 'Stredomori',
     'Stehovani', 'Reky', 'Spojene Ostrovy', 'Nahodne', 'Scandinavie', 'Mongolsko',
     'Yucatan', 'Salt Marsh', 'Arena', 'Kr·l Vrchu', 'Oasa', 'Jezero duchu',
     'KoËovnÌk', 'Iberia', 'Anglie', 'Mideast', 'Texas', 'Italie', 'St¯ednÌ Amerika',
     'Francie', 'SeverskÈ zemÏ', 'JaponskÈ mo¯e (V˝chodnÌ mo¯e)', 'Byzantium', 'Voliteln˝', ''),

    // polish from empires2.net (missing Blind Random)
    ('Pustynia', 'Archipelag', 'Ba≥tyk', 'Czarny las', 'Wybrzeøe', 'Kontynent',
     'Krater-Jezioro', 'Zamek', 'Gorπczka Z≥ota', 'Szczyt', 'Wyspy', 'Morze årÛdziemne',
     'Migracja', 'Rzeki', 'Wielkie wyspy', 'Losowa', 'Skandynawia', 'Mongolia',
     'Jukatan', 'S≥one bagna', 'Arena', 'KrÛl GÛry', 'Oaza', 'Jezioro z widmami',
     'Koczownik', 'Iberia', 'Brytania', 'Bliski WschÛd', 'Teksas', 'W≥ochy', 'årodkowa Ameryka',
     'Francja', 'Ziemie PÛ≥nocne', 'Morze JapoÒskie (Morze Wschodnie)', 'Bizancjum', 'W≥asny', ''),

    // slovak from slovenciny.com provided by Maximus
    ('Ar·bia', 'S˙ostrovie', 'More', 'Temn˝ les', 'Pobreûie', 'Kontinent',
     'Jazero', 'Pevnosti', 'Zlat· hor˙Ëka', 'Pohorie', 'Ostrovy', 'Stredomorie',
     'Sùahovanie', 'Rieky', 'SpojeneckÈ ostrovy', 'N·hodn·', 'äkandin·via', 'Mongolsko',
     'Yucatan', 'Solisko', 'ArÈna', 'Kr·æ vrchu', 'O·za', 'Jazero duchov',
     'KoËovanie', 'Pyreneje', 'Brit·nia', 'Stredn˝ v˝chod', 'Texas', 'Taliansko', 'Stredn· Amerika',
     'Franc˙zsko', 'äkandin·via', 'JaponskÈ more', 'Byzancia', 'Voliteæn˝', '⁄plne n·hodn·')
    );

implementation

end.
