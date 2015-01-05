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
{ research ids }
const
  // town center
  riFeudalAge           = 101;
  riCastleAge           = 102;
  riImperialAge         = 103;
  riLoom                = 22;
  riWheelBarrow         = 213;
  riHandCart            = 249;
  riTownWatch           = 8;
  riTownPatrol          = 280;
  // mill
  riHorseCollar         = 14;
  riHeavyPlow           = 13;
  riCropRotation        = 12;
  // lumber camp
  riDoubleBitAxe        = 202;
  riBowSaw              = 203;
  riTwoManSaw           = 221;
  // mining camp
  riStoneMining         = 278;
  riGoldMining          = 55;
  riStoneShaftMining    = 279;
  riGoldShaftMining     = 182;
  // blacksmith
  riPaddedArcherArmor   = 211;
  riLeatherArcherArmor  = 212;
  riRingArcherArmor     = 219;
  riFletching           = 199;
  riBodkinArrow         = 200;
  riBracer              = 201;
  riForging             = 67;
  riIronCasting         = 68;
  riBlastFurnace        = 75;
  riScaleBardingArmor   = 81;
  riChainBardingArmor   = 82;
  riPlateBardingArmor   = 80;
  riScaleMailArmor      = 74;
  riChainMailArmor      = 76;
  riPlateMailArmor      = 77;
  // university
  riMasonry             = 50;
  riFortifiedWall       = 194;
  riBallistics          = 93;
  riGuardTower          = 140;
  riHeatedShot          = 380;
  riMurderHoles         = 322;
  riTreadmillCrane      = 54;
  riArchitecture        = 51;
  riChemistry           = 47;
  riSiegeEngineers      = 377;
  riKeep                = 63;
  riBombardTower        = 64;
  // monastery
  riRedemption          = 316;
  riFervor              = 252;
  riSanctity            = 231;
  riAtonement           = 319;
  riHerbalMedicine      = 441;
  riHeresy              = 439;
  riBlockPrinting       = 230;
  riIllumination        = 233;
  riFaith               = 45;
  riTheocracy           = 438;
  // market
  riCartography         = 19;
  riCaravan             = 48;
  riGuilds              = 15;
  riCoinage             = 23;
  riBanking             = 17;
  // castle
  riHoardings           = 379;
  riSappers             = 321;
  riConscription        = 315;
  riSpiesTreason        = 408;
  // barrack
  riManAtArms           = 222;
  riLongSwordsman       = 207;
  riTwoHandedSwordsman  = 217;
  riChampion            = 264;
  riPikeman             = 197;
  riHalberdier          = 429;
  riEliteEagleWarrior   = 434;
  riTracking            = 90;
  riSquires             = 215;
  // archery range
  riCrossbow            = 100;
  riArbalest            = 237;
  riEliteSkirmisher     = 98;
  riHeavyCavalryArcher  = 218;
  riThumbRing           = 437;
  riParthianTactics     = 436;
  // stable
  riLightCavalry        = 254;
  riHussar              = 428;
  riCavalier            = 209;
  riPaladin             = 265;
  riHeavyCamel          = 236;
  riBloodlines          = 435;
  riHusbandry           = 39;
  // siege workshop
  riOnager              = 257;
  riSiegeOnager         = 320;
  riCappedRam           = 96;
  riSiegeRam            = 255;
  riHeavyScorpion       = 239;
  // dock
  riWarGalley           = 34;
  riGalleon             = 35;
  riFastFireShip        = 246;
  riHeavyDemolitionShip = 244;
  riCannonGalleon       = 37;
//  riDeckGuns            = 376;
  riEliteCannonGalleon  = 376;
  riCareening           = 374;
  riDryDock             = 375;
  riShipwright          = 373;
  // unique-unit-upgrade
  riEliteJaguarMan      = 432;
  riEliteCataphract     = 361;
  riEliteWoadRaider     = 370;
  riEliteChuKoNu        = 362;
  riEliteLongbowman     = 360;
  riEliteThrowingAxeman = 363;
  riEliteHuskarl        = 365;
  riEliteTarkan         = 2;
  riEliteSamurai        = 366;
  riEliteWarWagon       = 450;
  riEliteTurtleShip     = 448;
//  riEliteTurtleShip     = 348;
  riElitePlumedArcher   = 27;
  riEliteMangudai       = 371;
  riEliteWarElephant    = 367;
  riEliteMameluke       = 368;
//  riElite Mameluke      = 378;
  riEliteConquistador   = 60;
  riEliteTeutonicKnight = 364;
  riEliteJanissary      = 369;
  riEliteBerserk        = 398;
  riEliteLongboat       = 372;
  // unique-research
  riGarlandWars         = 24;
  riLogistica           = 61;
  riFurorCeltica        = 5;
  riRocketry            = 52;
  riYeomen              = 3;
  riBeardedAxe          = 83;
  riAnarchy             = 16;
  riPerfusion           = 457;
  riAtheism             = 21;
  riKataparuto          = 59;
  riShinkichon          = 445;
  riElDorado            = 4;
  riDrill               = 6;
  riMahouts             = 7;
  riZealotry            = 9;
  riSupremacy           = 440;
  riCrenellations       = 11;
  riArtillery           = 10;
  riBerserkergang       = 49;
  // AoFE
  riHuntingDogs         = 526;
  riImperialCamel       = 521;
  riCouriers            = 517;
  riAndeanSling         = 516;
  riRecurveBow          = 515;
  riMercenaries         = 514;
  riDruzhina            = 513;
  riOrthodoxy           = 512;
  riShatagni            = 507;
  riSultans             = 506;
  riSilkRoad            = 499;
  riPavise              = 494;
  riChivalry            = 493;
  riInquisition         = 492;
  riSipahi              = 491;
  riMadrasah            = 490;
  riIronclad            = 489;
  riBoilingOil          = 488;
  riNomads              = 487;
  riPanokseon           = 486;
  riTlatoani            = 485;
  riMarauders           = 484;
  riStronghold          = 483;
  riGreekFire           = 464;
  riChieftains          = 463;
  riGreatWall           = 462;
  riWarwolf             = 461;
  riAtlatl              = 460;
  riEagleWarrior        = 384;
  riGillnets            = 494;
  riEliteKamayuk        = 509;
  riEliteBoyar          = 504;
  riEliteElephantArcher = 481;
  riEliteMagyarHuszar   = 472;
  riEliteGenoeseCrossbowman = 468;

