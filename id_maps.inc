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
{ map ids }
const
  miArabia         = 9;
  miArchipelago    = 10;
  miBaltic         = 11;
  miBlackForest    = 12;
  miCoastal        = 13;
  miContinental    = 14;
  miCraterLake     = 15;
  miFortress       = 16;
  miGoldRush       = 17;
  miHighland       = 18;
  miIslands        = 19;
  miMediterranean  = 20;
  miMigration      = 21;
  miRivers         = 22;
  miTeamIslands    = 23;
  miRandom         = 24;
  miScandinavia    = 25;
  miMongolia       = 26;
  miYucatan        = 27;
  miSaltMarsh      = 28;
  miArena          = 29;
  miKingOfTheHill  = 30;
  miOasis          = 31;
  miGhostLake      = 32;
  miNomad          = 33;
  miIberia         = 34;
  miBritain        = 35;
  miMideast        = 36;
  miTexas          = 37;
  miItaly          = 38;
  miCentralAmerica = 39;
  miFrance         = 40;
  miNorseLands     = 41;
  miSeaOfJapan     = 42;
  miByzantinum     = 43;
  miCustom         = 44;
  miBlindRandom    = 48;

  RealWorldMapSet: set of Byte = [miIberia..miByzantinum];
// zobrazuje sa random land map? otestovat v hre
