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
unit BitmapEx;

interface

uses
  Windows, Graphics;

type
  TBitmapEx = class(TBitmap)
  private
  public
    procedure Rotate(const Angle: Extended; bgColor: TColor);
    procedure Resize(const Width: Integer; const Height: Integer);
    procedure ResizeEx(const Width: Integer; const Height: Integer);
    procedure Resize2(const Width: Integer; const Height: Integer);
  end;

implementation

uses
  Math, Classes;

type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..0] of TRGBQuad;
  PLongIntArray = ^TLongIntArray;
  TLongIntArray = array[0..16383] of LongInt;
//  TRGBArray = ARRAY[0..32767] OF TRGBTriple;
//  pRGBArray = ^TRGBArray;

  TColorRec = packed record
  case Integer of
    0: (Value: Longint);
    1: (Red, Green, Blue: Byte);
    2: (R, G, B, Flag: Byte);
    {$IFDEF MSWINDOWS}
    3: (Index: Word); // GetSysColor, PaletteIndex
    {$ENDIF MSWINDOWS}
  end;

procedure StretchBitmap(const Source, Destination: TBitmap); forward;
procedure GetRGBValue (const Color: TColor; out Red, Green, Blue: Byte); forward;
function SetRGBValue(const Red, Green, Blue: Byte): TColor; forward;

procedure TBitmapEx.Rotate(const Angle: Extended; bgColor: TColor);
var
  bgRGB: TRGBQuad;
  NormalAngle: Extended;
  CosTheta, SinTheta: Extended;
  iCosTheta, iSinTheta: Integer;
  xSrc, ySrc: Integer;
  xDst, yDst: Integer;
  xODst, yODst: Integer;
  xOSrc, yOSrc: Integer;
  xPrime, yPrime: Integer;
  srcWidth, srcHeight: Integer;
  dstWidth, dstHeight: Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;
  srcRGBs: PRGBQuadArray;
  dstRGBs: PRGBQuadArray;
  dstRGB: PRGBQuad;
  BitmapInfo: TBitmapInfo;
  srcBMP, dstBMP: HBITMAP;
  DC: HDC;
  Bitmap: TBitmap;
begin
  { Converts bgColor to true RGB Color }
  bgColor := ColorToRGB (bgColor);
  with bgRGB do
  begin
    rgbRed := Byte (bgColor);
    rgbGreen := Byte (bgColor shr 8);
    rgbBlue := Byte (bgColor shr 16);
    rgbReserved := Byte (bgColor shr 24);
  end;

  { Calculates Sine and Cosine of the rotation angle }
  NormalAngle := Frac (Angle / 360.0) * 360.0;
  SinCos (Pi * -NormalAngle / 180, SinTheta, CosTheta);
  iSinTheta := Trunc (SinTheta * (1 shl 16));
  iCosTheta := Trunc (CosTheta * (1 shl 16));

  { Prepares the required data for the source bitmap }
  srcBMP := Self.Handle;
  srcWidth := Self.Width;
  srcHeight := Self.Height;
  xOSrc := srcWidth shr 1;
  yOSrc := srcHeight shr 1;

  { Prepares the required data for the target bitmap }
  dstWidth := SmallInt ((srcWidth * Abs (iCosTheta) + srcHeight * Abs (iSinTheta)) shr 16);
  dstHeight := SmallInt ((srcWidth * Abs (iSinTheta) + srcHeight * Abs (iCosTheta)) shr 16);
  xODst := dstWidth shr 1;
  if not Odd (dstWidth) and ((NormalAngle = 0.0) or (NormalAngle = -90.0)) then
    Dec (xODst);
  yODst := dstHeight shr 1;
  if not Odd (dstHeight) and ((NormalAngle = 0.0) or (NormalAngle = +90.0)) then
    Dec (yODst);

  { Initializes bitmap header }
  FillChar (BitmapInfo, SizeOf (BitmapInfo), 0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf (BitmapInfo.bmiHeader);
    biCompression := BI_RGB;
    biBitCount := 32;
    biPlanes := 1;
  end;

  { Get source and target RGB bits }
  DC := CreateCompatibleDC (0);
  try
    BitmapInfo.bmiHeader.biWidth := srcWidth;
    BitmapInfo.bmiHeader.biHeight := srcHeight;
    GetMem (srcRGBs, srcWidth * srcHeight * SizeOf (TRGBQuad));
    GdiFlush;
    GetDIBits (DC, srcBMP, 0, srcHeight, srcRGBS, BitmapInfo, DIB_RGB_COLORS);
    BitmapInfo.bmiHeader.biWidth := dstWidth;
    BitmapInfo.bmiHeader.biHeight := dstHeight;
    dstBMP := CreateDIBSection (DC, BitmapInfo, DIB_RGB_COLORS, Pointer (dstRGBs), 0, 0);
  finally
    DeleteDC (DC);
  end;

  { Perfoms rotation on RGB bits }
  dstRGB := @dstRGBs[(dstWidth * dstHeight) - 1];
  yPrime := yODst;
  for yDst := dstHeight - 1 downto 0 do
  begin
    yPrimeSinTheta := yPrime * iSinTheta;
    yPrimeCosTheta := yPrime * iCosTheta;
    xPrime := xODst;
    for xDst := dstWidth - 1 downto 0 do
    begin
      xSrc := SmallInt ((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + xOSrc;
      ySrc := SmallInt ((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + yOSrc;
      if (DWORD (ySrc) < DWORD (srcHeight)) and (DWORD (xSrc) < DWORD (srcWidth)) then
        dstRGB^ := srcRGBs[ySrc * srcWidth + xSrc]
      else
        dstRGB^ := bgRGB;
      Dec (dstRGB);
      Dec (xPrime);
    end;
    Dec (yPrime);
  end;

  { Releases memory for source bitmap RGB bits }
  FreeMem (srcRGBs);

  { Create result bitmap }
  Bitmap := TBitmap.Create;
  try
    Bitmap.Handle := dstBMP;
    Self.Assign (Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TBitmapEx.Resize(const Width: Integer; const Height: Integer);
var
  Bitmap: TBitmap;
  ARect: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    ARect := Rect (0, 0 , Width, Height);
    Bitmap.Canvas.StretchDraw (ARect, Self);
    Self.Assign (Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TBitmapEx.ResizeEx(const Width: Integer; const Height: Integer);
var
  Bitmap: TBitmap;
  pt: TPoint;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.PixelFormat := pf32bit;

    GetBrushOrgEx (Bitmap.Canvas.Handle, pt);
    SetStretchBltMode (Bitmap.Canvas.Handle, HALFTONE);
    SetBrushOrgEx (Bitmap.Canvas.Handle, pt.x, pt.y, @pt);
    StretchBlt (Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Self.Canvas.Handle, 0, 0, Self.Width, Self.Height, SRCCOPY);
    Self.Assign (Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TBitmapEx.Resize2(const Width: Integer; const Height: Integer);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Width;
    Bmp.Height := Height;
    StretchBitmap (Self, Bmp);
    Assign (Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure GetIndicies(const DestinationLength, SourceLength,
  DestinationIndex: Integer;
  out FirstIndex, LastIndex: Integer;
  out FirstFraction, LastFraction: Double);
{
This proceedure compares the length of two pixel arrays and determines
which pixels in the destination are covered by those in the source.
It also determines what fraction of the first and last pixels are covered
in the destination.
}
var
  Index1A: Double;
  Index2A: Double;
  Index2B: Integer;
begin
  Index1A := DestinationIndex / DestinationLength * SourceLength;
  FirstIndex := Trunc (Index1A);
  FirstFraction := 1 - Frac (Index1A);
  Index2A := (DestinationIndex + 1) / DestinationLength * SourceLength;
  Index2B := Trunc (Index2A);
  if Index2A = Index2B then
  begin
    LastIndex := Index2B - 1;
    LastFraction := 1;
  end else
  begin
    LastIndex := Index2B;
    LastFraction := Frac (Index2A);
  end;
  if FirstIndex = LastIndex then
  begin
    FirstFraction := FirstFraction - (1 - LastFraction);
    LastFraction := FirstFraction;
  end;
end;

procedure StretchBitmap(const Source, Destination: TBitmap);
{
This proceedure takes stretches the image in Source
and puts it in Destination.
The width and height of Destination must be specified
before calling StretchBitmap.
The PixelFormat of both Source and Destination are changed to pf32bit.
}
var
  P, P1: PLongIntArray;
  X, Y: Integer;
  FirstY, LastY, FirstX, LastX: Integer;
  FirstYFrac, LastYFrac, FirstXFrac, LastXFrac: Double;
  YFrac, XFrac: Double;
  YIndex, XIndex: Integer;
  AColor: TColor;
  Red, Green, Blue: Integer;
  RedTotal, GreenTotal, BlueTotal, FracTotal: Double;
begin
  Source.PixelFormat := pf32bit;
  Destination.PixelFormat := Source.PixelFormat;

  for Y := 0 to Destination.height - 1  do
  begin
    P := Destination.ScanLine[y];

    GetIndicies (Destination.Height, Source.Height, Y,
      FirstY, LastY, FirstYFrac, LastYFrac);

    for x := 0 to Destination.Width - 1 do
    begin

      GetIndicies (Destination.Width, Source.Width, X,
        FirstX, LastX, FirstXFrac, LastXFrac);

      RedTotal := 0;
      GreenTotal := 0;
      BlueTotal := 0;
      FracTotal := 0;

      for YIndex := FirstY to LastY do
      begin
        P1 := Source.ScanLine[YIndex];
        if YIndex = FirstY then
          YFrac := FirstYFrac
        else if YIndex = LastY then
          YFrac := LastYFrac
        else
          YFrac := 1;

        for XIndex := FirstX to LastX do
        begin
          AColor := P1[XIndex];
          Red := AColor mod $100;
          AColor := AColor div $100;
          Green := AColor mod $100;
          AColor := AColor div $100;
          Blue := AColor mod $100;

          if XIndex = FirstX then
            XFrac := FirstXFrac
          else if XIndex = LastX then
            XFrac := LastXFrac
          else
            XFrac := 1;

          RedTotal := RedTotal + Red * XFrac * YFrac;
          GreenTotal := GreenTotal + Green * XFrac * YFrac;
          BlueTotal := BlueTotal + Blue * XFrac * YFrac;
          FracTotal := FracTotal + XFrac * YFrac;
        end;
      end;

      Red := Round (RedTotal / FracTotal);
      Green := Round (GreenTotal / FracTotal);
      Blue := Round (BlueTotal / FracTotal);

      AColor := Blue * $10000 + Green * $100 + Red;

      P[X] := AColor;
    end;
  end;
end;

procedure GetRGBValue (const Color: TColor; out Red, Green, Blue: Byte);
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB (Color);
  Red := Temp.R;
  Green := Temp.G;
  Blue := Temp.B;
end;

function SetRGBValue(const Red, Green, Blue: Byte): TColor;
begin
  TColorRec(Result).Red := Red;
  TColorRec(Result).Green := Green;
  TColorRec(Result).Blue := Blue;
  TColorRec(Result).Flag := 0;
end;

end.
