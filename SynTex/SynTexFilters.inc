const
  FLT_FILL=0;
  FLT_ADD=1;
  FLT_PIXELS=2;
  FLT_BLEND=3;
  FLT_MAKEALPHA=4;
  FLT_PERLIN=5;
  FLT_BUMP=6;
  FLT_NORMALS=7;
  FLT_GLOWRECT=8;
  FLT_DISTORT=9;
  FLT_TRANSFORM=10;
  FLT_BLUR=11;
  FLT_COLORRANGE=12;
  FLT_ADJUST=13;
  FltNames: array[FLT_FILL..FLT_ADJUST] of string =
    ('FILL', 'ADD', 'PIXELS', 'BLEND', 'MAKEALPHA', 'PERLIN', 'BUMP', 'NORMALS',
    'GLOWRECT', 'DISTORT', 'TRANSFORM', 'BLUR', 'COLORRANGE', 'ADJUST');

  ADDMODE_ADDCLAMP=0;
  ADDMODE_ADDWRAP=1;
  ADDMODE_SUBCLAMP=2;
  ADDMODE_SUBWRAP=3;
  ADDMODE_DIFF=4;
  ADDMODE_MUL=5;
  AddModes: array[ADDMODE_ADDCLAMP..ADDMODE_MUL] of string =
    ('ADDCLAMP', 'ADDWRAP', 'SUBCLAMP', 'SUBWRAP', 'DIFF', 'MUL');
  BLURFILTER_FLAT=0;
  BLURFILTER_LINEAR=1;
  BLURFILTER_GAUSS=2;
  BlurFilters: array[BLURFILTER_FLAT..BLURFILTER_GAUSS] of string =
    ('FLAT', 'LINEAR', 'GAUSS');
  FlagClampX=1;
  FlagClampY=2;
  FlagNoLERP=4;

  BDegToRad=pi/128;

type
  PFillParams=^TFillParams;
  TFillParams=packed record
    Color: TRGBA;
  end;
  PAddParams=^TAddParams;
  TAddParams=packed record
    Mode: Byte;
  end;
  PPixelsParams=^TPixelsParams;
  TPixelsParams=packed record
    Count: Integer;
    Color0, Color1: TRGBA;
  end;
  PPerlinParams=^TPerlinParams;
  TPerlinParams=packed record
    Freq, Octaves, Fade, Amp: Byte;
    Color0, Color1: TRGBA;
  end;
  PBumpParams=^TBumpParams;
  TBumpParams=packed record
    Phi, Theta, Amplify, DiffAmount, SpecAmount, SpecPower: Byte;
  end;
  PNormalsParams=^TNormalsParams;
  TNormalsParams=packed record
    Amount: Byte;
  end;
  PGlowRectParams=^TGlowRectParams;
  TGlowRectParams=packed record
    PosX, PosY, RadX, RadY, SizeX, SizeY, Blend, Pow: Word;
    Color: TRGBA;
  end;
  PDistortParams=^TDistortParams;
  TDistortParams=packed record
    Amount, Flags: Byte;
  end;
  PTransformParams=^TTransformParams;
  TTransformParams=packed record
    Angle: Byte;
    ZoomX, ZoomY: Word;
    ScrollX, ScrollY: Word;
    Flags: Byte;
  end;
  PBlurParams=^TBlurParams;
  TBlurParams=packed record
    SizeX, SizeY, Filter, Amp, Flags: Byte;
  end;
  PColorRangeParams=^TColorRangeParams;
  TColorRangeParams=packed record
    Color0, Color1: TRGBA;
  end;
  PAdjustParams=^TAdjustParams;
  TAdjustParams=packed record
    Brightness, Contrast, Gamma: Byte;
  end;

