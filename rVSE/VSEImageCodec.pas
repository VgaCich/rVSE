unit VSEImageCodec;

interface

uses
  Windows, AvL, avlUtils, GDIPAPI, avlIStreamAdapter{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TImageFormat = (ifBMP, ifJPEG, ifGIF, ifPNG, ifTIFF); // Image formats
  TPixelFormat = (pfGS8bit, pfBGR24bit, pfRGBA32bit, pfBGRA32bit); // Pixel data format: 8-bit grayscale, 24-bit BGR, 32-bit RGBA and BGRA
  TSaveFunction = function(Bitmap, UserData: Pointer; EncCLSID: PGUID; EncParams: PEncoderParameters): TStatus; // used internally
  TImage = class
  private
    FWidth, FHeight: Cardinal;
    FStride: Integer;
    FPixelFormat: TPixelFormat;
    FPixels: PByteArray;
    procedure LoadImage(Bitmap: Pointer; Status: TStatus);
    procedure SaveImage(SaveFunction: TSaveFunction; UserData: Pointer; ImageFormat: TImageFormat; Quality: Cardinal);
    procedure SetPixels(Value: PByteArray);
  public
    constructor Create; overload; // Create codec with no image
    constructor Create(Width, Height: Cardinal; PixelFormat: TPixelFormat; Stride: Integer = 0); overload; // Create codec with void image
    destructor Destroy; override;
    procedure Load(const FileName: string); overload; // Load image from file
    procedure Load(Stream: TStream); overload; // Load image from stream
    procedure Load(Mem: Pointer; Size: Cardinal); overload; // Load image from memory
    function  LoadRaw(Stream: TStream): Boolean; // Load raw image data; returns true if successful
    procedure Save(const FileName: string; ImageFormat: TImageFormat; Quality: Cardinal = 0); overload; // Save image to file in specified format
    procedure Save(Stream: TStream; ImageFormat: TImageFormat; Quality: Cardinal = 0); overload; // Save image to stream in specified format
    procedure SaveRaw(Stream: TStream); // Save raw image data
    procedure Pack; // Remove rows alignment
    property Width: Cardinal read FWidth; // Image width
    property Height: Cardinal read FHeight; // Image height
    property Stride: Integer read FStride; // Image stride - size of row in bytes, including alignment
    property PixelFormat: TPixelFormat read FPixelFormat; // Format of pixel data
    property Pixels: PByteArray read FPixels write SetPixels; // Pointer to raw pixels array
  end;

const
  ImageFormatMime: array[TImageFormat] of string = ('image/bmp', 'image/jpeg', 'image/gif', 'image/png', 'image/tiff');
  ImageFormatExtension: array[TImageFormat] of string = ('.bmp', '.jpg', '.gif', '.png', '.tif');

implementation

const
  SCantSaveImage = 'Image.SaveImage: can''t save image (Status=%d)';
  SGDIPEncoderIsNotAvailable = 'Image.SaveImage: GDI+ encoder for %s is not available';
  SSaveCantCreateGDIBitmap = 'Image.SaveImage: can''t create GDI+ bitmap (Status=%d)';
  SCantLockBitmapData = 'Image.LoadImage: can''t lock bitmap data (Status=%d)';
  SCanTGetPalette = 'Image.LoadImage: can''t get palette';
  SUnknownPixelFormat = 'Image.LoadImage: unknown pixel format';
  SCantGetImageProperties = 'Image.LoadImage: can''t get image properties';
  SLoadCantCreateGDIBitmap = 'Image.LoadImage: can''t create GDI+ bitmap (Status=%d)';
  BitDepths: array[TPixelFormat] of Integer = (8, 24, 32, 32);
  PixelFormats: array[TPixelFormat] of Integer = (PixelFormat8bppIndexed, PixelFormat24bppRGB, 0, PixelFormat32bppARGB);
  RawMagic: Cardinal = $57415249;
  SGDIPIsNotInitialized = 'Image: GDI+ is not initialized (Status=%d)';

function GetEncoderCLSID(const Format: string): TGUID;
var
  i, Num, Size: Cardinal;
  ImageCodecsInfo: PImageCodecsInfo;
begin
  ZeroMemory(@Result, SizeOf(Result));
  GdipGetImageEncodersSize(Num, Size);
  if Size = 0 then Exit;
  GetMem(ImageCodecsInfo, Size);
  GdipGetImageEncoders(Num, Size, PImageCodecInfo(ImageCodecsInfo));
  for i := 0 to Num-1 do
    if ImageCodecsInfo[i].MimeType = Format then
    begin
      Result := ImageCodecsInfo[i].Clsid;
      Break;
    end;
  FreeMem(ImageCodecsInfo);
end;

function GSPalette: TColorPalette256;
var
  i: Integer;
begin
  with Result do
  begin
    Flags := PaletteFlagsGrayScale;
    Count := 256;
    for i := 0 to 255 do
      Entries[i] := i or (i shl 8) or (i shl 16) or $FF000000;
  end;
end;

function SaveToFileFunction(Bitmap, UserData: Pointer; EncCLSID: PGUID; EncParams: PEncoderParameters): TStatus;
begin
  Result := GdipSaveImageToFile(Bitmap, UserData, EncCLSID, EncParams);
end;

function SaveToStreamFunction(Bitmap, UserData: Pointer; EncCLSID: PGUID; EncParams: PEncoderParameters): TStatus;
var
  StreamAdapter: IStream;
begin
  TIStreamAdapter.Create(TStream(UserData)).GetInterface(IStream, StreamAdapter);
  Result := GdipSaveImageToStream(Bitmap, StreamAdapter, EncCLSID, EncParams);
end;

{ TImage }

constructor TImage.Create(Width, Height: Cardinal; PixelFormat: TPixelFormat; Stride: Integer);
begin
  if GdiplusInitStatus <> Ok then raise Exception.CreateFmt(SGDIPIsNotInitialized, [Integer(GdiplusInitStatus)]);
  FWidth := Width;
  FHeight := Height;
  FPixelFormat := PixelFormat;
  if Stride = 0 then Stride := Width * BitDepths[PixelFormat] div 8;
  FStride := Stride;
  GetMem(FPixels, Stride * Height);
end;

constructor TImage.Create;
begin
  if GdiplusInitStatus <> Ok then raise Exception.CreateFmt(SGDIPIsNotInitialized, [Integer(GdiplusInitStatus)]);
end;

destructor TImage.Destroy;
begin
  if Assigned(FPixels) then FreeMem(FPixels);
  inherited;
end;

procedure TImage.Load(const FileName: string);
var
  Bitmap: Pointer;
  Status: TStatus;
begin
  Status := GdipCreateBitmapFromFile(PWideChar(WideString(FileName)), Bitmap);
  {$IFDEF VSE_LOG}try{$ENDIF}
    LoadImage(Bitmap, Status);
  {$IFDEF VSE_LOG}
  except
    LogF(llError, 'Image.Load(%s): Exception "%s"', [FileName, Exception(ExceptObject).Message]);
    raise;
  end;
  {$ENDIF}
end;

procedure TImage.Load(Mem: Pointer; Size: Cardinal);
var
  Bitmap: Pointer;
  Status: TStatus;
  Glob: HGLOBAL;
  Stream: IStream;
begin
  Glob := CreateIStreamOnMemory(Mem, Size, Stream);
  try
    if (Glob <> 0) and Assigned(Stream) then
    begin
      Status := GdipCreateBitmapFromStream(Stream, Bitmap);
      {$IFDEF VSE_LOG}try{$ENDIF}
        LoadImage(Bitmap, Status);
      {$IFDEF VSE_LOG}
      except
        LogF(llError, 'Image.Load($%x, %d): Exception "%s"', [Mem, Size, Exception(ExceptObject).Message]);
        raise;
      end;
      {$ENDIF}
    end;
  finally
    if Glob <> 0 then GlobalFree(Glob);
  end;
end;

procedure TImage.Load(Stream: TStream);
var
  Bitmap: Pointer;
  Status: TStatus;
  StreamAdapter: IStream;
begin
  TIStreamAdapter.Create(Stream).GetInterface(IStream, StreamAdapter);
  Status := GdipCreateBitmapFromStream(StreamAdapter, Bitmap);
  {$IFDEF VSE_LOG}try{$ENDIF}
  LoadImage(Bitmap, Status);
  {$IFDEF VSE_LOG}
  except
    LogF(llError, 'Image.Load(TStream($%x)): Exception "%s"', [Integer(Stream), Exception(ExceptObject).Message]);
    raise;
  end;
  {$ENDIF}
end;

procedure TImage.LoadImage(Bitmap: Pointer; Status: TStatus);
var
  BitmapData: TBitmapData;
  i, PixFormat, PalSize: Integer;
  Palette: TColorPalette256;
  Rect: TGPRect;
begin
  if Status <> Ok then
    raise Exception.CreateFmt(SLoadCantCreateGDIBitmap, [Integer(Status)]);
  try
    if (GdipGetImageWidth(Bitmap, FWidth) <> Ok) or
       (GdipGetImageHeight(Bitmap, FHeight) <> Ok) or
       (GdipGetImagePixelFormat(Bitmap, PixFormat) <> Ok)
      then raise Exception.Create(SCantGetImageProperties);
    case (PixFormat and $FF00) shr 8 of
      1, 4, 8: FPixelFormat := pfGS8bit;
      16, 24: FPixelFormat := pfBGR24bit;
      32: FPixelFormat := pfBGRA32bit;
      else raise Exception.Create(SUnknownPixelFormat);
    end;
    if PixFormat and PixelFormatIndexed <> 0 then
    begin
      if (GdipGetImagePaletteSize(Bitmap, PalSize) <> Ok) or
         (GdipGetImagePalette(Bitmap, Palette, Min(PalSize, SizeOf(Palette))) <> Ok)
        then raise Exception.Create(SCanTGetPalette);
      with Palette do
        for i := 0 to Count - 1 do
          if Entries[i] <> Entries[i] or ((Entries[i] shl 8) and $00FFFF00) then
          begin
            FPixelFormat := pfBGR24bit;
            Break;
          end;
    end;
    with Rect do
    begin
      X := 0;
      Y := 0;
      Width := FWidth;
      Height := FHeight;
    end;
    ZeroMemory(@BitmapData, SizeOf(BitmapData));
    Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormats[FPixelFormat], @BitmapData);
    if (Status = InvalidParameter) and (FPixelFormat = pfGS8bit) then
    begin
      FPixelFormat := pfBGR24bit;
      Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormats[FPixelFormat], @BitmapData);
    end;
    if Status <> Ok then
      raise Exception.CreateFmt(SCantLockBitmapData, [Integer(Status)]);
    try
      FStride := Abs(BitmapData.Stride);
      ReallocMem(FPixels, FStride * FHeight);
      Move(BitmapData.Scan0^, FPixels^, FStride * FHeight);
    finally
      GdipBitmapUnlockBits(Bitmap, @BitmapData);
    end;
  finally
    GdipDisposeImage(Bitmap);
  end;
end;

function TImage.LoadRaw(Stream: TStream): Boolean;
var
  Magic: Cardinal;
begin
  Result := false;
  if (Stream.Read(Magic, SizeOf(Magic)) < SizeOf(Magic)) or (Magic <> RawMagic) then Exit;
  Stream.Read(FWidth, SizeOf(FWidth));
  Stream.Read(FHeight, SizeOf(FHeight));
  Stream.Read(FStride, SizeOf(FStride));
  Stream.Read(FPixelFormat, SizeOf(FPixelFormat));
  if (Stream.Size - Stream.Position < FStride * FHeight) or
    not (FPixelFormat in [Low(TPixelFormat) .. High(TPixelFormat)]) then Exit;
  ReallocMem(FPixels, FStride * FHeight);
  Stream.Read(FPixels^, FStride * FHeight);
  Result := true;
end;

procedure TImage.Save(const FileName: string; ImageFormat: TImageFormat; Quality: Cardinal);
begin
  {$IFDEF VSE_LOG}try{$ENDIF}
  SaveImage(SaveToFileFunction, PWideChar(WideString(FileName)), ImageFormat, Quality);
  {$IFDEF VSE_LOG}
  except
    LogF(llError, 'Image.Load(%s): Exception "%s"', [FileName, Exception(ExceptObject).Message]);
    raise;
  end;
  {$ENDIF}
end;

procedure TImage.Save(Stream: TStream; ImageFormat: TImageFormat; Quality: Cardinal);
begin
  {$IFDEF VSE_LOG}try{$ENDIF}
  SaveImage(SaveToStreamFunction, Pointer(Stream), ImageFormat, Quality);
  {$IFDEF VSE_LOG}
  except
    LogF(llError, 'Image.Save(TStream($%x)): Exception "%s"', [Integer(Stream), Exception(ExceptObject).Message]);
    raise;
  end;
  {$ENDIF}
end;

procedure TImage.SaveImage(SaveFunction: TSaveFunction; UserData: Pointer; ImageFormat: TImageFormat; Quality: Cardinal);
var
  Status: TStatus;
  Bitmap: Pointer;
  EncCLSID: TGUID;
  EncParams: TEncoderParameters;
  PEncParams: PEncoderParameters;
begin
  if not Assigned(FPixels) then Exit;
  Status := GdipCreateBitmapFromScan0(FWidth, FHeight, FStride, PixelFormats[FPixelFormat], FPixels, Bitmap);
  if Status <> Ok then
    raise Exception.CreateFmt(SSaveCantCreateGDIBitmap, [Integer(Status)]);
  try
    if FPixelFormat = pfGS8bit then GdipSetImagePalette(Bitmap, GSPalette);
    GdipImageRotateFlip(Bitmap, RotateNoneFlipY);
    EncCLSID := GetEncoderCLSID(ImageFormatMime[ImageFormat]);
    if EncCLSID.D1 = 0 then
      raise Exception.CreateFmt(SGDIPEncoderIsNotAvailable, [ImageFormatMime[ImageFormat]]);
    if Quality > 0 then
    begin
      EncParams.Count := 1;
      with EncParams.Parameter[0] do
      begin
        Guid := EncoderQuality;
        NumberOfValues := 1;
        Type_ := EncoderParameterValueTypeLong;
        Value := @Quality;
      end;
      PEncParams := @EncParams;
    end
      else PEncParams := nil;
    Status := SaveFunction(Bitmap, UserData, @EncCLSID, PEncParams);
    if Status <> Ok then
      raise Exception.CreateFmt(SCantSaveImage, [Integer(Status)]);
  finally
    GdipDisposeImage(Bitmap);
  end;
end;

procedure TImage.SaveRaw(Stream: TStream);
begin
  Stream.Write(RawMagic, SizeOf(RawMagic));
  Stream.Write(FWidth, SizeOf(FWidth));
  Stream.Write(FHeight, SizeOf(FHeight));
  Stream.Write(FStride, SizeOf(FStride));
  Stream.Write(FPixelFormat, SizeOf(FPixelFormat));
  Stream.Write(FPixels^, FStride * FHeight);
end;

procedure TImage.Pack;
var
  i, RowSize: Integer;
begin
  RowSize := FWidth * BitDepths[FPixelFormat] div 8;
  if FStride > RowSize then
    for i := 1 to FHeight - 1 do
      Move(IncPtr(FPixels, i * FStride)^, IncPtr(FPixels, i * RowSize)^, RowSize);
  FStride := RowSize;
end;

procedure TImage.SetPixels(Value: PByteArray);
begin
  Move(Value^, FPixels^, FStride * FHeight);
end;

end.