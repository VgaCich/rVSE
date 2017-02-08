unit Import3ds;

interface

uses
  Windows, SysUtils, Classes, Controls, Dialogs, PMBuild;

function Read3ds(Data, PMesh: TStream): Boolean;

implementation

const
  ROOT_CHUNK=$4D4D;
  EDIT_CHUNK=$3D3D;
  EDIT_OBJECT_CHUNK=$4000;
  OBJ_TRIMESH_CHUNK=$4100;
  TRI_VERTEX_CHUNK=$4110;
  TRI_MAPPINGCOORDS_CHUNK=$4140;
  TRI_FACES_CHUNK=$4120;

type
  TChunk=packed record
    ID: Word;
    Size: Cardinal;
  end;

function FindChunk(Data: TStream; FindTo: Cardinal; ChunkID: Word): Boolean;
var
  Chunk: TChunk;
begin
  Result:=false;
  while Data.Position<FindTo do
  begin
    Data.Read(Chunk, SizeOf(Chunk));
    if Chunk.ID=ChunkID then
    begin
      Data.Seek(-SizeOf(Chunk), soFromCurrent);
      Result:=true;
      Exit;
    end;
    Data.Seek(Chunk.Size-SizeOf(Chunk), soFromCurrent);
  end;
end;

function Read3ds(Data, PMesh: TStream): Boolean;
var
  Chunk: TChunk;
  ChunkEnd, ParentStart, ParentEnd: Cardinal;

  procedure ReadChunk;
  begin
    Data.Read(Chunk, SizeOf(Chunk));
    ChunkEnd:=Data.Position+Chunk.Size-SizeOf(Chunk);
  end;

  function ReadString: string;
  var
    C: Char;
  begin
    Result:='';
    Data.Read(C, SizeOf(C));
    while C<>#0 do
    begin
      Result:=Result+C;
      Data.Read(C, SizeOf(C));
    end;
  end;

var
  Count: Word;
  TempByte: Byte;
  VA: packed array of TVertex;
  IA: packed array of TFace;
  TempSingle: Single;
  i: Integer;
begin
  Result:=false;
  if not Assigned(Data) or not Assigned(PMesh) then Exit;
  try
    ReadChunk;
    if (Chunk.ID<>ROOT_CHUNK) or (Chunk.Size<>Data.Size)
      then raise Exception.Create('');
    if not FindChunk(Data, ChunkEnd, EDIT_CHUNK)
      then raise Exception.Create('');
    ReadChunk;
    ParentEnd:=ChunkEnd;
    while FindChunk(Data, ParentEnd, EDIT_OBJECT_CHUNK) do
    begin
      ReadChunk;
      if MessageDlg('Import object "'+ReadString+'"?', mtInformation, [mbYes, mbNo], 0)=mrNo then
      begin
        Data.Position:=ChunkEnd;
        Continue;
      end;
      try
        if not FindChunk(Data, ParentEnd, OBJ_TRIMESH_CHUNK)
          then raise Exception.Create('');
        ReadChunk;
        ParentStart:=Data.Position;
        ParentEnd:=ChunkEnd;
        if not FindChunk(Data, ParentEnd, TRI_VERTEX_CHUNK)
          then raise Exception.Create('');
        ReadChunk;
        Data.Read(Count, SizeOf(Count));
        if (Count>256) or (Count<1)
          then raise Exception.Create('Object vertices count are out of bounds [1..256]');
        SetLength(VA, Count);
        ZeroMemory(@VA[0], SizeOf(TVertex)*Count);
        for i:=0 to Count-1 do
        begin
          Data.Read(VA[i].Vertex, SizeOf(VA[0].Vertex));
          TempSingle:=VA[i].Vertex.Z;
          VA[i].Vertex.Z:=VA[i].Vertex.Y;
          VA[i].Vertex.Y:=TempSingle;
        end;
        Data.Position:=ParentStart;
        if FindChunk(Data, ParentEnd, TRI_MAPPINGCOORDS_CHUNK) then
        begin
          ReadChunk;
          Data.Read(Count, SizeOf(Count));
          if Count<>Length(VA)
            then raise Exception.Create('');
          for i:=0 to Count-1 do
            Data.Read(VA[i].TexCoord, SizeOf(VA[0].TexCoord));
        end;
        Data.Position:=ParentStart;
        if not FindChunk(Data, ParentEnd, TRI_FACES_CHUNK)
          then raise Exception.Create('');
        ReadChunk;
        Data.Read(Count, SizeOf(Count));
        SetLength(IA, Count);
        for i:=0 to Count-1 do
        begin
          Data.Read(IA[i], SizeOf(TFace));
          Data.Seek(2, soFromCurrent);
        end;
        ComputeNormalsTriangles(VA, IA);
        TempByte:=Length(VA)-1;
        PMesh.Write(TempByte, SizeOf(TempByte));
        PMesh.Write(VA[0], Length(VA)*SizeOf(TVertex));
        Count:=Length(IA);
        PMesh.Write(Count, SizeOf(Count));
        PMesh.Write(IA[0], Length(IA)*SizeOf(TFace));
        Result:=true;
        Break;
      finally
        Finalize(VA);
        Finalize(IA);
      end;
    end;
  except
    on E: Exception do
      if E.Message=''
        then MessageDlg('Invalid 3ds file', mtError, [mbOk], 0)
        else MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

end.