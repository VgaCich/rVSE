unit VSEVertexArrayUtils;

interface

uses AvL, avlUtils, avlVectors, VSEArrayBuffer;

procedure ComputeNormalsTriangles(var VA: array of TVertex; const IA: array of TFaceW); //Compute normals to triangle list
procedure ComputeNormalsTrianglesStrip(var VA: array of TVertex; const IA: array of Integer); //Compute normals to triangle strip

implementation

procedure ComputeNormalsTriangles(var VA: array of TVertex; const IA: array of TFaceW);
var
  i: Integer;
  Normal: TVector3D;
begin
  for i:=0 to High(IA) do
  begin
    if (IA[i].Vert1=IA[i].Vert2) or (IA[i].Vert1=IA[i].Vert3) or (IA[i].Vert2=IA[i].Vert3) then Continue;
    Normal:=TriangleNormal(VA[IA[i].Vert1].Vertex, VA[IA[i].Vert2].Vertex, VA[IA[i].Vert3].Vertex);
    VA[IA[i].Vert1].Normal:=VectorAdd(VA[IA[i].Vert1].Normal, VectorMultiply(Normal,
      TriangleAngle(VA[IA[i].Vert1].Vertex, VA[IA[i].Vert2].Vertex, VA[IA[i].Vert3].Vertex)));
    VA[IA[i].Vert2].Normal:=VectorAdd(VA[IA[i].Vert2].Normal, VectorMultiply(Normal,
      TriangleAngle(VA[IA[i].Vert2].Vertex, VA[IA[i].Vert3].Vertex, VA[IA[i].Vert1].Vertex)));
    VA[IA[i].Vert3].Normal:=VectorAdd(VA[IA[i].Vert3].Normal, VectorMultiply(Normal,
      TriangleAngle(VA[IA[i].Vert3].Vertex, VA[IA[i].Vert1].Vertex, VA[IA[i].Vert2].Vertex)));
  end;
  for i:=0 to High(VA) do VectorNormalize(VA[i].Normal);
end;

procedure ComputeNormalsTrianglesStrip(var VA: array of TVertex; const IA: array of Integer);
var
  i: Integer;
  Normal: TVector3D;
begin
  for i:=0 to High(IA)-2 do
  begin
    if (IA[i]=IA[i+1]) or (IA[i]=IA[i+2]) or (IA[i+1]=IA[i+2]) then Continue;
    Normal:=TriangleNormal(VA[IA[i]].Vertex, VA[IA[i+1]].Vertex, VA[IA[i+2]].Vertex);
    if i mod 2 = 1 then VectorScale(Normal, -1);
    VA[IA[i]].Normal:=VectorAdd(VA[IA[i]].Normal, VectorMultiply(Normal,
      TriangleAngle(VA[IA[i]].Vertex, VA[IA[i+1]].Vertex, VA[IA[i+2]].Vertex)));
    VA[IA[i+1]].Normal:=VectorAdd(VA[IA[i+1]].Normal, VectorMultiply(Normal,
      TriangleAngle(VA[IA[i+1]].Vertex, VA[IA[i+2]].Vertex, VA[IA[i]].Vertex)));
    VA[IA[i+2]].Normal:=VectorAdd(VA[IA[i+2]].Normal, VectorMultiply(Normal,
      TriangleAngle(VA[IA[i+2]].Vertex, VA[IA[i]].Vertex, VA[IA[i+1]].Vertex)));
  end;
  for i:=0 to High(VA) do VectorNormalize(VA[i].Normal);
end;

end.