type
  TPMTransform=packed record
    TranslateX, TranslateY, TranslateZ: SmallInt;
    Yaw, Pitch, Roll: ShortInt;
    ScaleX, ScaleY, ScaleZ: SmallInt;
  end;
  TPMMesh=packed record
    VertsCount: Byte;
    FacesCount: Word;
    Flags: Byte;
  end;
  TPMVertex=packed record
    X, Y, Z: ShortInt;
    Phi: ShortInt;
    Theta: Byte;
    U, V: Byte;
  end;
  TPMFace=packed record
    Vert1, Vert2, Vert3: Byte;
  end;
  TPMUVRect=packed record
    OrigU, OrigV, SizeU, SizeV: Byte;
  end;
  TPMUVCircle=packed record
    CenterU, CenterV, Radius: Byte;
  end;
  PPPCube=^TPPCube;
  TPPCube=packed record
    UV: packed array[0..2] of TPMUVRect;
  end;
  PPPSphere=^TPPSphere;
  TPPSphere=packed record
    Slices, Stacks, SlicesSector, StacksSector: Byte;
    UV: TPMUVRect;
  end;
  PPPCone=^TPPCone;
  TPPCone=packed record
    RadiusT, RadiusB, Slices, SlicesSector: Byte;
    UVSide: TPMUVRect;
    UVBaseT, UVBaseB: TPMUVCircle;
  end;
  PPPTorus=^TPPTorus;
  TPPTorus=packed record

  end;
  PPPTube=^TPPTube;
  TPPTube=packed record

  end;

const
  BDegToRad=pi/128;
  RadToBDeg=128/pi;
  ChunkModel=77;
  ChunkModelObject=1;
  ChunkObjectPrimitive=2;
  ChunkObjectMesh=3;
  ChunkMaterial=6;
  PrimitiveCube=0;
  PrimitiveSphere=1;
  PrimitiveCone=2;
  PrimitiveTorus=3;
  PrimitiveTube=4;
  PrimitiveParamSizes: array[PrimitiveCube..PrimitiveTube] of Integer=
    (SizeOf(TPPCube), SizeOf(TPPSphere), SizeOf(TPPCone),
     SizeOf(TPPTorus), SizeOf(TPPTube));
  ConeUVSize=SizeOf(TPMUVRect)+2*SizeOf(TPMUVCircle);
  //Flags
  MeshHasNormals=$01;
  MeshHasUV=$02;
  {CubeTexMergeUDSides=$01;
  CubeTexMergeRLSides=$02;
  CubeTexMergeFBSides=$04;}
  PMFInvertNormals=$20;
  PMFSmooth=$40;
  PMFTexInfo=$80;

