{
  ��������� ����������� �������� eXgine � exe
  ��� ���������� EX_STATIC, ���������� ������������� � ���� exe ����
  � ������� (dpr �����) ������ ���� ��������� ���� �� ���� �����������
  ��� �� ���� ������� ������ ������ � ���������� � ���� (�������������)
  �� ��������� ��������� EX_STATIC ��������� (������������ eXgine.dll)

  !!! ��������  !!!
  eXgine.dll ������ ���� �������������� � ������������ � �������, �������
  �� �� ��������� � cfg.pas.
  � ��������� ������, ����� �������������� �����������, � ������ �� ��������...

  ��� ������������ Ogg Vorbis ������, � ���������� � exe ������ ������ ������:
    ogg.dll, vorbis.dll, vorbisfile.dll (http://xiph.org/)
    � ��������������� ��������� NO_OGG �� ������ ���� ���������
}
  {.$DEFINE EX_STATIC} // �� �� ��� �� ������ ��� ���������� � eXgine.dll :)

  //{$DEFINE NO_TEX}  // ��� ITexture (����� �������� ������...)
  //{$DEFINE NO_VFP}  // IShader
  //{$DEFINE NO_VBO}  // IVBuffer
  {$DEFINE NO_SND}  // ISound
  {$DEFINE NO_INP}  // IInput
  {$DEFINE NO_VEC}  // IVector

  {$IFNDEF NO_TEX}
    //{$DEFINE NO_TGA}  // ��������� ��������� tga
    //{$DEFINE NO_BJG}  // bmp, jpg � gif ������ - �������
    //{$DEFINE NO_FBO}  // ���������� � �������� � �������������� FBO
  {$ENDIF}

  {$IFNDEF NO_SND}
    //{$DEFINE NO_MCI}  // ��� ������������ �������������� ������ (wav, mp3, wma, avi, asf � �.�.)
    //{$DEFINE NO_OGG}  // ��������� ��������� ogg ������
  {$ENDIF}

  {$IFNDEF NO_INP}
    //{$DEFINE NO_JOY}  // ��������� ��������� ���������
  {$ENDIF}

