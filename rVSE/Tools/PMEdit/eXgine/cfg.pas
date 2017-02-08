{
  Включение статической линковки eXgine с exe
  При объявлении EX_STATIC, приложение компилируется в один exe файл
  В проекте (dpr файле) должны быть прописаны пути ко всем библиотекам
  или же файл проекта должен лежать в директории с ними (рекомендуется)
  По умолчанию директива EX_STATIC отключена (используется eXgine.dll)

  !!! Внимание  !!!
  eXgine.dll должна быть скомпилирована и использована в проекте, имеющем
  те же настройки в cfg.pas.
  В противном случае, будет несоответствие интерфейсов, и ошибок не избежать...

  Для проигрывания Ogg Vorbis файлов, в директории с exe должны лежать модули:
    ogg.dll, vorbis.dll, vorbisfile.dll (http://xiph.org/)
    и соответствующая директива NO_OGG не должна быть объявлена
}
  {.$DEFINE EX_STATIC} // ни на что не влияет при компиляции в eXgine.dll :)

  //{$DEFINE NO_TEX}  // без ITexture (также отключит шрифты...)
  //{$DEFINE NO_VFP}  // IShader
  //{$DEFINE NO_VBO}  // IVBuffer
  {$DEFINE NO_SND}  // ISound
  {$DEFINE NO_INP}  // IInput
  {$DEFINE NO_VEC}  // IVector

  {$IFNDEF NO_TEX}
    //{$DEFINE NO_TGA}  // отключить поддержку tga
    //{$DEFINE NO_BJG}  // bmp, jpg и gif файлов - текстур
    //{$DEFINE NO_FBO}  // рендеринга в текстуру с использованием FBO
  {$ENDIF}

  {$IFNDEF NO_SND}
    //{$DEFINE NO_MCI}  // без проигрывания мультимедийных файлов (wav, mp3, wma, avi, asf и т.п.)
    //{$DEFINE NO_OGG}  // отключает поддержку ogg модуля
  {$ENDIF}

  {$IFNDEF NO_INP}
    //{$DEFINE NO_JOY}  // отключить поддержку джойстика
  {$ENDIF}

