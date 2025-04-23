unit AudioManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Dialogs, FMX.Forms,
{$IFDEF ANDROID}
  androidapi.jni.media, FMX.Helpers.Android, androidapi.jni.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, androidapi.helpers,
{$ENDIF}
{$IFDEF IOS}
  MacApi.CoreFoundation, FMX.Platform.iOS, iOSapi.CocoaTypes, iOSapi.AVFoundation, iOSapi.Foundation,
{$ELSE}
{$IFDEF MACOS}
  MacApi.CoreFoundation, FMX.Platform.Mac, Macapi.CocoaTypes, Macapi.AppKit, Macapi.Foundation, Macapi.Helpers,
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  MMSystem,
{$ENDIF}
  Generics.Collections;

type
  TSoundRec = record
    SFilename: String;
    SName: String;
    SNameExt: string;
    SID: integer;
  end;
  PSoundRec = ^TSoundRec;

  TAudioManager = class
  private
    fSoundsList: TList;
{$IFDEF ANDROID}
    fAudioMgr: JAudioManager;
    fSoundPool: JSoundPool;
{$ENDIF}
{$IFDEF IOS}
    AudioPlayers: TList<TAVAudioPlayer>;
{$ENDIF}

    function GetSoundsCount: integer;
    function GetSoundFromIndex(AIndex: integer): PSoundRec;
  public
    constructor Create;
    destructor Destroy; override;

    function AddSound(ASoundFile: string): integer;
    procedure DeleteSound(AName: String); overload;
    procedure DeleteSound(AIndex: integer); overload;
    procedure PlaySound(AName: String); overload;
    procedure PlaySound(AIndex: integer); overload;

    property SoundsCount: Integer read GetSoundsCount;
    property Sounds[AIndex: integer]: PSoundRec read GetSoundFromIndex;
  end;

implementation

{ TAudioManager }

constructor TAudioManager.Create;
begin
  try
    fSoundsList := TList.Create;

    {$IFDEF ANDROID}
    fAudioMgr := TJAudioManager.Wrap((SharedActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE) as ILocalObject).GetObjectID);
    fSoundPool := TJSoundPool.JavaClass.init(10, TJAudioManager.JavaClass.STREAM_MUSIC, 0); // زيادة عدد الأصوات إلى 10
    {$ENDIF}

    {$IFDEF IOS}
    AudioPlayers := TList<TAVAudioPlayer>.Create;
    {$ENDIF}
  except
    on E: Exception do
      raise Exception.Create('[TAudioManager.Create] : ' + E.Message);
  end;
end;

destructor TAudioManager.Destroy;
var
  i: integer;
  wRec: PSoundRec;
begin
  try
    for i := fSoundsList.Count - 1 downto 0 do
    begin
      wRec := fSoundsList[i];
      Dispose(wRec);
      fSoundsList.Delete(i);
    end;
    fSoundsList.Free;

    {$IFDEF ANDROID}
    if Assigned(fSoundPool) then
      fSoundPool.release;
    fSoundPool := nil;
    fAudioMgr := nil;
    {$ENDIF}

    {$IFDEF IOS}
    for var Player in AudioPlayers do
      Player.release;
    AudioPlayers.Free;
    {$ENDIF}

    inherited;
  except
    on E: Exception do
      raise Exception.Create('[TAudioManager.Destroy] : ' + E.Message);
  end;
end;

function TAudioManager.AddSound(ASoundFile: string): integer;
var
  wSndRec: PSoundRec;
{$IFDEF ANDROID}
  soundID: NativeInt;
{$ENDIF}
{$IFDEF IOS}
  URL: NSURL;
  Player: AVAudioPlayer;
{$ENDIF}
begin
  Result := -1;
  try
    New(wSndRec);
    wSndRec.SFilename := ASoundFile;
    wSndRec.SNameExt := ExtractFileName(ASoundFile);
    wSndRec.SName := ChangeFileExt(wSndRec.SNameExt, '');

    {$IFDEF ANDROID}
    soundID := fSoundPool.load(StringToJString(ASoundFile), 0);
    wSndRec.SID := soundID;
    {$ENDIF}

    {$IFDEF IOS}
    URL := TNSUrl.Wrap(TNSUrl.OCClass.fileURLWithPath(StrToNSStr(ASoundFile)));
    Player := TAVAudioPlayer.Wrap(TAVAudioPlayer.Alloc.initWithContentsOfURL(URL, True));
    if Assigned(Player) then
    begin
      Player.prepareToPlay;
      wSndRec.SID := Integer(Pointer(Player));
    end;
    {$ENDIF}

    {$IFDEF MSWINDOWS}
    wSndRec.SID := 0; // لا حاجة لـ ID على Windows
    {$ENDIF}

    Result := fSoundsList.Add(wSndRec);
  except
    on E: Exception do
      raise Exception.Create('[TAudioManager.AddSound] : ' + E.Message);
  end;
end;

procedure TAudioManager.DeleteSound(AIndex: integer);
var
  wRec: PSoundRec;
begin
  try
    if AIndex < fSoundsList.Count then
    begin
      wRec := fSoundsList[AIndex];

      {$IFDEF ANDROID}
      if Assigned(fSoundPool) then
        fSoundPool.unload(wRec.SID);
      {$ENDIF}

      {$IFDEF IOS}
      if Assigned(AudioPlayers) then
        for var i := AudioPlayers.Count - 1 downto 0 do
          if Integer(Pointer(AudioPlayers[i])) = wRec.SID then
            AudioPlayers.Delete(i);
      {$ENDIF}

      Dispose(wRec);
      fSoundsList.Delete(AIndex);
    end;
  except
    on E: Exception do
      raise Exception.Create('[TAudioManager.DeleteSound] : ' + E.Message);
  end;
end;

procedure TAudioManager.DeleteSound(AName: String);
var
  i: integer;
begin
  try
    for i := 0 to fSoundsList.Count - 1 do
    begin
      if CompareText(PSoundRec(fSoundsList[i]).SName, AName) = 0 then
      begin
        DeleteSound(i);
        Break;
      end;
    end;
  except
    on E: Exception do
      raise Exception.Create('[TAudioManager.DeleteSound] : ' + E.Message);
  end;
end;

procedure TAudioManager.PlaySound(AIndex: integer);
var
  wRec: PSoundRec;
{$IFDEF ANDROID}
  wCurrVolume, wMaxVolume, wVolume: Double;
{$ENDIF}
{$IFDEF IOS}
  URL: NSURL;
  Player: AVAudioPlayer;
{$ENDIF}
begin
  if AIndex < fSoundsList.Count then
  begin
    wRec := fSoundsList[AIndex];

    {$IFDEF ANDROID}
    if Assigned(fAudioMgr) and Assigned(fSoundPool) then
    begin
      wCurrVolume := fAudioMgr.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
      wMaxVolume := fAudioMgr.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
      wVolume := wCurrVolume / wMaxVolume;
      fSoundPool.play(wRec.SID, wVolume, wVolume, 1, 0, 1);
    end;
    {$ENDIF}

    {$IFDEF IOS}
    URL := TNSUrl.Wrap(TNSUrl.OCClass.fileURLWithPath(StrToNSStr(wRec.SFilename)));
    Player := TAVAudioPlayer.Wrap(TAVAudioPlayer.Alloc.initWithContentsOfURL(URL, True));
    if Assigned(Player) then
    begin
      Player.prepareToPlay;
      Player.play;
      AudioPlayers.Add(Player);
    end;
    {$ENDIF}

    {$IFDEF MSWINDOWS}
    sndPlaySound(PChar(wRec.SFilename), SND_NODEFAULT or SND_ASYNC);
    {$ENDIF}
  end;
end;

procedure TAudioManager.PlaySound(AName: String);
var
  i: integer;
begin
  for i := 0 to fSoundsList.Count - 1 do
  begin
    if CompareText(PSoundRec(fSoundsList[i]).SName, AName) = 0 then
    begin
      PlaySound(i);
      Break;
    end;
  end;
end;

function TAudioManager.GetSoundsCount: integer;
begin
  Result := fSoundsList.Count;
end;

function TAudioManager.GetSoundFromIndex(AIndex: integer): PSoundRec;
begin
  if AIndex < fSoundsList.Count then
    Result := PSoundRec(fSoundsList[AIndex])
  else
    Result := nil;
end;

end.
