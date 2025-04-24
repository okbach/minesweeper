unit AssetHelper;

interface

uses
  System.Classes, System.SysUtils;

type
  TAssetHelper = class
  public
    class function LoadAssetFileToStream(const RelativePath: string): TMemoryStream; static;
  end;

implementation

uses
  System.IOUtils
  {$IFDEF ANDROID}
  , Androidapi.Helpers,
    Androidapi.JNI.JavaTypes
  {$ENDIF}
  ;

{ TAssetHelper }

class function TAssetHelper.LoadAssetFileToStream(const RelativePath: string): TMemoryStream;
{$IFDEF ANDROID}
var
  AssetInputStream: JInputStream;
  Buffer: TJavaArray<Byte>;
  Size: Integer;
{$ENDIF}
var
  FullPath: string;
begin
  Result := TMemoryStream.Create;
  try
    {$IFDEF ANDROID}
      // Android: نستخدم الـ Assets
      AssetInputStream := TAndroidHelper.Context.getAssets.open(StringToJString(RelativePath));
      Size := AssetInputStream.available;
      SetLength(Buffer, Size);
      AssetInputStream.read(Buffer, 0, Size);
      Result.WriteBuffer(Buffer[0], Size);
      Result.Position := 0;
      AssetInputStream.close;
    {$ELSE}
      // باقي الأنظمة: نقرأ من نفس مجلد التطبيق
      FullPath := TPath.Combine(ExtractFilePath(ParamStr(0)), RelativePath);
      if not FileExists(FullPath) then
        raise Exception.Create('الملف غير موجود: ' + FullPath);
      Result.LoadFromFile(FullPath);
    {$ENDIF}
  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.Create('فشل تحميل الملف: ' + E.Message);
    end;
  end;
end;

end.

