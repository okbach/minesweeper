unit uMinesweeperUI;

interface

uses
  System.SysUtils, System.Types, FMX.Layouts, FMX.Controls, FMX.Objects,
  FMX.Graphics, FMX.StdCtrls, FMX.Types, uMinesweeperLogic, Math,
  Skia, FMX.Skia, System.Classes, System.IOUtils,FMX.Dialogs,AudioManager;

type
  TCellUI = class
    Rectangle: TRectangle;
    hintNum: TLabel;
    Anim: TSkAnimatedImage;
  end;

  TMinesweeperUIHelper = class
  private
    class var
      CellUIArray: array of array of TCellUI;
      FlagAnimData: TMemoryStream;
      ExplosionAnimData: TMemoryStream;

  public
    class procedure InitializeBoardUI(const Board: TBoard; GridLayout: TGridLayout);
    class procedure DisplayCell(const Board: TBoard; X, Y: Integer);
    class procedure DisplayBoard(const Board: TBoard);
    class procedure UpdateCells(const Board: TBoard; const Changed: TArray<TChangedCell>);
    class procedure ResetBoardUI(GridLayout: TGridLayout);
    class function CreateEmptyBoard(Rows, Cols: Integer): TBoard;
    class function GetCellColor(I, J: Integer): Boolean;
  end;

implementation

uses
  Unit1;
  var
  AudioMgr: TAudioManager;

{ TMinesweeperUIHelper }

class procedure TMinesweeperUIHelper.InitializeBoardUI(const Board: TBoard; GridLayout: TGridLayout);
var
  I, J: Integer;
  CellSize, Square: Single;
  CellUI: TCellUI;
  BasePath, FlagPath, ExplosionPath: string;
  Explosionclon,Flagclon:TSkAnimatedImage;
  FlagSoundPath, ExplosionSoundPath, RevealSoundPath: string;
  Data: TMemoryStream;
begin
  GridLayout.DeleteChildren;

  Square := Min(GridLayout.Width, GridLayout.Height);
  GridLayout.Align := TAlignLayout.Center;
  GridLayout.Width := Square;
  GridLayout.Height := Square;
  CellSize := (Square - 0.1) / Length(Board);
  GridLayout.ItemHeight := CellSize;
  GridLayout.ItemWidth := CellSize;

  SetLength(CellUIArray, Length(Board), Length(Board[0]));

  BasePath := TPath.GetDirectoryName(ParamStr(0));
  if not Assigned(FlagAnimData) then
  begin
    FlagAnimData := TMemoryStream.Create;
    FlagPath := TPath.Combine(BasePath, 'Assets\sprite\flag.json');
    FlagAnimData.LoadFromFile(FlagPath);
    FlagAnimData.Position:=0;
  end;

  if not Assigned(ExplosionAnimData) then
  begin
    ExplosionAnimData := TMemoryStream.Create;
    ExplosionPath := TPath.Combine(BasePath, 'Assets\sprite\explosion.json');
    ExplosionAnimData.LoadFromFile(ExplosionPath);
    ExplosionAnimData.Position:=0;
  end;
    if not Assigned(AudioMgr) then
  begin
    AudioMgr := TAudioManager.Create;
    FlagSoundPath := TPath.Combine(BasePath, 'Assets\sounds\flag.wav');
    ExplosionSoundPath := TPath.Combine(BasePath, 'Assets\sounds\explosion.wav');
    RevealSoundPath := TPath.Combine(BasePath, 'Assets\sounds\reveal.wav');
    AudioMgr.AddSound(FlagSoundPath);
    AudioMgr.AddSound(ExplosionSoundPath);
    AudioMgr.AddSound(RevealSoundPath);
  end;

  for I := Low(Board) to High(Board) do
    for J := Low(Board[I]) to High(Board[I]) do
    begin
      CellUI := TCellUI.Create;
      CellUI.Rectangle := TRectangle.Create(GridLayout);
      CellUI.Rectangle.Parent := GridLayout;
      CellUI.Rectangle.OnMouseDown := Form1.OnCellMouseDown;
      CellUI.Rectangle.TagString := IntToStr(I) + ',' + IntToStr(J);
      CellUI.Rectangle.Stroke.Thickness := 0;
      CellUI.Rectangle.Stroke.Kind := TBrushKind.None;
      if GetCellColor(I, J) then
        CellUI.Rectangle.Fill.Color := $FFa2d149
      else
        CellUI.Rectangle.Fill.Color := $FFaad751;

      CellUI.hintNum := TLabel.Create(CellUI.Rectangle);
      CellUI.hintNum.Parent := CellUI.Rectangle;
      CellUI.hintNum.StyledSettings := [];
      CellUI.hintNum.TextSettings.Font.Size := 20;
      CellUI.hintNum.TextSettings.HorzAlign := TTextAlign.Center;
      CellUI.hintNum.TextSettings.VertAlign := TTextAlign.Center;
      CellUI.hintNum.Align := TAlignLayout.Client;

      CellUI.Anim := TSkAnimatedImage.Create(CellUI.Rectangle);
      CellUI.Anim.Parent := CellUI.Rectangle;
      CellUI.Anim.Align := TAlignLayout.Contents;
      CellUI.Anim.Enabled := False;
      CellUI.Anim.Visible := False;

      CellUIArray[I][J] := CellUI;
    end;
end;

class procedure TMinesweeperUIHelper.DisplayCell(const Board: TBoard; X, Y: Integer);
begin
  with CellUIArray[X][Y] do
  begin
    Anim.Visible := False;
    Anim.Animation.Enabled := False;
    hintNum.Text := '';

    case Board[X][Y].State of
      csClosed:
        hintNum.Text := '';
      csFlagged:
        begin
          FlagAnimData.Position := 0;
          if Anim.Source.Data = nil then
          begin
            Anim.LoadFromStream(FlagAnimData);
          end;
          AudioMgr.PlaySound('flag');
          Anim.Animation.StartProgress := 0.0;
          Anim.Animation.StopProgress := 0.5;
          Anim.Visible := True;
          Anim.Animation.Loop := false;
          Anim.Animation.Start;
        end;
      csRevealed:
        begin
          if GetCellColor(X, Y) then
            Rectangle.Fill.Color := $FFd7b899
          else
            Rectangle.Fill.Color := $FFe5c29f;

          case Board[X][Y].Content of
            ccEmpty:  begin
              hintNum.Text := '';
              //AudioMgr.PlaySound('reveal');
            end;
            ccNumber:
              begin
                //AudioMgr.PlaySound('reveal');
                hintNum.Text := IntToStr(Board[X][Y].Number);
                case Board[X][Y].Number of
                  1: hintNum.TextSettings.FontColor := $FF388e3c;
                  2: hintNum.TextSettings.FontColor := $FF1976d2;
                  3: hintNum.TextSettings.FontColor := $FFd47034;
                  4..6: hintNum.TextSettings.FontColor := $FFd43534;
                end;
              end;
            ccMine:
              begin
                ExplosionAnimData.Position := 0;
                if Anim.Source.Data = nil then
                begin

                  Anim.LoadFromStream(ExplosionAnimData);
                end;
                AudioMgr.PlaySound('explosion');
                Anim.Animation.StartProgress := 0.0;
                Anim.Animation.StopProgress := 0.7;
                Anim.Visible := True;
                Anim.Animation.Loop := false;
                Anim.Animation.Start;
              end;
          end;
        end;
    end;
  end;
end;

class procedure TMinesweeperUIHelper.DisplayBoard(const Board: TBoard);
var
  I, J: Integer;
begin
  for I := 0 to Length(Board) - 1 do
    for J := 0 to Length(Board[I]) - 1 do
      DisplayCell(Board, I, J);
end;

class procedure TMinesweeperUIHelper.UpdateCells(const Board: TBoard; const Changed: TArray<TChangedCell>);
var
  cell: TChangedCell;
begin
  for cell in Changed do
    DisplayCell(Board, cell.X, cell.Y);
end;

class procedure TMinesweeperUIHelper.ResetBoardUI(GridLayout: TGridLayout);
begin
  GridLayout.DeleteChildren;
  SetLength(CellUIArray, 0);
end;

class function TMinesweeperUIHelper.CreateEmptyBoard(Rows, Cols: Integer): TBoard;
var
  I: Integer;
begin
  SetLength(Result, Rows);
  for I := 0 to Rows - 1 do
    SetLength(Result[I], Cols);
end;

class function TMinesweeperUIHelper.GetCellColor(I, J: Integer): Boolean;
begin
  Result := (I + J) mod 2 = 0;
end;

end.

