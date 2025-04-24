unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  System.ImageList, FMX.ImgList, FMX.Objects,System.StrUtils, FMX.Ani,
  System.Skia, FMX.Skia, FMX.Effects;

type
  TForm1 = class(TForm)
    GridLayout1: TGridLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    ImageList1: TImageList;
    Rectangle1: TRectangle;
    RoundRect1: TRoundRect;
    Label1: TLabel;
    RoundRect2: TRoundRect;
    Label2: TLabel;
    RoundRect3: TRoundRect;
    Label3: TLabel;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    BlurEffect1: TBlurEffect;
    BlurEffect2: TBlurEffect;
    BlurEffect3: TBlurEffect;
    ShadowEffect4: TShadowEffect;
    RoundRect4: TRoundRect;
    RoundRect5: TRoundRect;
    LabelTime: TLabel;
    Timer1: TTimer;
    Rectangle2: TRectangle;
    ShadowEffect5: TShadowEffect;
    Rectangle3: TRectangle;
    ShadowEffect9: TShadowEffect;
    score: TLabel;
    BlurEffect4: TBlurEffect;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnCellMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure RoundRect1Click(Sender: TObject);
    procedure RoundRect2Click(Sender: TObject);
    procedure RoundRect3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StartTimer();
    procedure StopTimer();




  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ElapsedSeconds: Integer = 0;
implementation


{$R *.fmx}
uses uMinesweeperLogic,uMinesweeperUI;
var
  board:Tboard;
  Game: TMinesweeper;      //nativ logic

procedure TForm1.Button1Click(Sender: TObject);

begin
  TMinesweeperUIHelper.ResetBoardUI(GridLayout1);
  Game.Initialize(6, 6, 3);
  board:= Game.GetBoard;
  TMinesweeperUIHelper.InitializeBoardUI(board, GridLayout1);
  TMinesweeperUIHelper.DisplayBoard(board);

end;



procedure TForm1.FormCreate(Sender: TObject);
begin
Game := TMinesweeper.Create;
end;

procedure TForm1.OnCellMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var

  Row, Col: Integer;
  Parts: TArray<string>;
 Changed: TArray<TChangedCell>;

begin
  Parts := SplitString((Sender as TRectangle).TagString, ',');
  if Length(Parts) <> 2 then Exit;

  Row := StrToIntDef(Parts[0], -1);
  Col := StrToIntDef(Parts[1], -1);


  if Button = TMouseButton.mbRight then      // flaged
  begin
    Game.Flag(Row, Col);
  end
  else if Button = TMouseButton.mbLeft then   // open
  begin
    Game.Reveal(Row, Col);
  end;


  Changed := Game.GetChangedCells;
  TMinesweeperUIHelper.UpdateCells(board, Changed);



  case Game.GetStatus of
    gsWon:
    begin
    StopTimer();
    Rectangle1.Visible:=true;
    score.Text := 'Score: ' + IntToStr(game.GetScore)+ '🎉 win!';



    end;

    gsLost:
    begin
    StopTimer();
    Rectangle1.Visible:=true;
    score.Text := 'Score: ' + IntToStr(game.GetScore)+'💣 los!';


    end;
  end;
end;


procedure TForm1.RoundRect1Click(Sender: TObject);
begin
  StartTimer();
  Rectangle1.Visible:=false;
  TMinesweeperUIHelper.ResetBoardUI(GridLayout1);
  Game.Initialize(10, 10, 6);
  board:= Game.GetBoard;
  TMinesweeperUIHelper.InitializeBoardUI(board, GridLayout1);
  TMinesweeperUIHelper.DisplayBoard(board);
end;



procedure TForm1.RoundRect2Click(Sender: TObject);
begin
  StartTimer();
  Rectangle1.Visible:=false;
  TMinesweeperUIHelper.ResetBoardUI(GridLayout1);
  Game.Initialize(15, 15, 15);
  board:= Game.GetBoard;
  TMinesweeperUIHelper.InitializeBoardUI(board, GridLayout1);
  TMinesweeperUIHelper.DisplayBoard(board);
end;

procedure TForm1.RoundRect3Click(Sender: TObject);
begin
  StartTimer();
  Rectangle1.Visible:=false;
  TMinesweeperUIHelper.ResetBoardUI(GridLayout1);
  Game.Initialize(15, 15, 25);
  board:= Game.GetBoard;
  TMinesweeperUIHelper.InitializeBoardUI(board, GridLayout1);
  TMinesweeperUIHelper.DisplayBoard(board);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Minutes, Seconds: Integer;
begin
  Inc(ElapsedSeconds);
  Minutes := ElapsedSeconds div 60;
  Seconds := ElapsedSeconds mod 60;
  LabelTime.Text := Format('%.2d:%.2d', [Minutes, Seconds]);
end;

procedure TForm1.StartTimer;
begin
  Timer1.Enabled := False;
  ElapsedSeconds := 0;
  LabelTime.Text := '00:00';
  Timer1.Enabled := True;
end;
procedure TForm1.StopTimer;
begin
  Timer1.Enabled := False;
end;
end.
