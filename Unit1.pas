unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  System.ImageList, FMX.ImgList, FMX.Objects,System.StrUtils, FMX.Ani,
  System.Skia, FMX.Skia;

type
  TForm1 = class(TForm)
    GridLayout1: TGridLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    ImageList1: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnCellMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);



  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

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
    gsWon: ShowMessage('🎉 win!');
    gsLost: ShowMessage('💣 los!');
  end;
end;


end.
