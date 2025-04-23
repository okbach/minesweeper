unit uMinesweeperLogic;

interface

uses
  System.SysUtils, System.Types;

type
  TCellContent = (ccEmpty, ccNumber, ccMine);
  TCellState = (csClosed, csFlagged, csRevealed);
  TGameStatus = (gsPlaying, gsWon, gsLost);

  TCell = record
    Content: TCellContent;
    Number: Integer;
    State: TCellState;
  end;
  TBoard = array of array of TCell;


  type
  TChangedCell = record
    X, Y: Integer;
  end;

  TMinesweeper = class
  private
    FBoard: TBoard;
    FStatus: TGameStatus;
    FChangedCells: TArray<TChangedCell>;
    function Valid(x, y: Integer): Boolean;
    procedure PlaceMines(MineCount: Integer);
    procedure CalculateNumbers;
    procedure AddChangedCell(X, Y: Integer);
  public
    procedure Initialize(Rows, Cols, MineCount: Integer);
    procedure Reveal(x, y: Integer);
    procedure Flag(x, y: Integer);
    function IsGameWon: Boolean;
    function DebugState: string;
    function GetBoard: TBoard;
    function GetStatus: TGameStatus;
    function GetChangedCells: TArray<TChangedCell>;
  end;

implementation

{ TMinesweeper }

procedure TMinesweeper.Initialize(Rows, Cols, MineCount: Integer);
var
  x, y: Integer;
begin
  SetLength(FBoard, Rows, Cols);
  FStatus := gsPlaying;

  for x := 0 to Rows - 1 do
    for y := 0 to Cols - 1 do
    begin
      FBoard[x][y].State := csClosed;
      FBoard[x][y].Content := ccEmpty;
      FBoard[x][y].Number := 0;
    end;

  PlaceMines(MineCount);
  CalculateNumbers;
  SetLength(FChangedCells, 0);

end;

function TMinesweeper.Valid(x, y: Integer): Boolean;
begin
  Result := (x >= 0) and (x < Length(FBoard)) and
            (y >= 0) and (y < Length(FBoard[0]));
end;

procedure TMinesweeper.PlaceMines(MineCount: Integer);
var
  Rows, Cols, x, y, Placed: Integer;
begin
  Rows := Length(FBoard);
  Cols := Length(FBoard[0]);
  Randomize;
  Placed := 0;

  while Placed < MineCount do
  begin
    x := Random(Rows);
    y := Random(Cols);
    if FBoard[x][y].Content <> ccMine then
    begin
      FBoard[x][y].Content := ccMine;
      Inc(Placed);
    end;
  end;
end;

procedure TMinesweeper.CalculateNumbers;
var
  x, y, i, j, Rows, Cols, Count: Integer;
begin
  Rows := Length(FBoard);
  Cols := Length(FBoard[0]);

  for x := 0 to Rows - 1 do
    for y := 0 to Cols - 1 do
    begin
      if FBoard[x][y].Content = ccMine then
        Continue;

      Count := 0;
      for i := -1 to 1 do
        for j := -1 to 1 do
          if (i <> 0) or (j <> 0) then
            if Valid(x + i, y + j) then
              if FBoard[x + i][y + j].Content = ccMine then
                Inc(Count);

      if Count > 0 then
      begin
        FBoard[x][y].Content := ccNumber;
        FBoard[x][y].Number := Count;
      end;
    end;
end;

procedure TMinesweeper.Reveal(x, y: Integer);
var
  i, j: Integer;
begin
  if not Valid(x, y) then Exit;
  if FStatus <> gsPlaying then Exit;

  with FBoard[x][y] do
  begin
    if (State = csRevealed) or (State = csFlagged) then Exit;

    State := csRevealed; // open  eveal
    AddChangedCell(x, y);

    if Content = ccMine then
    begin
      FStatus := gsLost;
      Exit;
    end;

    if (Content = ccEmpty) or ((Content = ccNumber) and (Number = 0)) then
    begin
      for i := -1 to 1 do
        for j := -1 to 1 do
          if not ((i = 0) and (j = 0)) then
            Reveal(x + i, y + j);
    end;
  end;

  if IsGameWon then
    FStatus := gsWon;
end;

procedure TMinesweeper.Flag(x, y: Integer);
begin
  if not Valid(x, y) then Exit;
  if FStatus <> gsPlaying then Exit;

  with FBoard[x][y] do
  begin
    if State = csRevealed then Exit;

    if State = csFlagged then
      State := csClosed
    else
      State := csFlagged;


    AddChangedCell(x, y);
  end;
end;

function TMinesweeper.IsGameWon: Boolean;
var
  x, y: Integer;
begin
  for x := 0 to High(FBoard) do
    for y := 0 to High(FBoard[0]) do
      with FBoard[x][y] do
      begin
        if (Content <> ccMine) and (State <> csRevealed) then
          Exit(False);
      end;
  Result := True;
end;

function TMinesweeper.GetBoard: TBoard;
begin
  Result := FBoard;
end;

function TMinesweeper.GetStatus: TGameStatus;
begin
  Result := FStatus;
end;

function TMinesweeper.DebugState: string;
var
  x, y: Integer;
  cell: TCell;
  row: string;
begin
  Result := '';
  for x := 0 to High(FBoard) do
  begin
    row := '';
    for y := 0 to High(FBoard[0]) do
    begin
      cell := FBoard[x][y];
      case cell.State of
        csClosed:
          row := row + '[ ]';
        csFlagged:
          row := row + '[F]';
        csRevealed:
          case cell.Content of
            ccEmpty: row := row + '[ ]';
            ccNumber: row := row + '[' + IntToStr(cell.Number) + ']';
            ccMine: row := row + '[*]';
          end;
      end;
    end;
    Result := Result + row + sLineBreak;
  end;
end;



procedure TMinesweeper.AddChangedCell(X, Y: Integer);
var
  idx: Integer;
begin
  idx := Length(FChangedCells);
  SetLength(FChangedCells, idx + 1);
  FChangedCells[idx].X := X;
  FChangedCells[idx].Y := Y;
end;

function TMinesweeper.GetChangedCells: TArray<TChangedCell>;
begin
  Result := FChangedCells;
  SetLength(FChangedCells, 0);
end;

end.

