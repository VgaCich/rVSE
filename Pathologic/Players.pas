unit Players;

interface

uses
  AvL, avlUtils, avlMath, avlVectors, VSECore, Game, GameObjects;

type
  TPlayerBachelor = class(TPlayer)
  private

  public
    constructor Create(Game: TGame);
  end;
  TPlayerHaruspex = class(TPlayer)
  private

  public
    constructor Create(Game: TGame);
  end;
  TPlayerChangeling = class(TPlayer)
  private

  public
    constructor Create(Game: TGame);
  end;
  TPlayerPlague = class(TPlayer)
  private

  public
    constructor Create(Game: TGame);
  end;

implementation

uses VSEMemPak, GameData
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TPlayerBachelor }

constructor TPlayerBachelor.Create;
begin
  FName := SBachelor;
  inherited;

end;

{ TPlayerHaruspex }

constructor TPlayerHaruspex.Create(Game: TGame);
begin
  FName := SHaruspex;
  inherited;

end;

{ TPlayerChangeling }

constructor TPlayerChangeling.Create(Game: TGame);
begin
  FName := SChangeling;
  inherited;

end;

{ TPlayerPlague }

constructor TPlayerPlague.Create(Game: TGame);
begin
  FName := SPlague;
  inherited;

end;

end.
