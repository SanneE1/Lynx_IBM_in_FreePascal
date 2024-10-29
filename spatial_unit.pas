
unit spatial_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Math, LCLType;

type           {here you declare the data structure for you individuals}
  Array2Dbyte = array of array of byte;
  Array3Dbyte = array of array of array of byte;

  PAgent = ^Agent;

  Agent = record
    sex: string[1];
    Age: byte;         // In years
    Status: shortint;  // 0=pre-dispersal cubs and subadults, 1=dispersing individuals, 2=early settled adults, 3 = fully settled adults

    Coor_X: byte;
    Coor_Y: byte;

    TerritoryX: array of integer;
    TerritoryY: array of integer;

    DailySteps: byte;
    DailyStepsOpen: byte;
    mov_mem: byte;
    return_home: boolean;  // not to be confused with Territory. This is for when individuals go on an excursion
    // into Open habitat, for them to return to the last known Dispersal habitat they've visited
    homeX: byte;
    homeY: byte;

  end;

  { Tspatial_Form }

  Tspatial_Form = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Exit_Button: TButton;
    Abort_Button: TButton;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Run_Button: TButton;
    procedure Abort_ButtonClick(Sender: TObject);
    procedure Exit_ButtonClick(Sender: TObject);
    procedure Run_ButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Pop_dynamics;
  end;

var               {here you declare global variables}
  spatial_Form: Tspatial_Form;
  Population: TList;
  Individual: PAgent;
  n_ini: integer;
  max_years: integer;
  rep_prob: real;
  AlphaR: real;
  BetaR: real;
  surv_prob: real;
  AlphaS: real;
  BetaS: real;
  sett_prob: real;
  avg_steps: integer;
  AlphaD: real;
  BetaD: real;
  sink: real;
  populationsize: longint;
  n_sim: integer;
  n_extint: integer;
  sum_distance_X: integer;
  sum_distance_Y: integer;
  current_sim: integer;
  max_pop_size: integer;
  step_probs: array of double;
  pop_size: array[1..100] of integer;
  sum_pop_size: array[1..100] of integer;
  n_sim_no_ext: array[1..100] of integer;
  to_file_out: TextFile;
  filename: Text;
  HabitatMap: Array2Dbyte;
  MalesMap: Array3Dbyte;
  FemalesMap: Array3Dbyte;
  Mapdimx, Mapdimy: integer;
  mapname, paramname: string;
  dx: array[0..8] of integer = (0, 0, 1, 1, 1, 0, -1, -1, -1);
  dy: array[0..8] of integer = (0, 1, 1, 0, -1, -1, -1, 0, 1);
  xp, yp: integer;
  tempX, tempY: integer;
  homeX, homeY, steps, s, new_dir, mem: integer;
  tohome: boolean;

  {Vital rate variables}
  min_rep_age, min_rep_age_m, max_rep_age, max_age: integer;
  Tsize: integer;
  litter_size, litter_size_sd, rep_prob_oNP, rep_prob_iNP: real;
  surv_cub_iNP, surv_cub_oNP, surv_sub_iNP, surv_sub_oNP, surv_resident_iNP, surv_resident_oNP, surv_disperse_iNP, surv_disperse_oNP, surv_disp_rho, surv_old_iNP, surv_old_oNP: real;
  alpha_steps: real;
  theta_d, theta_delta, delta_theta_long, delta_theta_f, L, N_d, beta, gamma: real;

const             {here you declare constants}

  file_name = 'output.txt';

  max_steps = 100;

implementation

{$R *.lfm}

{ Tspatial_Form }

function randomPoisson(mean: integer): integer;
  {pseudorandom Poisson distributed number genetrator, Donald Knuth's algorithm}
const
  RESOLUTION = 1000;
var
  k: integer;
  b, l: real;
begin
  //assert(mean > 0, 'mean < 1');
  k := 0;
  b := 1;
  l := exp(-mean);
  while b > l do
  begin
    k := k + 1;
    b := b * random(RESOLUTION) / RESOLUTION;
  end;
  if mean <= 0 then randomPoisson := 0
  else
    randomPoisson := k - 1;
end;

procedure ArrayToNegOne(var arr: array of integer);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    arr[i] := -1;
end;

procedure ShowErrorAndExit(const errMsg: string);
begin
  {$IFDEF WINDOWS}
  if IsConsole then
    Writeln('Error: ', errMsg)  // Console error output
  else
    MessageDlg('Error: ' + errMsg, mtError, [mbOK], 0); // GUI error dialog
  {$ELSE}
  Writeln('Error: ', errMsg);  // For non-Windows systems or always CLI
  {$ENDIF}

  Halt(1);  // Exit the program with a non-zero code to indicate error
end;

procedure ReadMap(mapname: string);
var
  ix, iy, Value: integer;
begin
  Assign(filename, mapName);
  reset(filename);
  readln(filename, Mapdimx, Mapdimy);
  SetLength(HabitatMap, Mapdimx + 1, Mapdimy + 1);
  SetLength(MalesMap, Mapdimx + 1, Mapdimy + 1, 2);
  SetLength(FemalesMap, Mapdimx + 1, Mapdimy + 1, 2);

  for iy := 1 to Mapdimy do
  begin
    begin
      for ix := 1 to Mapdimx do
      begin
        Read(filename, Value);
        // HabitatMap (and the others) are 'byte' types which is less memory
        // intensive than an integer, but that does mean it can only deal with 0:255
        // There are values in the map of -9999 that represent the sea. As its the same as a barrier, here set to 0!
        if Value < 0 then HabitatMap[ix, iy] := 0
        else
          HabitatMap[ix, iy] := Value;
      end;
    end;
    readln(filename);
  end;

  Close(filename);
end;

procedure ReadParameters(paramname: string);
var
  par_seq: array[1..28] of string;
  val_seq: array of real;
  r, spacePos: integer;
  a, param: string;
  value: real;
begin
  {This function is probably much longer than it needs to be. I just need to make absolutely sure
  that if I at some point change or mess with the param file, I get a warning here, so
  I don't accedentily work with parameter values in the wrong variable!}

   par_seq[1]:= 'min_rep_age';
   par_seq[2]:= 'max_rep_age';
   par_seq[3]:= 'max_age';
   par_seq[4]:= 'Tsize';
   par_seq[5]:= 'litter_size';
   par_seq[6]:= 'litter_size_sd';
   par_seq[7]:= 'rep_prob_iNP';
   par_seq[8]:= 'rep_prob_oNP';
   par_seq[9]:= 'surv_cub_iNP';
   par_seq[10]:= 'surv_cub_oNP';
   par_seq[11]:= 'surv_sub_iNP';
   par_seq[12]:= 'surv_sub_oNP';
   par_seq[13]:= 'surv_resident_iNP';
   par_seq[14]:= 'surv_resident_oNP';
   par_seq[15]:= 'surv_disperse_iNP';
   par_seq[16]:= 'surv_resident_oNP';
   par_seq[17]:= 'surv_disp_rho';
   par_seq[18]:= 'surv_old_iNP';
   par_seq[19]:= 'surv_old_oNP';
   par_seq[20]:= 'alpha_steps';
   par_seq[21]:= 'theta_d';
   par_seq[22]:= 'theta_delta';
   par_seq[23]:= 'delta_theta_long';
   par_seq[24]:= 'delta_theta_f';
   par_seq[25]:= 'L';
   par_seq[26]:= 'N_d';
   par_seq[27]:= 'beta';
   par_seq[28]:= 'gamma';

   SetLength(val_seq, High(par_seq)+1);

   Assign(filename, paramname);
   reset(filename);

   for r:=1 to High(par_seq) do
   begin
     readln(filename, a);
     // Find the first space to split the string
    spacePos := Pos(' ', a);

    if spacePos > 0 then
    begin
      // Extract parameter name and convert the rest to a real
      param := Copy(a, 1, spacePos - 1);                      // Get parameter name
      Val(Trim(Copy(a, spacePos + 1, Length(a))), value);     // Convert value part to real - any integers are converted below to correct type

    if (param = par_seq[r]) then
     val_seq[r] := value
   else
   // stop program and get error message that parameter name not expected
   ShowErrorAndExit('One of the parameter names is not as expected. Check parameter file');
   end
    else ShowErrorAndExit('No space found. Check parameter file');
   end;

   min_rep_age        := Round(val_seq[1]);
   max_rep_age        := Round(val_seq[2]);
   max_age            := Round(val_seq[3]);
   Tsize              := Round(val_seq[4]);
   litter_size        := val_seq[5];
   litter_size_sd     := val_seq[6];
   rep_prob_iNP       := val_seq[7];
   rep_prob_oNP       := val_seq[8];
   surv_cub_iNP       := val_seq[9];
   surv_cub_oNP       := val_seq[10];
   surv_sub_iNP       := val_seq[11];
   surv_sub_oNP       := val_seq[12];
   surv_resident_iNP  := val_seq[13];
   surv_resident_oNP  := val_seq[14];
   surv_disperse_iNP  := val_seq[15];
   surv_disperse_oNP  := val_seq[16];
   surv_disp_rho      := val_seq[17];
   surv_old_iNP       := val_seq[18];
   surv_old_oNP       := val_seq[19];
   alpha_steps        := val_seq[20];
   theta_d            := val_seq[21];
   theta_delta        := val_seq[22];
   delta_theta_long   := val_seq[23];
   delta_theta_f      := val_seq[24];
   L                  := val_seq[25];
   N_d                := val_seq[26];
   beta               := val_seq[27];
   gamma              := val_seq[28];

















end;

procedure UpdateAbundanceMap;
// Update the availability of males + local abundances. NOTE: DOESN'T CHECK IF MALE IS OF REPRODUCTIVE AGE
var
  a, b, c, x, y: integer;
  s: string;
begin

  for a := 0 to MapdimX - 1 do
    for b := 0 to Mapdimy - 1 do
      for c := 0 to 1 do           // where 0 is status, 1 is age
    begin
      Malesmap[a, b, c] := 0;      // Empty maps to fill with status and age below
      Femalesmap[a, b, c] := 0;
    end;


  with population do
  begin
    for a := 0 to populationsize - 1 do
    begin
      Individual := Items[a];
      if Individual^.Status >=2 then
      begin
        for b := 0 to length(Individual^.TerritoryX) - 1 do
        begin
          x := Individual^.TerritoryX[b];
          y := Individual^.TerritoryY[b];
          s := Individual^.sex;

          if ((x < 0) and (y < 0)) or ((x > MapdimX) or (y > MapdimY)) then
            Continue;

          if (s = 'f') then
            begin
            Femalesmap[x, y, 0] := Individual^.Status;
            Femalesmap[x, y, 1] := Individual^.Age;
            end;
          if (s = 'm') then
            begin
            Malesmap[x, y, 0] := Individual^.Status;
            Malesmap[x, y, 1] := Individual^.Age;
            end;
        end;
      end;
    end;
  end;

end;

procedure WriteMapCSV(filename: string; var arrayData: Array3Dbyte; dimx, dimy, dimz: integer);
var
  ix, iy: integer;
  outfile: Text;
begin
  Assign(outfile, filename);
  rewrite(outfile);

  // Loop over the arrayData and write each element to the CSV
  for iy := 1 to dimy do
  begin
    for ix := 1 to dimx do
    begin
      // Write each value, followed by a comma, except for the last value in the row
      if ix < dimx then
        Write(outfile, arrayData[ix, iy, dimz], ',')
      else
        Write(outfile, arrayData[ix, iy, dimz]);  // No comma at the end of the row
    end;
    writeln(outfile);  // Move to the next line in the CSV file
  end;

  Close(outfile);
end;

procedure WritePopulationToCSV(const population: TList; const filename: string);
var
  csvFile: TextFile;
  i, j: integer;
begin
  AssignFile(csvFile, filename);
  Rewrite(csvFile);

  // Write header
  WriteLn(csvFile, 'Sex,Age,Status,Coor_X,Coor_Y,Territory_X,Territory_Y');

  // Write data for each individual
  for i := 0 to population.Count - 1 do
  begin
    individual := PAgent(population[i]);

    // Write individual information
    Write(csvFile, individual^.sex, ',');
    Write(csvFile, individual^.Age, ',');
    Write(csvFile, individual^.Status, ',');
    Write(csvFile, individual^.Coor_X, ',');
    Write(csvFile, individual^.Coor_Y, ',');

    // Write territory coordinates
    for j := 0 to length(individual^.TerritoryX) - 1 do
    begin

      Write(csvFile, Individual^.TerritoryX[j], ',');
      Write(csvFile, individual^.TerritoryY[j]);

      // Add comma if not last coordinate
      if j < length(individual^.TerritoryX) - 1 then
        Write(csvFile, ',');
    end;

    WriteLn(csvFile); // End of current individual's data
  end;

  CloseFile(csvFile);
end;


function NSteps(step_probs: array of double): integer;
var
  r: double;
  s: integer;
begin

  r := Random;

  for s := 0 to length(step_probs) - 1 do
    if r > step_probs[s] then
    begin
      Result := s + 1;
      // Remember 1 step is indexed as 0 in step_prob array (in other words, that array starts at 0 not 1)
      Break;
    end;

end;

procedure Step_probabilities;
var
  steps: integer;
begin

  SetLength(step_probs, max_steps);
  for steps := 0 to max_steps - 1 do
    step_probs[steps] := (1 / (1 + alpha_steps * Power(steps, 3)));

end;


function CanMoveHere(mem: integer): boolean;
begin
  // outside of map dimensions
  if (xp + dx[mem] < 0) or (xp + dx[mem] > Mapdimx) or (yp + dy[mem] < 0) or (yp + dy[mem] > Mapdimy) then Result := False
  else
  // barrier cell
    if (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 0) then Result := False
    else
      Result := True;
end;

Function inPark(x,y:integer):boolean;

begin
  Result:=False;   // false = not in park, true = in NP
  {if ((x>=89) and (x<=97)) then
    if ((y>=71) and (y<=77)) then Result:=True;
  if ((x>=71) and (x<=113)) then
    if ((y>=3) and (y<=71)) then Result:=True;}

  if ((x >= 122) and (x <= 135)) then
  if((y >= 85) and (y <= 115)) then Result:=True;

  if ((x >= 140) and (x <= 147)) then
  if ((y >=140) and (y <= 146)) then Result:= True;

  if ((x >= 135) and (x <= 141)) then
  if ((y >= 75) and (y <= 82)) then Result:= True;


end;

function MoveDir: integer;
var
  nOpen, nDisp, nBarr, i, h, f: integer;
  fragmented: boolean;
  theta, P_d, P_o, dist_min, dist_act: real;
  p: double;
begin

  new_dir := 99;
  mem := Individual^.mov_mem;
  tohome := Individual^.return_home;
  homeX := Individual^.homeX;
  homeY := Individual^.homeY;

  {Set base autocorrelation on either short distance or long}
  if (steps / 10.5 > L) then
    theta := theta_d + delta_theta_long
  else
    theta := theta_d;

  {Check if individual is in open habitat, and if so, model probability
  to return from an excursion}
  if (HabitatMap[xp, yp] = 1) and (tohome = False) then
  begin
    P_d := (10.5 * (s / steps)) * gamma;
    if random < P_d then
      tohome := True;
  end;

  {Look around}
  nOpen := 0;
  nDisp := 0;
  nBarr := 0;

  for i := 1 to 8 do
  begin
    if CanMoveHere(i) then
    begin
      if (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 1) then
        nOpen := nOpen + 1
      else
        if (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 2) then
          nDisp := nDisp + 1;
    end
    else
      NBarr := NBarr + 1;
    ;
  end;

  {Determine if surroundings is fragmented}
  if nDisp < N_d then fragmented := True
  else
    fragmented := False;

  p := random;

  if tohome = True then
  begin
    {return to last dispersal location if no dispersal habitat is in sight}
    if nDisp = 0 then
    begin
      dist_min := 1000;
      for i := 1 to 8 do
      begin
        if CanMoveHere(i) then
          dist_act := sqrt(sqr((xp + dx[i]) - homeX) + sqr((yp + dy[i]) - homeY))
        else
          dist_act := 1001;
        if dist_act <= dist_min then
        begin
          dist_min := dist_act;
          new_dir := i;
        end;
      end;
    end
    else
  {Move to the nearby dispersal cell if there's any in sight.
  Consider autocorrelatin in movement - add to autocorrelation in fragmented area}
    begin
      if fragmented = True then theta := theta + delta_theta_f;
      if CanMoveHere(mem) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 2) and (p < (theta)) then
        new_dir := mem
      else
      begin
        h := random(nDisp) + 1;
        f := 0;
        for i := 1 to 8 do
        begin
          if CanMoveHere(i) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 2) then
          begin
            f := f + 1;
            if f = h then
            begin
              new_dir := i;
              Break;
            end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    if fragmented = False then
    begin
      {movement in non-fragmented area}
      if (p < theta) and CanMoveHere(mem) then new_dir := mem
      else
        if f < (theta + theta * theta_delta) then
        begin
          if (mem > 4) and CanMoveHere(mem - 4) then new_dir := mem - 4
          else
            if (mem > 0) and (mem <= 4) and CanMoveHere(mem + 4) then new_dir := mem + 4;
        end;
      if new_dir > 10 then  // If new direction has not yet been assigned through autocorrelation, random movement
      begin
        h := random(nDisp + nOpen) + 1;
        f := 0;
        for i := 1 to 8 do
        begin
          if CanMoveHere(i) then
          begin
            f := f + 1;
            if f = h then
            begin
              new_dir := i;
              Break;
            end;
          end;
        end;
      end;
    end
    else
    begin
      {movement in fragmented area}
      theta := theta + delta_theta_f;
      P_o := (1 / (nOpen + nDisp)) * beta;
      if ((random < P_o) and (nOpen > 0)) or (nDisp = 0) then
        {moving to open habitat}
      begin
    {If memory movement is same type as chosen habitat type (here Open) then use
    autocorrelation to see if ind. moves in memory direction.}
        if CanMoveHere(mem) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 1) and (p < theta) then
          new_dir := mem
        else
        if (new_dir > 10) and (nOpen > 0) then
        begin
          h := random(nOpen) + 1;
          f := 0;
          for i := 1 to 8 do
          begin
            if CanMoveHere(i) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 1) then
            begin
              f := f + 1;
              if f = h then
              begin
                new_dir := i;
                Break;
              end;
            end;
          end;
        end;
      end
      else
      begin
        {Move to dispersal habitat}
   {If memory movement is same type as chosen habitat type (here Dispersal) then
   use autocorrelation to see if ind. moves in memory direction.}
        if CanMoveHere(mem) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 2) and (p < theta) then
          new_dir := mem
        else
        {probability of moving backwards to autocorrelation}
          if (p < (theta + theta * theta_delta)) then
          begin
            if (mem = 0) and CanMoveHere(mem) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 2) then
              new_dir := mem
            else
              if (mem > 4) and CanMoveHere(mem - 4) and (HabitatMap[(xp + dx[mem - 4]), (yp + dy[mem - 4])] = 2) then
                new_dir := mem - 4
              else
                if (mem > 0) and (mem <= 4) and CanMoveHere(mem + 4) and (HabitatMap[(xp + dx[mem + 4]), (yp + dy[mem + 4])] = 2) then
                  new_dir := mem + 4;
          end;
        {Otherwise a random choise of Dispersal habitat cells}
        if (new_dir > 10) and (nDisp > 0) then
        begin
          h := random(nDisp) + 1;
          f := 0;

          for i := 1 to 8 do
          begin
            if CanMoveHere(i) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 2) then
            begin
              f := f + 1;
              if f = h then
              begin
                new_dir := i;
                Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  if new_dir > 10 then
    Exit;
  if HabitatMap[(xp + dx[new_dir]), (yp + dy[new_dir])] < 1 then
    Exit;
  if CanMoveHere(new_dir) = False then
    Exit;

  Result := new_dir;
end;


function FindCompetition(population: TList; targetSex: string; targetX, targetY: word): PAgent;
var
  i, j, x, y: integer;
  temp: PAgent;
begin
  Result := nil;
  with population do
  begin
    for i := 0 to population.Count - 1 do
    begin
      temp := Items[i];
      if temp^.Sex = targetSex then
      begin
        for j := 0 to length(temp^.TerritoryX) - 1 do
        begin
          x := temp^.TerritoryX[j];
          y := temp^.TerritoryY[j];

          if (x = targetX) and (y = targetY) then
          begin
            Result := temp;
            Exit;   // Match found
          end;
        end;
      end;
    end;
  end;

end;

function Fight(AgeDisperser, AgeEarlySettler: integer; Sex: string): boolean;
  // Return 1 means "win" (Dispersal wins), 0 means lost (Settler wins)
var
  ageseq: array of integer;
  rank_disp, rank_settler, i: integer;
begin

  SetLength(ageseq, 0); // Initialize the dynamic array

  {Set age priority}
  if sex = 'f' then
  begin
    SetLength(ageseq, 9);
    ageseq[1] := 4;
    ageseq[2] := 5;
    ageseq[3] := 6;
    ageseq[4] := 7;
    ageseq[5] := 3;
    ageseq[6] := 2;
    ageseq[7] := 8;
    ageseq[8] := 9;
  end
  else
  begin
    SetLength(ageseq, 8);
    ageseq[1] := 4;
    ageseq[2] := 5;
    ageseq[3] := 6;
    ageseq[4] := 7;
    ageseq[5] := 3;
    ageseq[6] := 8;
    ageseq[7] := 9;
  end;
  {Get ranking of both individuals}
  rank_disp := 10;
  rank_settler := 10;

  for i := 1 to Length(ageseq) - 1 do
  begin
    if AgeDisperser = ageseq[i] then rank_disp := i;
    if AgeEarlySettler = ageseq[i] then rank_settler := i;
  end;

  {See which has the best ranking - if equal ranking, the earlier settled individuals "wins"}
  if rank_disp < rank_settler then Result := True
  else
    Result := False;

end;


Function ReproductionQuality(x,y:integer):boolean;

begin
  Result:=False;

  {if ((x>=70) and (x<=95)) then
  if ((y>=35) and (y<=74)) then Result:= True;{reserva - 1}

  {if ((x>=71) and (x<=94)) then
  if ((y>=35) and (y<=62)) then Result:= True;{reserva - 1}}

  if ((x>=89) and (x<=97)) then
  if ((y>=71) and (y<=77)) then Result:= True;{cotorey - 2}

  if ((x>=100)and (x<=106))then
  if ((y>=11) and (y<=20)) then Result:= True;{marismillas - 3}

  if ((x>=60) and (x<=71)) then
  if ((y>=53) and (y<=71)) then Result:= True;{acebuche - 4}

  if ((x>=8)  and (x<=14)) then
  if ((y>=87) and (y<=93)) then Result:= True;{moguer - 5}

  if ((x>=109)and (x<=113))then
  if ((y>=82) and (y<=86)) then Result:= True;{hatoraton - 6}

  if ((x>=100)and (x<=104))then
  if ((y>=114)and (y<=119))then Result:= True;{torrecuadros - 7}

  if ((x>=132)and (x<=137))then
  if ((y>=95) and (y<=99)) then Result:= True;{puebla - 8}



  if ((x>=14)and (x<=20))then
  if ((y>=74) and (y<=80)) then Result:= True;{mazagon - 9}

  if ((x>=35)and (x<=50))then
  if ((y>=88) and (y<=105)) then Result:= True;{bonares - 10}

  if ((x>=75)and (x<=82))then
  if ((y>=70) and (y<=77)) then  Result:= True;{rocina - 11}



  if ((x>=0)and (x<=0))then
  if ((y>=0) and (y<=0)) then  Result:= True;{arrayangato - 12}

  if ((x>=0)and (x<=0))then
  if ((y>=0) and (y<=0)) then  Result:= True;{tojal del aguila - 13}

  if ((x>=0)and (x<=0))then
  if ((y>=0) and (y<=0)) then  Result:= True;{sotos - 14} }

  if ((x >= 122) and (x <= 135)) then
  if((y >= 85) and (y <= 115)) then Result:=True;

  if ((x >= 140) and (x <= 147)) then
  if ((y >=140) and (y <= 146)) then Result:= True;

  if ((x >= 135) and (x <= 141)) then
  if ((y >= 75) and (y <= 82)) then Result:= True;

  if ((x >= 106) and (x <= 114)) then
  if ((y >=89) and (y <= 96)) then Result:= True;

  if ((x >= 68) and (x <= 72)) then
  if ((y >= 53) and (y <= 62)) then Result:= True;


end;


procedure Reproduction;
var
  a, current_litter_size, ls, xy: integer;
  rep_prob: real;
  temp_X, temp_Y, Temp_mem: word;
  male_present: boolean;
begin
  with population do
  begin
    populationsize := population.Count;
    if (populationsize > 1) then
      for a := 0 to populationsize - 1 do
      begin
        Individual := Items[a];
        if Individual^.Sex = 'm' then Continue;

        male_present:= false;

        //Might need this later for density dependence
        //Localpop:=Abundancemap[Individual^.Coor_X,Individual^.Coor_Y]; // get local population size

        {Figure out in if the individual is in a NP}
        if inPark(Individual^.Coor_X, Individual^.Coor_Y) then
          rep_prob := rep_prob_iNP
        else rep_prob := rep_prob_oNP;

        {Check that the individual is capable of reproduction}
          if Individual^.status = 3 then
            // Individual is settled
            if Individual^.age >= min_rep_age then
              if Individual^.age <= max_rep_age then
              // Check that there is a local male
              begin
                for xy := 0 to length(Individual^.TerritoryX)-1 do
                begin
                  if Individual^.TerritoryX[xy] = -1 then Continue;
                    if Malesmap[Individual^.TerritoryX[xy], Individual^.TerritoryY[xy], 0] >=2 then
                    male_present := true;
                end;

                if male_present then
                  if random < rep_prob then
                  begin
                    current_litter_size := Round(randg(litter_size, litter_size_sd));

                    //Save location of the mother, to give to offspring
                    Temp_X := Individual^.Coor_X;
                    Temp_Y := Individual^.Coor_Y;
                    Temp_mem := Individual^.mov_mem;

                    {Create a number of new individuals}
                    for ls := 1 to current_litter_size do
                    begin

                      New(Individual);
                      Individual^.age := 0;
                      if random < 0.5 then Individual^.sex := 'f'
                      else
                        Individual^.sex := 'm';
                      Individual^.status := 0;
                      Individual^.Coor_X := Temp_X;
                      Individual^.Coor_Y := Temp_Y;

                      setLength(Individual^.TerritoryX, Tsize);
                      setLength(Individual^.TerritoryY, Tsize);
                      ArrayToNegOne(Individual^.TerritoryX);
                      ArrayToNegOne(Individual^.TerritoryY);

                      Individual^.mov_mem := Temp_mem;
                      Individual^.homeX := Individual^.Coor_X;
                      Individual^.homeY := Individual^.Coor_Y;
                      Individual^.return_home := False;

                      Individual^.DailySteps := 0;
                      Individual^.DailyStepsOpen := 0;

                      Population.add(Individual);
                    end;
                  end;
              end;
      end;
  end;
end;

procedure Survival;
var
  a, b: integer;
  surv_p, surv_day, daily_mortality_p: real;
  die: boolean;

  //temp_X,temp_Y:word;
begin

  with population do
  begin
    populationsize := population.Count;
    for a := populationsize - 1 downto 0 do  //the index in the list starts at 0
    begin
      Individual := items[a];

      surv_p := -1;

      {Assign yearly survival probabilities}
      if (Individual^.Status = 1) then
      begin
        if inPark(Individual^.Coor_X, Individual^.Coor_Y) then  surv_p:= surv_disperse_iNP
        else surv_p := surv_disperse_oNP
      end
      else
        if inPark(Individual^.Coor_X, Individual^.Coor_Y) then
        begin
          if (Individual^.Status = 0) and (Individual^.Age = 0) then
            surv_p := surv_cub_iNP
            else
          // the status statement shouldn't be necessary (cubs shouldn't be able to have another status but just to make sure)
          if (Individual^.Status = 0) and (Individual^.Age > 0) then
            surv_p := surv_sub_iNP
            else
          if (Individual^.Status > 1) and (Individual^.Age <= max_rep_age) then
            surv_p := surv_resident_iNP
            else
          if (Individual^.Age > max_rep_age) then surv_p := surv_old_iNP;
        end
        else
        begin
            if (Individual^.Age = 0) and (Individual^.Status = 0) then
              surv_p := surv_cub_oNP
            else
              if (Individual^.Age > 0) and (Individual^.Status = 0) then
                surv_p := surv_sub_oNP
              else
                if (Individual^.Status > 1) and (Individual^.Age <= max_rep_age) then
                  surv_p := surv_resident_oNP
                else
                  if (Individual^.Age > max_rep_age) then surv_p := surv_old_oNP;
          end;


      {Transform annual survival (surv_p) to daily survival (surv_day)}
        surv_day := Power(surv_p, (1 / 365));
        if (Individual^.Status = 1) then
      begin
       daily_mortality_p := (1-surv_day) + ((1-surv_day) * surv_disp_rho * (Individual^.DailyStepsOpen / Individual^.DailySteps));
       surv_day := 1-daily_mortality_p;
      end
    else
    begin

      end;

      {Determine fate of individuals}
      die := False;
      if Individual^.age > max_age then die := True
      else
        if random > surv_day then die := True;
      if die then
      begin
       if Individual^.Status >= 2 then
       for b := 0 to length(Individual^.TerritoryX) - 1 do
          begin
            if (Individual^.TerritoryX[b] = -1) then Continue;
            if Individual^.Sex = 'f' then
            begin
            FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0]:= 0;
            FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 1]:= 0;
            end
            else
            begin
            MalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0]:= 0;
            MalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 1]:= 0;
            end;
            Individual^.TerritoryX[b] := -1;
            Individual^.TerritoryY[b] := -1;
          end;
        Delete(a);
      end;
    end;
  end;
end;

procedure Dispersal(day: integer);
var
  a, b, c, d, e, f, g, i, j, new_dir, TestCoordX, TestCoordY, TCount, first_Tcount, FCount, Bcount, xi, yi, xy, coordX, coordY, competitor_age : integer;
  age_m, P_disp_start: real;
  temp_terrX, temp_terrY: array of integer;
  temp_ind, check_status: PAgent;
  Iwin, test_cell_available, c_available, already_terr: boolean;

begin

  with population do
  begin
    populationsize := population.Count;
    for a := 0 to populationsize - 1 do

    begin
      Individual := items[a];

      {If the individual is a subadult determine if it starts dispersing}
      if (Individual^.Status = 0) and (Individual^.Age > 0) then
      begin
        age_m := (Individual^.Age * 12) + (day / 30);
        // Formula requires age in months
        P_disp_start := -1.55 + 2.62 * (1 - Exp(-0.115 * age_m));
        // Calculate probability of dispersing -> higher prob, more likely to start disp.
        if random <= P_disp_start then Individual^.Status := 1;
        // Change status to dispersing if random <= to p_disp_start
      end;

      {restart dispersal in case newly settled individuals do not have enough territory}
      if (Individual^.Status = 2) then
      begin
        TCount := 0;
        FCount := 0;

        for b := 0 to length(Individual^.TerritoryX) - 1 do
        begin
          if (Individual^.TerritoryX[b] > -1) and (Individual^.TerritoryY[b] > -1) then
          begin
            Inc(TCount);
            if (Individual^.Sex = 'm') and (FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0] > 0) then
              Inc(FCount);
          end;
        end;
        {Reset territory information to empty if there's not enough territory}
        if (TCount < Tsize) and ((Individual^.Sex = 'f') or ((Individual^.Sex = 'm') and (FCount < 1))) then
        begin
          for b := 0 to length(Individual^.TerritoryX) - 1 do
          begin
            if (Individual^.TerritoryX[b] = -1) then Continue;   // If the territory is already set to -1 then it's been 'taken away' already, and we don't need to update the info below
            if Individual^.Sex = 'f' then
            begin
            FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0]:= 0;
            FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 1]:= 0;
            end
            else
            begin
            MalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0]:= 0;
            MalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 1]:= 0;
            end;
            Individual^.TerritoryX[b] := -1;
            Individual^.TerritoryY[b] := -1;
          end;
          Individual^.Status := 1;
        end;
      end;


      {Now start dispersal IF individual has dispersal status}

      { #todo : Include a step to check if the current location is breeding habitat that is free -
      Survival happens after this, so it is possible for an individual to be in breeding habitat
      that it couldn't take over in the previous day, but can now. In that case it doesn't make sense to move }

      if (Individual^.Status = 1) then
      begin
        SetLength(temp_terrX, Tsize);
        SetLength(temp_terrY, Tsize);
        ArrayToNegOne(temp_terrX);
        ArrayToNegOne(temp_terrY);

        {This next section is weird, I know... However, for some reason the NSteps function would sometimes randomly produce
        a number of steps around the 66 milion.... This fixes it so *shrug*}
        steps := 200;
        while steps > 100 do steps := NSteps(step_probs);

        Individual^.DailySteps:= steps;
        Individual^.DailyStepsOpen := 0;
        s := 1;

        while s <= steps do
        begin

          {Reset variables}
          xp := Individual^.Coor_X;
          yp := Individual^.Coor_Y;
          new_dir := -1;
          TestCoordX := -1;
          TestCoordY := -1;

          {Calculate new movement direction}
          new_dir := MoveDir;

          {Calculate coordinates to move to}
          TestCoordX := xp + dx[new_dir];
          TestCoordY := yp + dy[new_dir];

          {update home location if individual moves from dispersal to open habitat}
          if (HabitatMap[xp, yp] = 2) and (HabitatMap[TestCoordX, TestCoordY] = 1) then
          begin
            Individual^.homeX := xp;
            Individual^.homeY := yp;
          end;

          {change tohome to false if individual is back in dispersal habitat}
          if (HabitatMap[TestCoordX, TestCoordY] = 2) and tohome = True then
            Individual^.return_home := False;

          {Move individual and update memory}
          Individual^.Coor_X := TestCoordX;
          Individual^.Coor_Y := TestCoordY;
          if (new_dir <> 0) then Individual^.mov_mem := new_dir;

          {Increase daily steps in open, if new coordinates are in an open habitat}
          if HabitatMap[TestCoordX, TestCoordY] = 1 then Individual^.DailyStepsOpen:= Individual^.DailyStepsOpen + 1;


          {If in breeding habitat, check if settlement is possible}
          if (HabitatMap[TestCoordX, TestCoordY] = 2) and (ReproductionQuality(TestCoordX, TestCoordY)) then
          begin

            test_cell_available := False;

            if ((Individual^.sex = 'f') and (Femalesmap[TestCoordX, TestCoordY, 0] = 3)) or
            ((Individual^.sex = 'm') and (Malesmap[TestCoordX, TestCoordY, 0] = 3)) then
              Break
            else
            if ((Individual^.sex = 'f') and (Femalesmap[TestCoordX, TestCoordY, 0] = 0)) or
            ((Individual^.sex = 'm') and (Malesmap[TestCoordX, TestCoordY, 0] = 0) and (Femalesmap[TestCoordX, TestCoordY, 0] = 3)) then
              test_cell_available := True
            else
              if ((Individual^.sex = 'f') and (Femalesmap[TestCoordX, TestCoordY, 0] = 2)) or
              ((Individual^.sex = 'm') and (Malesmap[TestCoordX, TestCoordY, 0] = 2)) then
              begin
                competitor_age := -1;
                if (Individual^.sex = 'f') then competitor_age := Femalesmap[TestCoordX, TestCoordY, 1]
                    else competitor_age := Malesmap[TestCoordX, TestCoordY, 1];
                Iwin := fight(Individual^.Age, competitor_age, Individual^.Sex);
                if Iwin then test_cell_available := True;
              end;

            if test_cell_available then
            begin
              {Look for more breeding habitat until teritory is big enough}

              temp_terrX[0] := TestCoordX;
              temp_terrY[0] := TestCoordY;
              TCount := 1;

                // Walk through all 9 cells and find any available territory
                for i := 1 to 8 do
                  begin
                    c_available := False;
                    competitor_age := -1;
                    xi := TestCoordX + dx[i];
                    yi := TestCoordY + dy[i];
                    if ((HabitatMap[xi, yi] = 2) and (ReproductionQuality(xi, yi))) then
                    begin
                      if ((Individual^.sex = 'f') and (Femalesmap[xi, yi, 0] = 3)) or
                      ((Individual^.sex = 'm') and (Malesmap[xi, yi, 0] = 3)) then
                      Continue
                      else
                      if ((Individual^.Sex = 'f') and (FemalesMap[xi, yi, 0] = 0)) or
                      ((Individual^.Sex = 'm') and (MalesMap[xi, yi, 0] = 0) and (FemalesMap[xi, yi, 0] = 3 )) then
                         c_available := True
                    else
                    if ((Individual^.Sex = 'f') and (FemalesMap[xi, yi, 0] = 2)) or
                       ((Individual^.Sex = 'm') and (MalesMap[xi, yi, 0] = 2) and (FemalesMap[xi, yi, 0] = 3 )) then
                      begin
                       if Individual^.Sex = 'f' then competitor_age := FemalesMap[xi,yi,1] else competitor_age := MalesMap[xi,yi,1];
                        Iwin := fight(Individual^.Age, competitor_age, Individual^.Sex);
                      end;
                    if (c_available) or (Iwin) then
                    begin
                      temp_terrX[TCount] := xi;
                      temp_terrY[TCount] := yi;

                      Inc(TCount);


                      if TCount = Tsize then Break;
                      end;
                    end;

                  end;

                {Keep looking in adjacent cells if not enough territory has been found yet}
                if TCount < Tsize then
                begin
                  first_Tcount := TCount;
                  j := 0;
                while (TCount < Tsize) and (j < first_Tcount) do
                begin
                   for i := 1 to 8 do
                  begin
                   xi := temp_terrX[j] + dx[i];
                   yi := temp_terrY[j] + dy[i];

                   already_terr := false;
                   for g := 0 to TCount - 1 do
                     begin
                      if (xi = temp_terrX[g]) and (yi = temp_terrY[g]) then
                      begin
                      already_terr := true;
                      Break;
                      end;
                     end;

                   if not already_terr then
                    if ReproductionQuality(xi, yi) then
                    begin
                    c_available := False;
                    competitor_age := -1;
                      if ((Individual^.sex = 'f') and (Femalesmap[xi, yi, 0] = 3)) or
                      ((Individual^.sex = 'm') and (Malesmap[xi, yi, 0] = 3)) then
                      Continue
                      else
                      if ((Individual^.Sex = 'f') and (FemalesMap[xi, yi, 0] = 0)) or
                      ((Individual^.Sex = 'm') and (MalesMap[xi, yi, 0] = 0) and (FemalesMap[xi, yi, 0] = 3 )) then
                         c_available := True
                      else
                       if ((Individual^.Sex = 'f') and (FemalesMap[xi, yi, 0] = 2)) or
                       ((Individual^.Sex = 'm') and (MalesMap[xi, yi, 0] = 2) and (FemalesMap[xi, yi, 0] = 3 )) then
                      begin
                       if Individual^.Sex = 'f' then competitor_age := FemalesMap[xi,yi,1] else competitor_age := MalesMap[xi,yi,1];
                        Iwin := fight(Individual^.Age, competitor_age, Individual^.Sex);
                      end;
                    if (c_available) or (Iwin) then
                    begin
                      temp_terrX[TCount] := xi;
                      temp_terrY[TCount] := yi;

                      Inc(TCount);


                      if TCount = Tsize then Break;
                      end;
                    end;
                  end;
                   j := j + 1;
                 end;
                  end;



                  {Check that enough territory has been foundso territory can be removed and assigned according}
                  if TCount >= Tsize then
                  begin
                    {use temp_terr to remove those coordinates from existing territories}
                    for xy := 0 to TCount - 1 do
                      begin

                        with population do
                        begin
                          for d := 0 to population.Count - 1 do
                          begin
                            temp_ind := Items[d];
                            if temp_ind^.Sex = Individual^.Sex then
                            begin
                              with temp_ind^ do
                                for e := Length(TerritoryX) - 1 downto 0 do
                                begin
                                  if (TerritoryX[e] = temp_terrX[xy]) and (TerritoryY[e] = temp_terrY[xy]) then
                                  begin
                                    TerritoryX[e] := -1;
                                    TerritoryY[e] := -1;
                                  end;
                                end;
                            end;
                          end;
                        end;
                      end;

                    {Assign territory to individual and change status}
                    Individual^.status := 2;
                      for f := 0 to TCount - 1 do
                      begin
                        Individual^.TerritoryX[f] := temp_terrX[f];
                        Individual^.TerritoryY[f] := temp_terrY[f];

                        if Individual^.Sex = 'f' then
                        begin
                        FemalesMap[temp_terrX[f], temp_terrY[f], 0] := Individual^.Status;
                        FemalesMap[temp_terrX[f], temp_terrY[f], 1] := Individual^.Age;
                        end
                        else
                        begin
                          MalesMap[temp_terrX[f], temp_terrY[f], 0] := Individual^.Status;
                          MalesMap[temp_terrX[f], temp_terrY[f], 1] := Individual^.Age;
                        end;
                      end;

                      Break;  // No more steps required, as individual is now settled

                    end;

              ArrayToNegOne(temp_terrX);
              ArrayToNegOne(temp_terrY);
              TCount := 0;
              end;

              end;
              Inc(s);
          end;

        end;

        end;
      end;

    end;




procedure Startpopulation;
var
  a: integer;
begin
  Population := TList.Create;
  with population do
  begin
    for a := 1 to n_ini do
    begin
      new(Individual);

      Individual^.age := random(3) + 3;   {alternative: Individual^.age:=0; }

      if random < 0.5 then Individual^.sex := 'f'
      else
        Individual^.sex := 'm';
      Individual^.status := 1;

      Individual^.Coor_X := 130;
      Individual^.Coor_Y := 100;

      setLength(Individual^.TerritoryX, Tsize);
      setLength(Individual^.TerritoryY, Tsize);
      ArrayToNegOne(Individual^.TerritoryX);
      ArrayToNegOne(Individual^.TerritoryY);

      Individual^.mov_mem := random(8) + 1;
      Individual^.homeX := Individual^.Coor_X;
      Individual^.homeY := Individual^.Coor_Y;
      Individual^.return_home := False;

      Individual^.DailySteps := 0;
      Individual^.DailyStepsOpen := 0;

      Population.add(Individual);

    end;
  end;
end;

procedure Tspatial_Form.Pop_dynamics;
var
  a, b, xy, day, Tcheck: integer;
begin
  with population do
  begin
    for a := 1 to max_years do
    begin
      day := 0;  // Start the year
      while day < 366 do //Let's pretend there's no such thing as leap years
      begin
        day := day + 1;
        populationsize := population.Count;

        UpdateAbundanceMap;

        if day = 90 then
          if populationsize > 2 then
            reproduction;               // Reproduction happens at the end of March

        Dispersal(day);                 // Dispersal of surviving individuals (also includes dispersion start for subadults)

        survival;                       // Determine which individuals survive this day
      end;

      populationsize := population.Count;
      if populationsize > 0 then
      begin
        for b := 0 to populationsize - 1 do
        begin
          Individual := Items[b];
          Individual^.Age := Individual^.Age + 1;
          if (Individual^.Status = 2) and (Individual^.Age < max_rep_age) then
          begin
          Tcheck := 0;
          for xy := 0 to Tsize - 1 do
          begin
            if ((Individual^.TerritoryX[xy] > 0) and (Individual^.TerritoryY[xy] > 0)) then Tcheck := Tcheck + 1;
          end;
          if Tcheck = Tsize then Individual^.Status := 3;
          end;
        end;
      end;

      {ploting pop size}
      if max_pop_size < populationsize then max_pop_size := populationsize;
      if max_pop_size > Chart1.extent.YMax then
      begin
        Chart1.extent.YMax := max_pop_size;
        application.ProcessMessages;
      end;
      pop_size[a] := populationsize;
      sum_pop_size[a] := sum_pop_size[a] + populationsize;
      if populationsize > 0 then n_sim_no_ext[a] := n_sim_no_ext[a] + 1;
      {plot trajectory}
      Chart1LineSeries1.addxy(a, populationsize);

    end;
    if populationsize = 0 then N_extint := N_extint + 1;

    WriteMapCSV('FemalesMap_status.csv', Femalesmap, MapdimX, MapdimY, 0);
    WriteMapCSV('FemalesMap_age.csv', Femalesmap, MapdimX, MapdimY, 1);
    WriteMapCSV('MalesMap_status.csv', Malesmap, MapdimX, MapdimY, 0);
    WriteMapCSV('MalesMap_age.csv', Malesmap, MapdimX, MapdimY, 1);

  end;
end;


procedure Tspatial_Form.Run_ButtonClick(Sender: TObject);
var
  a, b: integer;
  t: string;
begin
  randomize; {initialize the pseudorandom number generator}
  val(Edit1.Text, n_ini);  {read parameter values from the form}
  val(Edit2.Text, max_years);
  val(Edit5.Text, n_sim);

  mapname := Edit10.Text;
  readmap(mapname);

  paramname := Edit3.Text;
  ReadParameters(paramname);

  N_extint := 0;

  AssignFile(to_file_out, file_name);
  rewrite(to_file_out); {create txt file}

  for a := 1 to max_years do sum_pop_size[a] := 0;
  for a := 1 to max_years do n_sim_no_ext[a] := 0;

  // Calculate array of step probabilities (here once) to be used in dispersal procedure later
  Step_probabilities;

  for current_sim := 1 to n_sim do
  begin

    max_pop_size := 0;
    Startpopulation; {call the procedure to initialize your population}
    Pop_dynamics;    {call the procedure to run the population dynamics}


    {plot population trayectories}
    Chart1LineSeries1.Clear;
    Chart1LineSeries2.Clear;

    for b := 1 to max_years do Chart1LineSeries1.addxy(b, pop_size[b]);
    application.ProcessMessages;
    if (current_sim > 1) then
      if (n_sim > 1) and (n_extint <> n_sim) then
        //   for b:=1 to max_years do Chart1LineSeries2.addxy(b,sum_pop_size[b]/n_sim_no_ext[b]);  //now we calculate the avg only when the population is not extinct
        for b := 1 to max_years do
          Chart1LineSeries2.addxy(b, sum_pop_size[b] / current_sim);

    {extinction memo1}
    if current_sim > 0 then str(n_extint / current_sim: 3: 2, t);
    memo1.Text := t;

    {save the results to a text file}
    append(to_file_out);
    for b := 1 to max_years do
      writeln(to_file_out, current_sim, ' ', b, ' ', pop_size[b]);
    {we save all simulations}
    if current_sim = n_sim then
      for b := 1 to max_years do
        writeln(to_file_out, 'avg', ' ', b, ' ', sum_pop_size[b] / current_sim);
    {and the average of all simulations}
    CloseFile(to_file_out);


    if current_sim = n_sim then WritePopulationToCSV(population, 'population_data.csv');

  end;

end;

procedure Tspatial_Form.Exit_ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure Tspatial_Form.Abort_ButtonClick(Sender: TObject);
begin
  halt;
end;

end.

