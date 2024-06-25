{vs. 1.0  1/9/2015
Revilla, E. (in press). Individual and agent based models in population ecology and conservation biology.
In: Population Ecology in Practice: Underused, Misused, and Abused Methods.
Eds Murray DL, Sandercock B. John Wiley & Sons Ltd. ISBN/ISSN: 9780470674130}

unit spatial_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, math, LCLType;

type           {here you declare the data structure for you individuals}
  PAgent=^Agent;
  Agent = record
    sex:             string[1];
    Age:             byte;         // In years
    Status:          shortint;  // 0=pre-dispersal cubs and subadults, 1=dispersing individuals, 2=early settled adults, 3 = fully settled adults

    Coor_X:          byte;
    Coor_Y:          byte;

    TerritoryX:      array[0..1] of integer;
    TerritoryY:      array[0..1] of integer;

    DailySteps:      byte;
    DailyStepsOpen:  byte;
    mov_mem:         byte;
    return_home:     boolean;  // not to be confused with Territory. This is for when individuals go on an excursion
                               // into Open habitat, for them to return to the last known Dispersal habitat they've visited
    homeX:           byte;
    homeY:           byte;

  end;

  { Tspatial_Form }

  Tspatial_Form = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit5: TEdit;
    Exit_Button: TButton;
    Abort_Button: TButton;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
    Procedure Pop_dynamics;
  end;

var               {here you declare global variables}
  spatial_Form: Tspatial_Form;
  Population : Tlist;
  Individual : PAgent;
  n_ini : integer;
  max_years: integer;
  rep_prob: real;
  AlphaR:real;
  BetaR:real;
  surv_prob: real;
  AlphaS:real;
  BetaS:real;
  sett_prob: real;
  avg_steps:integer;
  AlphaD:real;
  BetaD:real;
  sink:real;
  populationsize: longint;
  n_sim: integer;
  n_extint:integer;
  sum_distance_X:integer;
  sum_distance_Y:integer;
  current_sim:integer;
  max_pop_size: integer;
  step_probs: array of double;
  pop_size: array[1..100] of integer;
  sum_pop_size: array[1..100] of integer;
  n_sim_no_ext: array[1..100] of integer;
  to_file_out: TextFile;
  filename:text;
  HabitatMap: array of array of byte;
  FemalesSettledMap: array of array of byte;     // Fully settled individuals (Status 3)
  MalesSettledMap: array of array of byte;
  FemalesEarlyMap: array of array of byte;       // Early settlement - can still be outcompeted (Status 2)
  MalesEarlyMap: array of array of byte;
  Mapdimx,Mapdimy: integer;
  mapname:string;
  dx:array[0..8] of integer =(0, 0, 1, 1, 1, 0,-1,-1,-1);
  dy:array[0..8] of integer =(0, 1, 1, 0,-1,-1,-1, 0, 1);
  xp, yp: integer;
  tempX,tempY: integer;
  homeX, homeY,steps, s, new_dir, mem: integer;
  tohome: boolean;


const             {here you declare constants}
  //max_years = 10;
  min_rep_age = 2;
  min_rep_age_m = 3;
  max_rep_age = 9;
  max_age = 13;

  litter_size = 2.9;    // 0.2, 0.7 and 0.1 for 2,3 and 4 offspring resp.
  litter_size_sd = 2.61;
  rep_prob_oNP = 0.6;
  rep_prob_iNP = 0.8;

  surv_cub_iNP = 0.5;
  //surv_cub_iNP_sd = 0.516;
  surv_cub_oNP = 0.4;
  surv_sub_iNP = 0.7;
  //surv_sub_iNP_sd = 0.204;
  surv_sub_oNP = 0.6;
  surv_resident_iNP = 0.9;
  //surv_resident_iNP_sd = 0.312;
  surv_resident_oNP = 0.8;
  surv_disperse_f = 0.5;
  //surv_disperse_f_sd = 0.490;
  surv_disperse_m = 0.4;
  //surv_disperse_m_sd = 0.505;
  surv_old_iNP = 0.6;
  surv_old_oNP = 0.5;


  alpha_steps = 0.00027;
  max_steps = 100;

  theta_d = 0.4;
  // probability to move in the same direction as the previous movement step
  theta_delta = 0.65;
  // probability to move "backwards", with 0 = no backwards movement and 1=equally probable as moving forward
  delta_theta_long = 0.27;
  // Increase in movement autocorrelation in long-distance movements
  delta_theta_f = 0.1;
  // Increase in movement autocorrelation in fragmented habitat
  L = 6;                       // Long-distance threshold
  N_d = 5;                     // Fragmentation threshold
  beta = 0.8;                  // Avoidance of open habitat
  gamma = 0.09;                // Probability to return to dispersal habitat

  file_name = 'output.txt';
  file_name_ab_map = 'abundance_map_average.txt';

implementation

{$R *.lfm}

{ Tspatial_Form }

procedure ReadMap(mapname: string);
var
  ix, iy, Value: integer;
begin
  Assign(filename, mapName);
  reset(filename);
  readln(filename, Mapdimx, Mapdimy);
  SetLength(HabitatMap, Mapdimx + 1, Mapdimy + 1);
  SetLength(MalesSettledMap, Mapdimx + 1, Mapdimy + 1);
  SetLength(FemalesSettledMap, Mapdimx + 1, Mapdimy + 1);
  SetLength(MalesEarlyMap, Mapdimx + 1, Mapdimy + 1);
  SetLength(FemalesEarlyMap, Mapdimx + 1, Mapdimy + 1);

  for iy := 1 to Mapdimy do
  begin
    begin
      for ix := 1 to Mapdimx do
      begin
        Read(filename, Value);
        //HabitatMap (and the others) are 'byte' types which is less memory
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

procedure WritePopulationToCSV(const population: TList; const filename: string);
var
  csvFile: TextFile;
  i, j, x, y: integer;
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
      x := individual^.TerritoryX[j];
      y := individual^.TerritoryY[j];

      Write(csvFile, x, ',');
      Write(csvFile, y);

      // Add comma if not last coordinate
      if j < length(individual^.TerritoryX) - 1 then
        Write(csvFile, ',');
    end;

    WriteLn(csvFile); // End of current individual's data
  end;

  CloseFile(csvFile);
end;

function randomPoisson(mean: integer): integer;
{pseudorandom Poisson distributed number genetrator, Donald Knuth's algorithm}
const
  RESOLUTION = 1000;
var
  k: integer;
  b, l: real;
begin
  //assert(mean > 0, 'mean < 1');
  k:= 0;
  b:= 1;
  l:= exp(-mean);
  while b > l do
  begin
    k:= k + 1;
    b:= b * random(RESOLUTION)/RESOLUTION;
  end;
  if mean<= 0 then randomPoisson:=0 else randomPoisson:= k - 1;
end;

procedure ArrayToNegOne(var arr: array of integer);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    arr[i] := -1;
end;

procedure UpdateTerritoryMaps;
// Update the availability of males + local abundances. NOTE: DOESN'T CHECK IF MALE IS OF REPRODUCTIVE AGE
var
  a, b, x, y, t: integer;
  s : string;
begin

  for a := 0 to MapdimX - 1 do
    for b := 0 to Mapdimy - 1 do
    begin
      MalesSettledMap[a, b] := 0;      // Empty all maps to fill with age below
      FemalesSettledMap[a, b] := 0;
      FemalesEarlyMap[a,b] := 0;
      MalesEarlyMap[a,b] := 0;
    end;


  with population do
  begin
    for a := 0 to populationsize - 1 do
    begin
      Individual := Items[a];
      begin
        for b := 0 to length(Individual^.TerritoryX) - 1 do
        begin
          x := Individual^.TerritoryX[b];
          y := Individual^.TerritoryY[b];
          s := Individual^.sex;
          t := Individual^.Status;

          if (x = -1) and (y = -1) then Continue;

          if (x < 0) or (y < 0) or (x > MapdimX) or (y > MapdimY) then
          Continue;

          if (s = 'f') then
          begin
            if t = 3 then
            begin
              if FemalesSettledMap[x, y] <> 0 then
            Exit;
              FemalesSettledMap[x, y] := Individual^.Age
            end
            else if t=2 then
            begin
              if FemalesSettledMap[x, y] <> 0 then
            Exit;
            FemalesEarlyMap[x, y] := Individual^.Age
            end
            else
              Exit;
          end;

          if (s = 'm') then
          begin
            if t = 3 then
            begin
              if MalesSettledMap[x, y] <> 0 then
            Exit;
              MalesSettledMap[x, y] := Individual^.Age
            end
            else if t=2 then
            begin
              if MalesSettledMap[x, y] <> 0 then
            Exit;
            MalesEarlyMap[x, y] := Individual^.Age
            end
            else
              Exit;
          end;
      end;
    end;
  end;

end;

end;

function CanMoveHere(mem: integer): boolean;
begin
  // outside of map dimensions
  if (xp + dx[mem] < 0) or (xp + dx[mem] > Mapdimx) or (yp + dy[mem] < 0) or
    (yp + dy[mem] > Mapdimy) then Result := False
  else
  // barrier cell
  if (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 0) then Result := False
  else
    Result := True;
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

      Individual^.Coor_X := 2;
      Individual^.Coor_Y := 2;
      ArrayToNegOne(Individual^.TerritoryX);
      ArrayToNegOne(Individual^.TerritoryY);

      Individual^.mov_mem := random(8) + 1;
      Individual^.homeX:= Individual^.Coor_X;
      Individual^.homeY:= Individual^.Coor_Y;
      Individual^.return_home := False;

      Individual^.DailySteps:= 0;
      Individual^.DailyStepsOpen:= 0;

      Population.add(Individual);

    end;
  end;
end;

procedure Reproduction;
var
  a, current_litter_size, ls: integer;
  rep_prob: real;
  temp_X, temp_Y, Temp_mem: word;
begin
  with population do
  begin
    populationsize := population.Count;
    if (populationsize > 1) then
      for a := 0 to populationsize - 1 do
      begin
        Individual := Items[a];

        //Might need this later for density dependence
        //Localpop:=Abundancemap[Individual^.Coor_X,Individual^.Coor_Y]; // get local population size

        {Figure out in what habitat quality the individual is}
        if HabitatMap[Individual^.Coor_X, Individual^.Coor_Y] = 3 then
          rep_prob := rep_prob_iNP;
        if HabitatMap[Individual^.Coor_X, Individual^.Coor_Y] = 2 then
          rep_prob := rep_prob_oNP;

        {Check that the individual is capable of reproduction}
        if Individual^.sex = 'f' then
          if Individual^.status <= 2 then
            // Individual is settled
            if Individual^.age >= min_rep_age then
              if Individual^.age <= max_rep_age then
                if (MalesSettledMap[Individual^.Coor_X, Individual^.Coor_Y] <> 0) or
                (MalesEarlyMap[Individual^.Coor_X, Individual^.Coor_Y] <> 0) then
                  // Check that there is a local male
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

                      ArrayToNegOne(Individual^.TerritoryX);
                      ArrayToNegOne(Individual^.TerritoryX);

                      Individual^.mov_mem := Temp_mem;
                      Individual^.homeX:= Individual^.Coor_X;
                      Individual^.homeY:= Individual^.Coor_Y;
                      Individual^.return_home := False;

                      Individual^.DailySteps:= 0;
                      Individual^.DailyStepsOpen:= 0;

                      Population.add(Individual);
                    end;
                  end;
      end;
  end;

end;


procedure Survival;
var
  a: integer;
  surv_p, surv_day: real;
  die: boolean;

  //temp_X,temp_Y:word;
begin

  with population do
  begin
    populationsize := population.Count;
    for a := populationsize - 1 downto 0 do  //the index in the list starts at 0
    begin
      Individual := items[a];

      {Assign yearly survival probabilities}
      if HabitatMap[Individual^.Coor_X, Individual^.Coor_Y] = 3 then
      begin
        if (Individual^.Status = 0) and (Individual^.Age = 0) then
          surv_p := surv_cub_iNP;
        // the status statement shouldn't be necessary (cubs shouldn't be able to have another status but just to make sure)
        if (Individual^.Status = 0) and (Individual^.Age > 0) then
          surv_p := surv_sub_iNP;
        if (Individual^.Status > 1) and (Individual^.Age <= max_rep_age) then
          surv_p := surv_resident_iNP;
        if (Individual^.Age > max_rep_age) then surv_p := surv_old_iNP;
      end

      else if HabitatMap[Individual^.Coor_X, Individual^.Coor_Y] < 3 then
      begin
        if (Individual^.Age = 0) and (Individual^.Status = 0) then
          surv_p := surv_cub_oNP
        else if (Individual^.Age > 0) and (Individual^.Status = 0) then
          surv_p := surv_sub_oNP
        else if (Individual^.Status > 1) and (Individual^.Age <= max_rep_age) then
          surv_p := surv_resident_oNP
        else if (Individual^.Age > max_rep_age) then surv_p := surv_old_oNP;
      end;

      if (Individual^.Status = 1) and (Individual^.Sex = 'f') then
        surv_p := surv_disperse_f
      else if (Individual^.Status = 1) and (Individual^.Sex = 'm') then
        surv_p := surv_disperse_m;

      {Transform annual survival (surv_p) to daily survival (surv_day)}
      surv_day := Power(surv_p, (1 / 365));

      {Determine fate of individuals}
      die := False;
      if Individual^.age > max_age then die := True
      else
      if random > surv_day then die := True;
      if die then
        Delete(a);

    end;
  end;
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
      else if (HabitatMap[(xp + dx[i]), (yp + dy[i])] <= 2) then
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
      if CanMoveHere(mem) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] >= 2) and
        (p < (theta)) then
        new_dir := mem
      else
      begin
        h := random(nDisp) + 1;
        f := 0;
        for i := 1 to 8 do
        begin
          if CanMoveHere(i) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] >= 2) then
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
          {probability of moving backwards to autocorrelation}
        if (p < (theta + theta * theta_delta)) then
          begin
            if (mem = 0) and CanMoveHere(mem) and
              (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 1) then
            new_dir := mem
              else
            if (mem > 4) and CanMoveHere(mem - 4) and
              (HabitatMap[(xp + dx[mem - 4]), (yp + dy[mem - 4])] = 1) then
            new_dir := mem - 4
              else
            if (mem > 0) and (mem <= 4) and CanMoveHere(mem + 4) and
              (HabitatMap[(xp + dx[mem + 4]), (yp + dy[mem + 4])] = 1) then
            new_dir := mem + 4
          end;
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
        if CanMoveHere(mem) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] <= 2) and
          (p < theta) then
          new_dir := mem
        else
        {probability of moving backwards to autocorrelation}
        if (p < (theta + theta * theta_delta)) then
        begin
          if (mem = 0) and CanMoveHere(mem) and
            (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] <= 2) then
            new_dir := mem
          else
          if (mem > 4) and CanMoveHere(mem - 4) and
            (HabitatMap[(xp + dx[mem - 4]), (yp + dy[mem - 4])] <= 2) then
            new_dir := mem - 4
          else
          if (mem > 0) and (mem <= 4) and CanMoveHere(mem + 4) and
            (HabitatMap[(xp + dx[mem + 4]), (yp + dy[mem + 4])] <= 2) then
            new_dir := mem + 4;
        end;
        {Otherwise a random choise of Dispersal habitat cells}
        if (new_dir > 10) and (nDisp > 0) then
        begin
          h := random(nDisp) + 1;
          f := 0;

          for i := 1 to 8 do
          begin
            if CanMoveHere(i) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] >= 2) then
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
  if CanMoveHere(new_dir) = false then
    Exit;

  Result := new_dir;
end;

function FindCompetition(population: TList; targetSex: string;
  targetX, targetY: word): PAgent;
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


Procedure Dispersal(day: integer);
var
a, b, c, d, e, f, new_dir,  TCount, Bcount, FCount, age_competition: integer;
TestCoordX, TestCoordY, xi, yi, xy, coordX, coordY: integer;
age_m, P_disp_start : real;
temp_terrX, temp_terrY, B_cellsX, B_cellsY: array of integer;
competitor, competitor2, temp_ind: PAgent;
Iwin, test_cell_available, can_settle: boolean;

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

      {restart dispersal in case newly settled female individuals do not have enough territory}
      if (Individual^.Status = 2) then
      begin
        TCount := 0;
        FCount := 0;

        for b := 0 to length(Individual^.TerritoryX) - 1 do
        begin
          if (Individual^.TerritoryX[b] > -1) and (Individual^.TerritoryY[b] > -1) then
          begin
            Inc(TCount);
            if (Individual^.Sex = 'm') and (FemalesSettledMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b]] > 0) then
              Inc(FCount);
          end;
        end;
        if (TCount < 2) or ((Individual^.Sex = 'm') and (FCount < 1)) then
        begin
          Individual^.Status := 1;
          /// Reset Territory to -1 / NA values
          for b := 0 to length(Individual^.TerritoryX) - 1 do
          begin
            Individual^.TerritoryX[b] := -1;
            Individual^.TerritoryY[b] := -1;
          end;
        end;
      end;

      {Now start dispersal IF individual has dispersal status}

      { #todo : Include a step to check if the current location is breeding habitat that is free -
      Survival happens after this, so it is possible for an individual to be in breeding habitat
      that it couldn't take over in the previous day, but can now. In that case it doesn't make sense to move }

      if (Individual^.Status = 1) then
      begin
        SetLength(temp_terrX, 2);
        SetLength(temp_terrY, 2);

        steps := NSteps(step_probs);
        s := 1;

        while s <= steps do
        begin

          {Reset variables}
          xp := Individual^.Coor_X;
          yp := Individual^.Coor_Y;

          TestCoordX := 0;
          TestCoordY := 0;

          new_dir := MoveDir;

          {Calculate coordinates to move to}
          TestCoordX := xp + dx[new_dir];
          TestCoordY := yp + dy[new_dir];

          {update home location if individual moves from dispersal to open habitat}
          if (HabitatMap[xp, yp] >= 2) and (HabitatMap[TestCoordX, TestCoordY] = 1) then
          begin
            Individual^.homeX := xp;
            Individual^.homeY := yp;
          end;

          {change tohome to false if individual is back in dispersal habitat}
          if (HabitatMap[TestCoordX, TestCoordY] >= 2) and tohome = True then
            Individual^.return_home := False;

          {Move individual and update memory}
          Individual^.Coor_X := TestCoordX;
          Individual^.Coor_Y := TestCoordY;
          if (new_dir <> 0) then Individual^.mov_mem := new_dir;

          {Is settlement possible}
          if HabitatMap[TestCoordX, TestCoordY] >= 2 then
          begin
            test_cell_available := false;

            if ((Individual^.sex = 'f') and (FemalesSettledMap[TestCoordX, TestCoordY] = 0)) or
            ((Individual^.sex = 'm') and (MalesSettledMap[TestCoordX, TestCoordY] = 0)) then
            test_cell_available := true
            else
            if ((Individual^.sex = 'f') and (FemalesEarlymap[TestCoordX, TestCoordY] <> 0)) then
            begin
            age_competition := FemalesEarlymap[TestCoordX, TestCoordY];
            Iwin := fight(Individual^.Age, age_competition, 'f');
            if Iwin then test_cell_available:= true;
            end
            else
            if ((Individual^.sex = 'm') and (MalesEarlymap[TestCoordX, TestCoordY] <> 0)) then
            begin
            age_competition := MalesEarlymap[TestCoordX, TestCoordY];
            Iwin := fight(Individual^.Age, age_competition, 'm');
            if Iwin then test_cell_available:= true;
            end;

            if test_cell_available then
            begin
        {Look for more breeding habitat until teritory is big enough}

              temp_terrX[0] := TestCoordX;
              temp_terrY[0] := TestCoordY;
              TCount := 1;

              begin
                {Look at and identify nearby cells}
                SetLength(B_cellsX, 9);
                SetLength(B_cellsY, 9);
                Bcount := 0;

                // Walk through all 9 cells and sort cell in right category
                for xi := (TestCoordX - 1) to (TestCoordX + 1) do
                  for yi := (TestCoordY - 1) to (TestCoordY + 1) do
                  begin
                    //Skip this iteration if it's the central cell
                    if (xi = TestCoordX) and (yi = TestCoordY) then Continue;

                    if HabitatMap[xi, yi] >= 2 then
                    begin
                      B_cellsX[BCount] := xi;
                      B_cellsY[BCount] := yi;
                      Inc(Bcount);
                    end;

                  end;

                {Investigate availability of adjacent breeding cell and add to temporary territory}
                if Bcount > 0 then
                begin
                  { #note : This creates a bias towards selecting territory that gets found first through the xi, yi loop above. But as this territory assignment needs to be different anyway, I'll leave it for now }
                  for c := 0 to Bcount - 1 do
                  begin

                    coordX := B_cellsX[c];
                    coordY := B_cellsY[c];
                    test_cell_available := false;

                    if ((Individual^.sex = 'f') and (FemalesSettledMap[coordX, coordY] = 0)) or
                    ((Individual^.sex = 'm') and (MalesSettledMap[TestCoordX, TestCoordY] = 0)) then
                    test_cell_available := true
                    else
                    if ((Individual^.sex = 'f') and (FemalesEarlymap[TestCoordX, TestCoordY] <> 0)) then
                    begin
                      age_competition := FemalesEarlymap[TestCoordX, TestCoordY];
                      Iwin := fight(Individual^.Age, age_competition, 'f');
                      if Iwin then test_cell_available:= true;
                    end
                    else
                    if ((Individual^.sex = 'm') and (MalesEarlymap[TestCoordX, TestCoordY] <> 0)) then
                    begin
                      age_competition := MalesEarlymap[TestCoordX, TestCoordY];
                      Iwin := fight(Individual^.Age, age_competition, 'm');
                      if Iwin then test_cell_available:= true;
                    end;

                    if test_cell_available then
                    begin
                      temp_terrX[TCount] := coordX;
                      temp_terrY[TCount] := coordY;

                      Inc(TCount);
                      if Tcount = 2 then Break;
                    end;
                  end;


                  {Check if sufficient breeding territory is available, and if there is a settled female available for the males}
                  can_settle:= false;

                  if (Individual^.sex = 'f') and (TCount > 1) then can_settle := true;
                  if (Individual^.sex = 'm') and (TCount > 1) then
                  for b := 0 to Tcount - 1 do
                  begin
                    if FemalesSettledMap[temp_terrX[b], temp_terrY[b]] <> 0 then can_settle := true;
                    if can_settle := true then Break;
                  end;

                  {If settlement is possible,}
                  if can_settle then
                  begin
                    {First remove the territory from any possible competition}
                    for xy := 0 to TCount - 1 do
                      begin
                        if (Individual^.sex = 'f') and (FemalesEarlyMap[temp_terrX[xy], temp_terrY[xy]] <> 0) or
                        (Individual^.sex = 'm') and (MalesEarlyMap[temp_terrX[xy], temp_terrY[xy]] <> 0) then
                        with population do
                        begin
                          for d := 0 to population.Count - 1 do
                          begin
                            temp_ind := Items[d];
                            if temp_ind^.Sex = Individual^.sex then
                            begin
                              with temp_ind^ do
                                for e := Length(TerritoryX) - 1 downto 0 do
                                begin
                                  if (TerritoryX[e] = temp_terrX[xy]) and
                                    (TerritoryY[e] = temp_terrY[xy]) then
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
                    begin

                      Individual^.status := 2;
                      for f := 0 to TCount - 1 do
                      begin
                        Individual^.TerritoryX[f] := temp_terrX[f];
                        Individual^.TerritoryY[f] := temp_terrY[f];

                        {Add territory to the early territory maps}
                        if Individual^.sex = 'f' then
                        begin
                          FemalesEarlyMap[temp_terrX[f], temp_terrY[f]] := Individual^.Age
                        end
                        else
                        begin
                          MalesEarlyMap[temp_terrX[f], temp_terrY[f]] := Individual^.Age
                        end;

                      end;

                      Break;  // No more steps required, as individual is now settled

                    end;
                  end;
                end;
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

Procedure Tspatial_Form.Pop_dynamics;
var
a, b, day:integer;

begin
  with population do
  begin
    for a:=1 to max_years do
    begin
      day := 0;  // Start the year
      while day < 366 do //Let's pretend there's no such thing as leap years
      begin
        day := day + 1;
        populationsize := population.Count;

        UpdateTerritoryMaps;

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
          Individual^.Age:= Individual^.Age + 1;
          if Individual^.Status = 2 then Individual^.Status := 3;
        end;
      end;

      {ploting pop size}
      if max_pop_size<populationsize then max_pop_size:=populationsize;
      if max_pop_size>Chart1.extent.YMax then
      begin
        Chart1.extent.YMax:=max_pop_size;
        application.processmessages;
      end;
      pop_size[a]:=populationsize;
      sum_pop_size[a]:=sum_pop_size[a]+ populationsize;
      if populationsize>0 then n_sim_no_ext[a]:=n_sim_no_ext[a]+1;
      {plot trajectory}
      Chart1LineSeries1.addxy(a,populationsize);

    end;
    if  populationsize=0 then N_extint:=N_extint+1;
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
  close;
end;

procedure Tspatial_Form.Abort_ButtonClick(Sender: TObject);
begin
  halt;
end;

end.

