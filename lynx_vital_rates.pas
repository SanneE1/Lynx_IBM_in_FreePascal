unit lynx_vital_rates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  lynx_define_units, general_functions;


procedure Reproduction;
procedure Survival;
procedure Dispersal(day: integer);

function Nsteps(step_probs: array of double): integer;
procedure Step_probabilities;

function CanMoveHere(x,y: integer): boolean;
Function inPark(x,y:integer):boolean;
function whichPop(x,y:integer):integer;
Function ReproductionQuality(x,y:integer):boolean;

function MoveDir: integer;

function FindTerrOwner(population: TList; targetSex: string; targetX, targetY: word): PAgent;
function Fight(AgeDisperser, AgeEarlySettler: integer; Sex: string): boolean;
function TerritoryCellAvailable(x,y: integer; Sex:string; disperser_age: integer): boolean;
procedure ClaimNewTerrOrStartDispersal;

implementation


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
                    begin
                    male_present := true;
                    Break;
                    end;
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

                      Individual^.Natal_pop := whichPop(temp_X, temp_Y);
                      Individual^.Current_pop := whichPop(temp_X, temp_Y);
                      Individual^.Previous_pop := whichPop(temp_X, temp_Y);

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
  a, b, d, e, f, g, i, j, new_dir, TestCoordX, TestCoordY, TCount, first_Tcount, FCount, xi, yi, xy, competitor_age : integer;
  age_m, P_disp_start: real;
  temp_terrX, temp_terrY: array of integer;
  temp_ind: PAgent;
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

      {Check size of territory for newly established individuals.
      If they do not have enough, see if there is enough unclaimed territory around to add to their territory.
      If not, restart dispersal}
      if (Individual^.Status = 2) then
      begin
        TCount := 0;

        for b := 0 to length(Individual^.TerritoryX) - 1 do
        begin
          if (Individual^.TerritoryX[b] > -1) and (Individual^.TerritoryY[b] > -1) then
          begin
            if (Individual^.Sex = 'f') or
            ((Individual^.Sex = 'm') and (FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0] = 3)) then
            Inc(TCount);
          end;
        end;

        {If there's not enough territory, see if there's any unclaimed available}
        if (TCount < Tsize) and (TCount > 0) then //If TCount is 0 that means that the individual will have to move for sure to find new territory
        ClaimNewTerrOrStartDispersal;
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

          {Move individual and update memory and population}
          Individual^.Coor_X := TestCoordX;
          Individual^.Coor_Y := TestCoordY;
          if (new_dir <> 0) then Individual^.mov_mem := new_dir;

          {Add new location to connection map}
          if Individual^.Sex = 'f' then ConnectionMap[TestCoordX, TestCoordY, 0] := ConnectionMap[TestCoordX, TestCoordY, 0] + 1
          else ConnectionMap[TestCoordX, TestCoordY, 1] := ConnectionMap[TestCoordX, TestCoordY, 1] + 1;


          if  (Individual^.Current_pop = 0) and (whichPop(TestCoordX, TestCoordY) <> 0) and
          (Individual^.Previous_pop <> whichPop(TestCoordX, TestCoordY)) then
          begin
           new(MigrationEvent);

           MigrationEvent^.simulation := current_sim;
           MigrationEvent^.year := current_year;
           MigrationEvent^.sex := Individual^.Sex;
           MigrationEvent^.age := Individual^.Age;
           MigrationEvent^.natal_pop := Individual^.Natal_pop;
           MigrationEvent^.old_pop := Individual^.Previous_pop;
           MigrationEvent^.new_pop := whichPop(TestCoordX, TestCoordY);

           MigrationList.Add(MigrationEvent);
          end;

          if (Individual^.Current_pop <> whichPop(TestCoordX, TestCoordY)) then
          begin
           Individual^.Previous_pop := Individual^.Current_pop;
           Individual^.Current_pop := whichPop(TestCoordX, TestCoordY);
          end;


          {Increase daily steps in open, if new coordinates are in an open habitat}
          if HabitatMap[TestCoordX, TestCoordY] = 1 then Individual^.DailyStepsOpen:= Individual^.DailyStepsOpen + 1;


          {If in breeding habitat, check if settlement is possible}
          if (HabitatMap[TestCoordX, TestCoordY] = 2) and (ReproductionQuality(TestCoordX, TestCoordY)) then
          begin

            test_cell_available := False;
            test_cell_available := TerritoryCellAvailable(TestCoordX, TestCoordY, Individual^.Sex, Individual^.Age);

            if test_cell_available then
            begin
              {Look for more breeding habitat until teritory is big enough}

              temp_terrX[0] := TestCoordX;
              temp_terrY[0] := TestCoordY;
              TCount := 1;

                // Walk through all 9 cells and find any available territory
                for i := 1 to 8 do
                  begin
                    xi := TestCoordX + dx[i];
                    yi := TestCoordY + dy[i];
                    if CanMoveHere(xi, yi) then
                    if ((HabitatMap[xi, yi] = 2) and (ReproductionQuality(xi, yi))) then
                    begin
                      c_available:= False;
                      c_available:= TerritoryCellAvailable(xi, yi, Individual^.Sex, Individual^.Age);

                      if c_available then
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
                    if CanMoveHere(xi, yi) then
                    if ((HabitatMap[xi, yi] = 2) and (ReproductionQuality(xi, yi))) then
                    begin
                    c_available := False;
                    c_available:= TerritoryCellAvailable(xi, yi, Individual^.Sex, Individual^.Age);

                      if c_available then
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


function CanMoveHere(x,y: integer): boolean;
begin
  // outside of map dimensions
  if (x < 0) or (x > Mapdimx) or (y < 0) or (y > Mapdimy) then Result := False
  else
  // barrier cell
    if (HabitatMap[x, y] = 0) then Result := False
    else
      Result := True;
end;

Function inPark(x,y:integer):boolean;
begin
  Result:=False;   // false = not in park, true = in NP

  if (ParkMap[x,y] = 1) then Result := True
{else
   ShowErrorAndExit('No in-park areas defined for this map file');
 }
end;

function whichPop(x,y:integer):integer;
begin
  Result := PopsMap[x,y];
end;

Function ReproductionQuality(x,y:integer):boolean;

begin
  Result:=False;
  if (BreedingHabitatMap[x,y] = 1) then Result := True

  {else
  ShowErrorAndExit('No reproduction quality areas defined for this map file');
   }

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
    if CanMoveHere((xp + dx[i]), (yp + dy[i])) then
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
        if CanMoveHere((xp + dx[i]), (yp + dy[i])) then
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
        h := random(nDisp) + 1;
        f := 0;
        for i := 1 to 8 do
        begin
          if CanMoveHere((xp + dx[i]), (yp + dy[i])) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 2) then
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
    if fragmented = False then
    begin
      {movement in non-fragmented area}
      if (p < theta) and CanMoveHere((xp + dx[mem]), (yp + dy[mem])) then new_dir := mem
      else
        if f < (theta + theta * theta_delta) then
        begin
          if (mem > 4) and CanMoveHere((xp + dx[mem-4]), (yp + dy[mem-4])) then new_dir := mem - 4
          else
            if (mem > 0) and (mem <= 4) and CanMoveHere((xp + dx[mem+4]), (yp + dy[mem+4])) then new_dir := mem + 4;
        end;
      if new_dir > 10 then  // If new direction has not yet been assigned through autocorrelation, random movement
      begin
        h := random(nDisp + nOpen) + 1;
        f := 0;
        for i := 1 to 8 do
        begin
          if CanMoveHere((xp + dx[i]), (yp + dy[i])) then
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
        if CanMoveHere((xp + dx[mem]), (yp + dy[mem])) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 1) and (p < theta) then
          new_dir := mem
        else
        if (new_dir > 10) and (nOpen > 0) then
        begin
          h := random(nOpen) + 1;
          f := 0;
          for i := 1 to 8 do
          begin
            if CanMoveHere((xp + dx[i]), (yp + dy[i])) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 1) then
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
        if CanMoveHere((xp + dx[mem]), (yp + dy[mem])) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 2) and (p < theta) then
          new_dir := mem
        else
        {probability of moving backwards to autocorrelation}
          if (p < (theta + theta * theta_delta)) then
          begin
            if (mem = 0) and CanMoveHere((xp + dx[mem]), (yp + dy[mem])) and (HabitatMap[(xp + dx[mem]), (yp + dy[mem])] = 2) then
              new_dir := mem
            else
              if (mem > 4) and CanMoveHere((xp + dx[mem-4]), (yp + dy[mem-4])) and (HabitatMap[(xp + dx[mem - 4]), (yp + dy[mem - 4])] = 2) then
                new_dir := mem - 4
              else
                if (mem > 0) and (mem <= 4) and CanMoveHere((xp + dx[mem+4]), (yp + dy[mem+4])) and (HabitatMap[(xp + dx[mem + 4]), (yp + dy[mem + 4])] = 2) then
                  new_dir := mem + 4;
          end;
        {Otherwise a random choise of Dispersal habitat cells}
        if (new_dir > 10) and (nDisp > 0) then
        begin
          h := random(nDisp) + 1;
          f := 0;

          for i := 1 to 8 do
          begin
            if CanMoveHere((xp + dx[i]), (yp + dy[i])) and (HabitatMap[(xp + dx[i]), (yp + dy[i])] = 2) then
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
  if CanMoveHere((xp + dx[new_dir]), (yp + dy[new_dir])) = False then
    Exit;

  Result := new_dir;
end;


function FindTerrOwner(population: TList; targetSex: string; targetX, targetY: word): PAgent;
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

function TerritoryCellAvailable(x,y: integer; Sex:string; disperser_age: integer): boolean;
var
  resident_age: integer;
  Iwin: boolean;
begin

  Result := False;

  if ((Sex = 'f') and (Femalesmap[x, y, 0] = 3)) or
  ((Sex = 'm') and (Malesmap[x, y, 0] = 3)) then
  Result := False
  else
    if ((Sex = 'f') and (Femalesmap[x, y, 0] = 0)) or
    ((Sex = 'm') and (Malesmap[x, y, 0] = 0) and (Femalesmap[x, y, 0] = 3)) then
    Result := True
    else
      if ((Sex = 'f') and (Femalesmap[x, y, 0] = 2)) or
      ((Sex = 'm') and (Malesmap[x, y, 0] = 2) and (FemalesMap[x,y,0] = 3)) then
      begin
      resident_age := -1;
        if (Sex = 'f') then
        resident_age := Femalesmap[x, y, 1]
        else
          resident_age := Malesmap[x, y, 1];
        Iwin := fight(disperser_age, resident_age, Sex);
        if Iwin then Result := True;
      end;

end;

procedure ClaimNewTerrOrStartDispersal;
var
  temp_terrX, temp_terrY: array of integer;
  temp_ind: PAgent;
  b,first_Tcount, TCount,d, e, f, j, i, g, xi, yi, xy: integer;
  already_terr, c_available: boolean;
 begin
        SetLength(temp_terrX, Tsize);
        SetLength(temp_terrY, Tsize);
        ArrayToNegOne(temp_terrX);
        ArrayToNegOne(temp_terrY);

        {Get all current claimed territory} //not already done above, as TCount < TSize for status = 2 should be less common throughout the year once individuals are more settled}
        for b := 0 to length(Individual^.TerritoryX) - 1 do
        begin
          if (Individual^.TerritoryX[b] > -1) and (Individual^.TerritoryY[b] > -1) then
          begin
          if (Individual^.Sex = 'f') or
            ((Individual^.Sex = 'm') and (FemalesMap[Individual^.TerritoryX[b], Individual^.TerritoryY[b], 0] = 3)) then
          temp_terrX[b] := Individual^.TerritoryX[b];
          temp_terrY[b] := Individual^.TerritoryY[b];
          end;
        end;

        {See if there's other available territory nearby}
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
                    if ((HabitatMap[xi, yi] = 2) and (ReproductionQuality(xi, yi))) then
                    begin
                    c_available := False;
                    c_available:= TerritoryCellAvailable(xi, yi, Individual^.Sex, Individual^.Age);

                      if c_available then
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

        {If there's enough territory available, assign to individual, and make sure individual is located within territory}
        if (TCount = Tsize) then
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
        end
        else
        {Reset territory information to empty and restart dispersal if there's not enough territory}
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

end.

