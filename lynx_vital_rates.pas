unit lynx_vital_rates;

{$mode ObjFPC}{$H+}
{$OPTIMIZATION OFF}
interface

uses
  Classes, SysUtils, Math, Dialogs,
  lynx_define_units, lynx_dispersal_assist_functions, general_functions;


procedure Reproduction;
procedure Survival;
procedure Dispersal(day: integer);

function CalculateIC(child_ID: integer; Famtree: Array2Dreal): real;
function FindClosestCommonAncestors(Famtree:  Array2Dreal; child_ID: integer): Array2Dinteger;

implementation


procedure Reproduction;
var
  a, g, i, k, current_litter_size, ls, xy, male_x, male_y, homogeneity_count: integer;
  rep_prob, tmic, IC_kittens : real;
  temp_X, temp_Y, Temp_mem: word;
  male_present: boolean;
  PotentialFather: PAgent;
  father, mother: array of array of integer;
  mother_ID, father_ID, selected_kitten: integer;
begin
  rep_prob := rep_prob;

  with population do
  begin
    populationsize := population.Count;
    if (populationsize > 1) then
      for a := 0 to populationsize - 1 do
      begin
        Individual := Items[a];
        if Individual^.Sex = 'm' then Continue;

        male_present:= false;

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
					male_x := Individual^.TerritoryX[xy];
                    male_y := Individual^.TerritoryY[xy];
                    Break;
                    end;
                end;

                if male_present then
                begin
                  setLength(mother, 25, 2);
                  setLength(father, 25, 2);

                  PotentialFather := nil;
                PotentialFather := FindTerrOwner(population, 'm', male_x, male_y);
                if (PotentialFather <> nil) then
                begin
                for i:= 1 to 24 do
                begin
                    for k:= 0 to 1 do
                    begin
                      mother[i,k]:= Individual^.Genome[i,k];
                      father[i,k]:= PotentialFather^.Genome[i,k];
                    end;
               end;
                father_ID := PotentialFather^.UniqueID
                end
                else
                begin
                  male_present:=false;
                  end;
                end;


                if male_present then
                  if random < rep_prob then
                  begin
                    current_litter_size := Round(randg(litter_size, litter_size_sd));
                    mother_ID := Individual^.UniqueID;
                    IC_kittens:= -1;       //something goes wrong, -1 and not the before liiter IC



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
                      Individual^.UniqueID := UniqueIDnext;
                      UniqueIDnext:= UniqueIDnext+1;

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


                      //inherit genes of mother
                      setLength(Individual^.Genome, 25, 2);
                      homogeneity_count := 0;


                      for i := 1 to 24 do
                      begin
                        tmic:=random;
                        if tmic < 0.5 then
                           Individual^.Genome[i, 0] := mother[i, 0]
                        else
                           Individual^.Genome[i, 0] := mother[i, 1];

                       //inherit genes of father

                        tmic := random;
                        if tmic < 0.5 then
                           Individual^.Genome[i, 1] := father[i, 0]
                        else
                           Individual^.Genome[i, 1] := father[i, 1];

                      //check for homogeneity
                      if Individual^.Genome[i, 0] = Individual^.Genome[i, 1] then
                      homogeneity_count := homogeneity_count + 1;
                      end;

                      //ratio of homogeneity
                       Individual^.P_homogeneity := homogeneity_count / 24.0;


                       {Inbreeding calculations}
                       // Add new individual to Famtree
                      SetLength(Famtree, Length(Famtree) + 1, 4);
                      Famtree[High(Famtree), 0] := Individual^.UniqueID;
                      Famtree[High(Famtree), 2] := father_ID;
                      Famtree[High(Famtree), 3] := mother_ID;


                      //Only one kitten gets CA and IC calculation
                      if ls = 1 then
                      begin
                      IC_kittens := CalculateIC(Individual^.UniqueID, Famtree);
                      end;


                      Individual^.IC:= IC_kittens;
                      Famtree[High(Famtree), 1] := IC_kittens;

                      Population.add(Individual);


                    end;
                  end;
              end;
      end;
  end;
end;
// Placeholder for function CalculateIC

function CalculateIC(child_ID: integer; Famtree: Array2Dreal): real;
var
  CA: Array2Dinteger;
  Steps, a: integer;
  IC_sum, b, c: real;
begin

 setLength(CA, 2);
 CA := FindClosestCommonAncestors(Famtree,child_ID);

 if CA <> nil then
 begin
 IC_sum := 0;

 for a:=0 to Length(CA) - 1 do
 begin
  c:= Famtree[CA[a,0], 1];
  b := power(0.5, CA[a,1]-1) * (1 + c);
  IC_sum := IC_sum + b;
 end;

 Result := IC_sum;
 end
 else
 Result := 0;

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
      if (Individual^.Status = 1) then surv_p := surv_disperse
      else
        begin
          if (Individual^.Status = 0) and (Individual^.Age = 0) then
            surv_p := surv_cub
            else
          // the status statement shouldn't be necessary (cubs shouldn't be able to have another status but just to make sure)
          if (Individual^.Status = 0) and (Individual^.Age > 0) then
            surv_p := surv_sub
            else
          if (Individual^.Status > 1) and (Individual^.Age <= max_rep_age) then
            surv_p := surv_resident
            else
          if (Individual^.Age > max_rep_age) then surv_p := surv_old;
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


function FindClosestCommonAncestors(Famtree: Array2Dreal; child_ID: integer): Array2Dinteger;

var
  ID, i, j, q, t, a, min_steps: integer;
  common_ancestors, queue_mother, queue_father, visited_mother, visited_father: array of array of integer;
  closest_ancestors : Array2Dinteger;
  current_mother, current_father: array[0..1] of integer;
  Steps: integer;

begin
  // Initialize the mother's queue
  SetLength(queue_mother, 1, 2);
  queue_mother[0,0] := Round(Famtree[child_ID, 3]);
  queue_mother[0,1] := 1;

  // Initialize the father's queue
  SetLength(queue_father, 1, 2);
  queue_father[0,0] := Round(Famtree[child_ID, 2]);
  queue_father[0,1] := 1;

  // Expansion of the mother's ancestors//

  while Length(queue_mother) > 0 do
  begin
    current_mother[0] := queue_mother[High(queue_mother),0];
    current_mother[1] := queue_mother[High(queue_mother),1];

    SetLength(queue_mother, Length(queue_mother) - 1);

    // Add to visited
    SetLength(visited_mother, Length(visited_mother) + 1, 2);
    visited_mother[High(visited_mother),0] := current_mother[0];
    visited_mother[High(visited_mother),1] := current_mother[1];

    // Expand to parents of current node
    if (current_mother[0] <> -1) and (current_mother[0] < Length(Famtree)) then
    begin
     if Famtree[current_mother[0], 2] <> -1 then               //father of current mother
     begin
      SetLength(queue_mother, Length(queue_mother) + 1,2);     //add new line to the queue to add father of current_mother
      queue_mother[High(queue_mother),0] := Round(Famtree[current_mother[0], 2]);
      queue_mother[High(queue_mother),1] := current_mother[1] + 1;          //number of steps correspond to the previous one +1
     end;
    end;

    if (current_mother[0] <> -1) and (current_mother[0] < Length(Famtree)) then
    begin
     if Famtree[current_mother[0], 3] <> -1 then                            //mother of current_mother
     begin
      SetLength(queue_mother, Length(queue_mother) + 1,2);
      queue_mother[High(queue_mother),0] := Round(Famtree[current_mother[0], 3]);
      queue_mother[High(queue_mother),1] := current_mother[1] + 1;
    end;
    end;
  end;

  // Expansion of the mother's ancestors//

  while Length(queue_father) > 0 do
  begin
    current_father[0] := queue_father[High(queue_father),0];
    current_father[1] := queue_father[High(queue_father),1];
    SetLength(queue_father, Length(queue_father) - 1);

    //Add to visited
    SetLength(visited_father, Length(visited_father) + 1,2);
    visited_father[High(visited_father),0] := current_father[0];
    visited_father[High(visited_father),1] := current_father[1];

    // Expand to parents of current node
    if (current_father[0] <> -1) and (current_father[0] < Length(Famtree)) then
    begin
     if Famtree[current_father[0], 2] <> -1 then
     begin
      SetLength(queue_father, Length(queue_father) + 1,2);
      queue_father[High(queue_father),0] := Round(Famtree[current_father[0], 2 ]);
      queue_father[High(queue_father),1] := current_father[1] + 1;
     end;
    end;

    if (current_father[0] <> -1) and (current_father[0] < Length(Famtree)) then
    begin
    if Famtree[current_father[0], 3] <> -1 then
    begin
      SetLength(queue_father, Length(queue_father) + 1,2);
      queue_father[High(queue_father),0] := Round(Famtree[current_father[0], 3]);
      queue_father[High(queue_father),1] := current_father[1] + 1;
    end;
    end;
  end;

  // find CA
  SetLength(common_ancestors, 0);
  for i := 0 to Length(visited_mother) - 1 do
    for j := 0 to Length(visited_father) - 1 do
      if visited_mother[i,0] = visited_father[j,0] then
      begin
        SetLength(common_ancestors, Length(common_ancestors) + 1, 2);
        common_ancestors[High(common_ancestors),0] := visited_mother[i,0];
        common_ancestors[High(common_ancestors),1] :=
          visited_mother[i,1] + visited_father[j,1];
      end;

  //Find min number of steps
  min_steps := MaxInt;
  for q := 0 to Length(common_ancestors) - 1 do
    if common_ancestors[q,1] < min_steps then
      min_steps := common_ancestors[q,1];

  //Filter Common Ancestors with the minimum number of steps
  if(common_ancestors <> nil) then
  begin
  setLength(closest_ancestors,1);
  SetLength(closest_ancestors[0], 2);
  a := 0;

  for t := 0 to Length(common_ancestors) - 1 do
    if common_ancestors[t,1] = min_steps then
    begin

      SetLength(closest_ancestors, a + 1);
      SetLength(closest_ancestors[a], 2);

      closest_ancestors[a][0] := common_ancestors[t,0];
      closest_ancestors[a][1] := common_ancestors[t,1];

      a := a+1;
    end;
  Result := closest_ancestors;
   end
  else
  begin
    Result := nil;
    end;

end;
end.

