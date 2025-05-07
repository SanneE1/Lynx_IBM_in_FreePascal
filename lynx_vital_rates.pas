unit lynx_vital_rates;

{$mode ObjFPC}{$H+}
{$OPTIMIZATION OFF}
interface

uses
  Classes, SysUtils, Math, Generics.Collections, Types,
  lynx_define_units, lynx_dispersal_assist_functions, general_functions;


procedure Reproduction;
procedure Survival;
procedure Dispersal(day: integer);

function CalculateIC(child_ID: integer; Famtree: Array2Dreal): real;
function FindClosestCommonAncestors(Famtree:  Array2Dreal; child_ID: integer): Array2Dinteger;

implementation


procedure Reproduction;
var
  a,  x,y, i, k, current_litter_size, ls, xy, male_x, male_y, homogeneity_count, CurrentDist: integer;
  tmic, IC_kittens, rand_val, IC_rep_prob : real;
  temp_X, temp_Y, Temp_mem: word;
  male_present: boolean;
  PotentialFather: PAgent;
  father, mother: array of array of integer;
  mother_ID, father_ID: integer;
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

        {Check that the individual is capable of reproduction}
          if Individual^.status = 3 then
            // Individual is settled
            if Individual^.age >= min_rep_age then
              if Individual^.age <= max_rep_age then
              begin

              // Check that there is a local male
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

                {for i:= 1 to 24 do
                begin
                    for k:= 0 to 1 do
                    begin
                      mother[i,k]:= Individual^.Genome[i,k];
                      father[i,k]:= PotentialFather^.Genome[i,k];
                    end;
               end;
                }
                father_ID := PotentialFather^.UniqueID
                end;
                if (PotentialFather = nil) then
                begin
                  male_present:=false;
                  end;

                end;

                if male_present then
                begin
                IC_rep_prob := rep_prob*(1+(IC_eff_rep*(0.5-Individual^.IC)));
                rand_val := random;
                   if rand_val < IC_rep_prob then
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
                      {setLength(Individual^.Genome, 25, 2);
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
                      }

                      {Inbreeding calculations}
                      // Add new individual to Famtree
                      SetLength(Famtree, Length(Famtree) + 1, 4);
                      Famtree[Individual^.UniqueID, 0] := Individual^.UniqueID;
                      Famtree[Individual^.UniqueID, 2] := father_ID;
                      Famtree[Individual^.UniqueID, 3] := mother_ID;

                      //Only one kitten gets CA and IC calculation
                      if ls = 1 then
                      begin
                      IC_kittens := CalculateIC(Individual^.UniqueID, Famtree);
                      end;

                      Individual^.IC:= IC_kittens;
                      Famtree[Individual^.UniqueID, 1] := IC_kittens;

                      Population.add(Individual);

                    end;
                  end;

                end;
              end;

      end;
  end;
end;

function CalculateIC(child_ID: integer; Famtree: Array2Dreal): real;
var
  CA: Array2Dinteger;
  a: integer;
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
  surv_p, surv_day, daily_mortality_p, IC_surv_prob: real;
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
      end;

      if (IC_eff_kittens>0) and (Individual^.Status=0)then
        IC_surv_prob := surv_cub * (1 + IC_eff_kittens *(0.5 - Individual^.IC))
      else
        IC_surv_prob := surv_day*(1+(IC_eff_surv*(0.5-Individual^.IC)));

      {Determine fate of individuals}
      die := False;
      if Individual^.age > max_age then die := True
      else
        if random > IC_surv_prob then die := True;
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
  a, b, d, e, f, g, i, j, new_dir, TestCoordX, TestCoordY, TCount, first_Tcount, xi, yi, xy : integer;
  age_m, P_disp_start: real;
  temp_terrX, temp_terrY: array of integer;
  temp_ind: PAgent;
  test_cell_available, c_available, already_terr: boolean;

begin
  with population do
  begin
    populationsize := population.Count;
    for a := 0 to populationsize - 1 do

    begin
      Individual := items[a];

      //Extra debug information
      if Individual = nil then WriteLn('Individual index ' + IntToStr(a) + 'doesnt have an individual assigned');

      {If the individual is a subadult determine if it starts dispersing}
      if (Individual^.Status = 0) and (Individual^.Age > 0) then
      begin
        //WriteLn('See if individual starts dispersing');
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
        //WriteLn('Individual is early settler, check if it has enough territory');
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
        begin
        //WriteLn('Not enough territory, claiming other free cells if possible, otherwise restart dispersal');
        ClaimNewTerrOrStartDispersal;
        end;
      end;

      {Now start dispersal IF individual has dispersal status}

      if (Individual^.Status = 1) then
      begin
        //WriteLn('Individual has status 1, starting walking');
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
            //WriteLn('Individual walked into breeding habitat, checking to see if territory can be claimed');
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
                  //WriteLn('Looking further outside immediate circle to find enough cells');
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
                    //WriteLn('Enough found, removing selected cells from others if needed');
                    for xy := 0 to TCount - 1 do
                      begin

                        with population do
                        begin
                          for d := 0 to population.Count - 1 do
                          begin
                            temp_ind := Items[d];
                            if temp_ind^.Status > 1 then
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
                    //WriteLn('Changing status of individual and assigning territory cells');
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
type
  // Define a record to store ancestor ID and distance
  TAncestorInfo = record
    ID: Integer;
    Steps: Integer;
  end;

  // Fully specialized generic types
  TAncestorQueue = specialize TQueue<TAncestorInfo>;
  TAncestorDict = specialize TDictionary<Integer, Integer>;
  TPointList = specialize TList<TPoint>;
var
  i, ancestorID, steps, minSteps: integer;
  qMother, qFather: TAncestorQueue;
  visitedMother, visitedFather: TAncestorDict;
  commonAncestors: TPointList;
  current: TAncestorInfo;
  closestAncestors: Array2Dinteger;
begin
  // Create data structures
  qMother := TAncestorQueue.Create;
  qFather := TAncestorQueue.Create;
  visitedMother := TAncestorDict.Create;
  visitedFather := TAncestorDict.Create;
  commonAncestors := TPointList.Create;

  try
    // Initialize with parents (ID, steps)
    if (child_ID >= 0) and (child_ID < Length(Famtree)) then
    begin
      // Add father to father's queue
      ancestorID := Round(Famtree[child_ID, 2]);
      if ancestorID <> -1 then
      begin
        current.ID := ancestorID;
        current.Steps := 1;
        qFather.Enqueue(current);
        visitedFather.Add(ancestorID, 1);
      end;

      // Add mother to mother's queue
      ancestorID := Round(Famtree[child_ID, 3]);
      if ancestorID <> -1 then
      begin
        current.ID := ancestorID;
        current.Steps := 1;
        qMother.Enqueue(current);
        visitedMother.Add(ancestorID, 1);
      end;
    end;

    // Initialize minSteps before use
    minSteps := MaxInt;

    // Process mother's side ancestors with breadth-first search
    while qMother.Count > 0 do
    begin
      current := qMother.Dequeue;
      ancestorID := current.ID;
      steps := current.Steps;

      // Check if this ancestor is already found on father's side
      if visitedFather.ContainsKey(ancestorID) then
        commonAncestors.Add(TPoint.Create(ancestorID, steps + visitedFather[ancestorID]));

      // Only continue if we haven't found common ancestors yet or need more with same distance
      if (commonAncestors.Count = 0) or
         ((steps <= minSteps) and (ancestorID < Length(Famtree)) and (ancestorID <> -1)) then
      begin
        // Add father of current ancestor
        if (ancestorID < Length(Famtree)) then
        begin
          ancestorID := Round(Famtree[ancestorID, 2]);
          if (ancestorID <> -1) and not visitedMother.ContainsKey(ancestorID) then
          begin
            current.ID := ancestorID;
            current.Steps := steps + 1;
            qMother.Enqueue(current);
            visitedMother.Add(ancestorID, steps + 1);
          end;

          // Reset ancestorID to current.ID for mother lookup
          ancestorID := current.ID;

          // Add mother of current ancestor
          if (ancestorID < Length(Famtree)) then
          begin
            ancestorID := Round(Famtree[ancestorID, 3]);
            if (ancestorID <> -1) and not visitedMother.ContainsKey(ancestorID) then
            begin
              current.ID := ancestorID;
              current.Steps := steps + 1;
              qMother.Enqueue(current);
              visitedMother.Add(ancestorID, steps + 1);
            end;
          end;
        end;
      end;
    end;

    // Process father's side ancestors with breadth-first search
    while qFather.Count > 0 do
    begin
      current := qFather.Dequeue;
      ancestorID := current.ID;
      steps := current.Steps;

      // Check if this ancestor is already found on mother's side
      if visitedMother.ContainsKey(ancestorID) then
        commonAncestors.Add(TPoint.Create(ancestorID, steps + visitedMother[ancestorID]));

      // Only continue if we haven't found common ancestors yet or need more with same distance
      if (commonAncestors.Count = 0) or
         ((steps <= minSteps) and (ancestorID < Length(Famtree)) and (ancestorID <> -1)) then
      begin
        // Add father of current ancestor
        if (ancestorID < Length(Famtree)) then
        begin
          ancestorID := Round(Famtree[ancestorID, 2]);
          if (ancestorID <> -1) and not visitedFather.ContainsKey(ancestorID) then
          begin
            current.ID := ancestorID;
            current.Steps := steps + 1;
            qFather.Enqueue(current);
            visitedFather.Add(ancestorID, steps + 1);
          end;

          // Reset ancestorID to current.ID for mother lookup
          ancestorID := current.ID;

          // Add mother of current ancestor
          if (ancestorID < Length(Famtree)) then
          begin
            ancestorID := Round(Famtree[ancestorID, 3]);
            if (ancestorID <> -1) and not visitedFather.ContainsKey(ancestorID) then
            begin
              current.ID := ancestorID;
              current.Steps := steps + 1;
              qFather.Enqueue(current);
              visitedFather.Add(ancestorID, steps + 1);
            end;
          end;
        end;
      end;
    end;

    // Find minimum distance if any common ancestors were found
    if commonAncestors.Count > 0 then
    begin
      minSteps := MaxInt;
      for i := 0 to commonAncestors.Count - 1 do
        if commonAncestors[i].Y < minSteps then
          minSteps := commonAncestors[i].Y;

      // Count ancestors with minimum distance
      steps := 0;
      for i := 0 to commonAncestors.Count - 1 do
        if commonAncestors[i].Y = minSteps then
          Inc(steps);

      // Create result array
      SetLength(closestAncestors, steps);
      steps := 0;

      for i := 0 to commonAncestors.Count - 1 do
        if commonAncestors[i].Y = minSteps then
        begin
          SetLength(closestAncestors[steps], 2);
          closestAncestors[steps][0] := commonAncestors[i].X;
          closestAncestors[steps][1] := commonAncestors[i].Y;
          Inc(steps);
        end;

      Result := closestAncestors;
    end
    else
      Result := nil;

  finally
    // Clean up
    qMother.Free;
    qFather.Free;
    visitedMother.Free;
    visitedFather.Free;
    commonAncestors.Free;
  end;
end;

end.

