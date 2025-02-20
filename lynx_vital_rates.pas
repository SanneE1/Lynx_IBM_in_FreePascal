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
Function ReproductionQuality(x,y:integer):boolean;

function MoveDir: integer;

function FindTerrOwner(population: TList; targetSex: string; targetX, targetY: word): PAgent;
function Fight(AgeDisperser, AgeEarlySettler: integer; Sex: string): boolean;
function TerritoryCellAvailable(x,y: integer; Sex:string; disperser_age: integer): boolean;
procedure ClaimNewTerrOrStartDispersal;
function CalculateIC(child_ID: integer; Famtree:Array2Dinteger): real;
function FindClosestCommonAncestors(Famtree: Array2Dinteger; child_ID: integer): Array1Dinteger;

implementation


procedure Reproduction;
var
  a, g, i, k, current_litter_size, ls, xy, male_x, male_y, homogeneity_count: integer;
  rep_prob, tmic,IC_kittens : real;
  temp_X, temp_Y, Temp_mem: word;
  male_present: boolean;
  PotentialFather: PAgent;
  father, mother: array of array of integer;
  mother_ID, father_ID, selected_kitten: integer;
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
                    male_x := Individual^.TerritoryX[xy];
                    male_y := Individual^.TerritoryY[xy];
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
                end;
                end;


                if male_present then
                  if random < rep_prob then
                  begin
                    current_litter_size := Round(randg(litter_size, litter_size_sd));
                    mother_ID := Individual^.UniqueID;   //we put it ere because then we skip to ind to the cub and not the mother anymore. Moreover,it makes sense to inset the mother UniqueID just if we have already a father
                    IC_kittens:= -1;       //se qualcosa va male ti da -1 ome IC, e non quella del before litter


                    //Save location of the mother, to give to offspring
                    Temp_X := Individual^.Coor_X;
                    Temp_Y := Individual^.Coor_Y;
                    Temp_mem := Individual^.mov_mem;

                   // selected_kitten:= Random (current_litter_size) + 1;

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
                      Individual^.UniqueID := UniqueIDnext; //sice is a kitten that is a new individual that need to be added to population
                      UniqueIDnext:= UniqueIDnext+1;

                      //if UniqueIDnext > 500 then
                      //Exit;

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

                      //Add uniqueID to Individual

                      //create the genome 0.5--> per ogni locus della mamma ed ogni coppia di allele, il kitten può ereditare ognuno dei due possibiliy alleli che sono 0 o 1.
                      //prima: quando l'ind ha il gene calcoliamo la percentuale di homogeneity e la diamo ad ogni individuo
                      //add the 0.5
                      //madre-----
                      setLength(Individual^.Genome, 25, 2);
                      homogeneity_count := 0;


                      for i := 1 to 24 do
                      begin
                        tmic:=random;   //sceglie nbumero tra 0 e 1
                        if tmic < 0.5 then
                           Individual^.Genome[i, 0] := mother[i, 0]     //magari non c'è bisogno di specificare che si tratta di micropadre o madre
                        else
                           Individual^.Genome[i, 0] := mother[i, 1];     //non importa quale prendi sarà sempre nella stessa posizione. ma proviene dall allele della mamma 1

                       //padre                                      //gli fa dire se tmic minore di 0.5 Fai che l'individual prenda da genome l'allele 0.

                        tmic := random;
                        if tmic < 0.5 then
                           Individual^.Genome[i, 1] := father[i, 0]
                        else
                           Individual^.Genome[i, 1] := father[i, 1];

                      //check for homogeneity                 //se il primo individuo^genome è diver
                      if Individual^.Genome[i, 0] = Individual^.Genome[i, 1] then   //ma allora non c'è bisogno //appunto dentro Individual^Genome [i,k]= è 1,2,3 o 4.
                      homogeneity_count := homogeneity_count + 1;
                      end;

                      //ratio of homogeneity
                       Individual^.P_homogeneity := homogeneity_count / 24.0;    //potrei usare questo per la csv percental of homogeneity??


                       {Inbreeding calculations}
                       // Aggiungere nuovo individuo al Famtree
                      SetLength(Famtree, Length(Famtree) + 1, 4);
                      Famtree[High(Famtree), 0] := Individual^.UniqueID;  // UniqueID del nuovo individuo
                      Famtree[High(Famtree), 1] := 0;  // Coefficiente di inbreeding
                      Famtree[High(Famtree), 2] := father_ID; // FatherID
                      Famtree[High(Famtree), 3] := mother_ID; // MotherID


                    // Individual^.IC:= CalculateIC(Individual^.UniqueID, Famtree);

                    //Only one kitten gets CA and IC calculation
                    if ls = 1 then IC_kittens := CalculateIC(Individual^.UniqueID, Famtree);

                    Individual^.IC:= IC_kittens;  //prende il primo ind della litter e ci calcola l'IC, e dice che per ogni altro kittel l'IC e lo stesso


                     // if ls=1 then
                     // begin
                     //   Individual^.IC:= CalculateIC(Individual^.UniqueID, Famtree);
                     //   end
                     // else
                     // begin
                     //   Individual^.IC:=0;
                     // end;
                       Population.add(Individual);

                      //Add new individual's ID to Famtree[UniqueID, 0]
                      //Add FatherID to Famtree[UniqueID,2]
                      //Add MotherID to Famtree[UniqueID,3]


                    end;
                  end;
              end;
      end;
  end;
end;
// Placeholder per la funzione CalculateIC
function CalculateIC(child_ID: integer; Famtree:Array2Dinteger): real;
var
  CA: Array1Dinteger;
  Steps: integer;
begin
  // Logica per calcolare IC
  setLength(CA, 2);

  CA := FindClosestCommonAncestors(Famtree,child_ID);
  Steps:=CA[1];
 //common anc with number of steps and add here the ic calculation
  Result := power (0.5, Steps)+ Power(0.5, Steps); // Modifica questa parte con il calcolo reale     prima: Result := 0.0
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

  if (mapname = 'input_data/old_donana.txt') then
  begin
  if ((x >= 122) and (x <= 135)) then
  if((y >= 85) and (y <= 115)) then Result:=True;

  if ((x >= 140) and (x <= 147)) then
  if ((y >=140) and (y <= 146)) then Result:= True;

  if ((x >= 135) and (x <= 141)) then
  if ((y >= 75) and (y <= 82)) then Result:= True;
  end
else
   ShowErrorAndExit('No in-park areas defined for this map file');

end;

Function ReproductionQuality(x,y:integer):boolean;

begin
  Result:=False;

  if (mapname = 'input_data/old_donana.txt') then
  begin
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

  if ((x >= 151) and (x <= 156)) then
  if ((y >= 67) and (y <= 74)) then Result:=True;

  if ((x >= 138) and (x <= 142)) then
  if ((y >= 37) and (y <= 41)) then Result:=True;

  end else
  ShowErrorAndExit('No reproduction quality areas defined for this map file');


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


function FindClosestCommonAncestors(Famtree: Array2Dinteger; child_ID: integer): Array1Dinteger;

var
  ID, i, j, q, t, min_steps: integer;
  common_ancestors, queue_mother, queue_father, visited_mother, visited_father: array of array of integer;
  closest_ancestors : array of integer;
  current_mother, current_father: array[0..1] of integer;
  Steps: integer;


begin
  // Inizializza la coda della madre
  SetLength(queue_mother, 1, 2);
  queue_mother[0,0] := Famtree[child_ID, 3]; // Madre   //elemento della riga 0 (primo elemento della coda) e alla colonna 0 (rappresenta l'ID dell'individuo, la mamma del figlio childID)
  queue_mother[0,1] := 1;                               //del primo elemento della coda (prima mamma), il numero di steps = 1

  // Inizializza la coda del padre
  SetLength(queue_father, 1, 2);
  queue_father[0,0] := Famtree[child_ID, 2]; // Padre
  queue_father[0,1] := 1;

  // Espansione degli antenati della madre  //we need more generation with smaller population t see the model.
///////////////////////////////////////////
  while Length(queue_mother) > 0 do
  begin
    current_mother[0] := queue_mother[High(queue_mother),0];  //e l'ID del nodo della madre che stiamo analizzando estratto dala coda queue_mother
    current_mother[1] := queue_mother[High(queue_mother),1];

    SetLength(queue_mother, Length(queue_mother) - 1);        //corretto che qui diventa nil quando tolgo un elemento da queue_mother

    // Aggiungi ai visitati
    SetLength(visited_mother, Length(visited_mother) + 1, 2);
    visited_mother[High(visited_mother),0] := current_mother[0];
    visited_mother[High(visited_mother),1] := current_mother[1];

    // Espandi ai genitori del nodo corrente
    if Famtree[current_mother[0], 2] <> -1 then    //dall'ID della current mother (0) prendi il papa dall famtree, verifica se il padre del nodo corrente (current mother) esiste nel famtree
    begin
      SetLength(queue_mother, Length(queue_mother) + 1,2);     //aggiungi una nuova riga alla coda queue mother per includere il padre del nodo corrente
      queue_mother[High(queue_mother),0] := Famtree[current_mother[0], 2];  //l'ID del padre corrisponde/viene messo come ultimo elemento della matrice bidimensionale sui antenati della mamma del childID
      queue_mother[High(queue_mother),1] := current_mother[1] + 1;          //viene agginto il numero di steps necessari a raggiungere il padre della current mother
    end;

    if Famtree[current_mother[0], 3] <> -1 then                            //stessa cosa: per vedere se la madre ha una madre
    begin
      SetLength(queue_mother, Length(queue_mother) + 1,2);                 // se c'e una madre, aggiungi spazio alla queue_mother
      queue_mother[High(queue_mother),0] := Famtree[current_mother[0], 3]; //aggiungi al posto libero in queue other sia l'ID della madre della madre
      queue_mother[High(queue_mother),1] := current_mother[1] + 1;         //e il numero di steps che corrisponde a quello precedente +1
    end;
  end;

  // Espansione degli antenati del padre
  while Length(queue_father) > 0 do
  begin
    current_father[0] := queue_father[High(queue_father),0];
    current_father[1] := queue_father[High(queue_father),1];
    SetLength(queue_father, Length(queue_father) - 1);

    // Aggiungi ai visitati
    SetLength(visited_father, Length(visited_father) + 1,2);
    visited_father[High(visited_father),0] := current_father[0];
    visited_father[High(visited_father),1] := current_father[1];

    // Espandi ai genitori del nodo corrente // se non mi trova nessun common ancestor vado a famtree
    if Famtree[current_father[0], 2] <> -1 then         // c'e il papa del papa? se si begin
    begin
      SetLength(queue_father, Length(queue_father) + 1,2);  //fai spazio nella queue_father
      queue_father[High(queue_father),0] := Famtree[current_father[0], 2 ];    //inserisci in queue_father l'ID del papa e gli steps
      queue_father[High(queue_father),1] := current_father[1] + 1;
    end;

    if Famtree[current_father[0], 3] <> -1 then
    begin                                                                  //stessa roba per la mamma del papa
      SetLength(queue_father, Length(queue_father) + 1,2);
      queue_father[High(queue_father),0] := Famtree[current_father[0], 3];
      queue_father[High(queue_father),1] := current_father[1] + 1;
    end;
  end;

  // Trova i Common Ancestors
  SetLength(common_ancestors, 0);
  for i := 0 to Length(visited_mother) - 1 do
    for j := 0 to Length(visited_father) - 1 do
      if visited_mother[i,0] = visited_father[j,0] then
      begin
        SetLength(common_ancestors, Length(common_ancestors) + 1, 2);
        common_ancestors[High(common_ancestors),0] := visited_mother[i,0];     //0 e common ancor father
        common_ancestors[High(common_ancestors),1] :=                           //steps
          visited_mother[i,1] + visited_father[j,1];
      end;

  // Trova il numero minimo di step
  min_steps := MaxInt;
  for q := 0 to Length(common_ancestors) - 1 do
    if common_ancestors[q,1] < min_steps then
      min_steps := common_ancestors[q,1];

  // Filtra i Common Ancestors con il minimo numero di step
  SetLength(closest_ancestors, 2);
  for t := 0 to Length(common_ancestors) - 1 do
    if common_ancestors[t,1] = min_steps then
    begin
      SetLength(closest_ancestors, Length(closest_ancestors) + 1);
      closest_ancestors[0] := common_ancestors[t,0];
      closest_ancestors[1] := common_ancestors[t,1];
    end;

  // Restituisci il risultato
  Result := closest_ancestors;
end;

//function CalculateIC(mother_ID, father_ID: integer): real;
//var
//a, b, c,d, common_anc: array of integer;
//x,y: integer;
//ancestor_found: boolean;
//begin

//  ancestor_found := false;

//  setLength(a, 2);
//  setLength(b, 2);
//  setLength(common_anc, 2);

//  a[0] := Famtree[mother_ID,2];    //gran mother of the son or mother of motherID
//  a[1] := Famtree[mother_ID,3];

//  b[0] := Famtree[father_ID, 2];
//  b[1] := Famtree[father_ID, 3];

 // c[] := a[0];  //??
 // c[] := a[1];  //?? whyy
//  c[] := Famtree[a[0], 2];       //mother of the mother of the motherID(granma) or grangranmother of son
//  c[] := Famtree[a[0], 3];
//  c[] := Famtree[a[1], 2];
//  c[] := Famtree[a[1], 3];

//  d[] := Famtree[b[0], 2];       //mother of the mother of the motherID(granma) or grangranmother of son
//  d[] := Famtree[b[0], 3];
//  d[] := Famtree[b[1], 2];
//  d[] := Famtree[b[1], 3];

//  for x := 0 to 1 do
//  for y := 0 to 1 do
//  begin
//    if a[x] = b[y] then
//    begin
//    common_anc := c;
//    ancestor_found := true;
//    end;
//  end;

//  Result := common_anc;    /// This still needs to be changed to actual IC later

//end;

end.

