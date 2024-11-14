unit lynx_population_dynamics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  lynx_define_units, lynx_vital_rates, general_functions, lynx_input_output_functions;

procedure Startpopulation;
procedure Pop_dynamics;
procedure RunPopSim;

implementation

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

procedure Pop_dynamics;
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

        if day = 90 then
          if populationsize > 2 then
            Reproduction;               // Reproduction happens at the end of March

        populationsize := population.Count;

        Dispersal(day);                 // Dispersal of surviving individuals (also includes dispersion start for subadults)
        Survival;                       // Determine which individuals survive this day

        populationsize := population.Count;

        UpdateAbundanceMap;
      end;

      WritePopulationToCSV(population, 'output_data/Yearly_census_ind.csv', current_sim, a);

      if (a mod 5 = 0) then
        begin
        WriteMapCSV('output_data/FemalesMap_status_' + IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Femalesmap, MapdimX, MapdimY, 0);
        WriteMapCSV('output_data/FemalesMap_age_' + IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Femalesmap, MapdimX, MapdimY, 1);
        WriteMapCSV('output_data/MalesMap_status_' + IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Malesmap, MapdimX, MapdimY, 0);
        WriteMapCSV('output_data/MalesMap_age_' +  IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Malesmap, MapdimX, MapdimY, 1);
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

      pop_size[a] := populationsize;
      sum_pop_size[a] := sum_pop_size[a] + populationsize;

    end;
  end;
end;

procedure RunPopSim;
var
  a,b: integer;
begin

  randomize; {initialize the pseudorandom number generator}
  ReadParameters(paramname);
  readmap(mapname);

  SetLength(MalesMap, Mapdimx + 1, Mapdimy + 1, 2);
  SetLength(FemalesMap, Mapdimx + 1, Mapdimy + 1, 2);

  AssignFile(to_file_out, file_name);
  rewrite(to_file_out); {create txt file}

  for a := 1 to max_years do sum_pop_size[a] := 0;
  for a := 1 to max_years do n_sim_no_ext[a] := 0;

  // Calculate array of step probabilities (here once) to be used in dispersal procedure later
  Step_probabilities;

  for current_sim := 1 to n_sim do
  begin
    WriteLn('Starting simulation ', current_sim);

    max_pop_size := 0;
    Startpopulation; {call the procedure to initialize your population}
    Pop_dynamics;    {call the procedure to run the population dynamics}

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

    WriteLn('Done with simulation ', current_sim);
end;

end;



end.

