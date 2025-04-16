unit lynx_population_dynamics;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  lynx_define_units, lynx_vital_rates, general_functions, lynx_input_output_functions, lynx_dispersal_assist_functions;

procedure Startpopulation;
procedure Pop_dynamics;
procedure RunPopSim;

implementation

procedure Startpopulation;
var
  a,b, i, k, Tcheck, xy, N, X, Y: integer;
  lineData: TStringList;
  tmic: real;
  popFile: TextFile;
  popName: string;
begin

  Population := TList.Create;
  lineData := TStringList.Create;

//Create/initiate the Famtree (array of array)
  SetLength(Famtree, 1);

  //Initialization of UniqueID at 0 (the first ind will have an ID of 0)
  UniqueIDnext:= 0;

  WriteLn('Creating start population');

  AssignFile(popFile, start_pop_file);
  reset(popFile);

  with population do
  begin
    while not Eof(popFile) do
    begin
      ReadLn(popFile, popName);

      if(Pos('N', popName) = 1) then Continue;

      LineData.Delimiter := ' ';
      lineData.DelimitedText := popName;

      N := StrToIntDef(lineData[0], 0);
      X := StrToIntDef(lineData[1], 0);
      Y := StrToIntDef(lineData[2], 0);

      SetLength(Famtree, Length(Famtree) + N, 4);

      for a := 1 to N do
      begin
      new(Individual);

      Individual^.age := random(3) + 3;   {alternative: Individual^.age:=0; }

      if random < 0.5 then Individual^.sex := 'f'
      else
        Individual^.sex := 'm';
        Individual^.status := 1;

        Individual^.UniqueID := UniqueIDnext;
        Individual^.IC :=0;

        Individual^.Coor_X := X;
        Individual^.Coor_Y := Y;

      Individual^.Natal_pop := whichPop(Individual^.Coor_X, Individual^.Coor_Y);
      Individual^.Current_pop := whichPop(Individual^.Coor_X, Individual^.Coor_Y);
      Individual^.Previous_pop := whichPop(Individual^.Coor_X, Individual^.Coor_Y);

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

      setLength(Individual^.Genome, 25);
      for i := 0 to High(Individual^.Genome) do setLength(Individual^.Genome[i], 2);

      for i := 1 to 24 do
      begin
        for k := 0 to 1 do
        begin
          tmic := random;
          if tmic < 0.25 then
            Individual^.Genome[i, k] := 1
          else if tmic < 0.5 then
            Individual^.Genome[i, k] := 2
          else if tmic < 0.75 then
            Individual^.Genome[i, k] := 3
          else
            Individual^.Genome[i, k] := 4;
          end;
        end;

      Population.add(Individual);

      Famtree[Individual^.UniqueID,0]:=Individual^.UniqueID;  //UniqueID
      Famtree[Individual^.UniqueID,1]:=0;                     //IC
      Famtree[Individual^.UniqueID,2]:= -1;                   //FatherID
      Famtree[Individual^.UniqueID,3]:= -1;                   //MotherID

      UniqueIDnext:= UniqueIDnext+1


    end;
  end;
    
    WriteLn('Beginning population size = ', Population.Count);
    {Go through some dispersal cycles, to get individuals settled}
    WriteLn('Starting ', n_cycles, ' initial dispersal cycles');

    with population do
    for a := 1 to n_cycles do
    begin

      dispersal(a);

      for b := 0 to population.count - 1 do
      begin
      Individual := Items[b];

      if (Individual^.Status = 2) then
      if (Individual^.Age < max_rep_age) then
      begin
          Tcheck := 0;
          for xy := 0 to Tsize - 1 do
          if ((Individual^.TerritoryX[xy] > 0) and (Individual^.TerritoryY[xy] > 0)) then
          Tcheck := Tcheck + 1;

          if Tcheck = Tsize then
          Individual^.Status := 3;

      end;

      UpdateAbundanceMap;

    end;

  end;
  end;

end;


procedure Pop_dynamics;
var
  a, b, xy, day, Tcheck: integer;
begin
 with population do
  begin
    WriteLn('Starting population dynamics');

    for a := 1 to max_years do
    begin
      day := 0;  // Start the year
      current_year := a;

      writeln('Simulation year ', current_year);

      while (day < 366) and (populationsize > 0) do //Let's pretend there's no such thing as leap years
      begin
        day := day + 1;
        populationsize := population.Count;

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
          if ((Individual^.TerritoryX[xy] > 0) and (Individual^.TerritoryY[xy] > 0)) then Tcheck := Tcheck + 1;

          if Tcheck = Tsize then
          begin

          Individual^.Status := 3;

          if (Individual^.natal_pop <> Individual^.Current_pop) then
          begin
          new(MigrationEvent);
          MigrationEvent^.simulation := current_sim;
          MigrationEvent^.year := current_year;
          MigrationEvent^.sex := Individual^.Sex;
          MigrationEvent^.age := Individual^.Age;
          MigrationEvent^.natal_pop := Individual^.Natal_pop;
          MigrationEvent^.old_pop := Individual^.Previous_pop;
          MigrationEvent^.new_pop := Individual^.Current_pop;
          SettledList.Add(MigrationEvent);
          end;

          end
          else
          begin
            {Reset individual to disperser and empty territory info}
            Individual^.Status := 1;
            for xy := 0 to length(Individual^.TerritoryX) - 1 do
          begin
            if (Individual^.TerritoryX[xy] = -1) then Continue;
            Individual^.TerritoryX[xy] := -1;
            Individual^.TerritoryY[xy] := -1;
            end;
          end;
        end;

          each_pop_sizes[Individual^.current_pop, current_year] := each_pop_sizes[Individual^.current_pop, current_year] + 1;
          each_pop_IC[Individual^.current_pop, current_year] := each_pop_IC[Individual^.current_pop, current_year] + Individual^.IC;
          pop_IC[current_year] := pop_IC[current_year] + Individual^.IC;

        end;
      end;

      UpdateAbundanceMap;

   {if (a mod 5 = 0) then
        begin
        WriteMapCSV('output_data/FemalesMap_status_' + IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Femalesmap, MapdimX, MapdimY, 0);
        WriteMapCSV('output_data/FemalesMap_age_' + IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Femalesmap, MapdimX, MapdimY, 1);
        WriteMapCSV('output_data/MalesMap_status_' + IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Malesmap, MapdimX, MapdimY, 0);
        WriteMapCSV('output_data/MalesMap_age_' +  IntToStr(current_sim) + '_' + IntToStr(a) + '_' + IntToStr(day) + '.csv', Malesmap, MapdimX, MapdimY, 1);
        end;


    WritePopulationToCSV(population, 'output_data/Population_data.csv', current_sim, current_year);}
    end;

  end;
end;

procedure RunPopSim;
var
  a,b, i, ix, iy: integer;
begin

  randomize; {initialize the pseudorandom number generator}
  ReadParameters(paramname);
  readmap(mapname, mapBHname, mapPops);

  WriteLn('Running ', n_sim, ' lynx population simulation(s) for ', max_years, ' years');
  WriteLn('Using parameter file: ', paramname);
  WriteLn('Map file name: ', mapname);

  output_dir := 'output_data_' + FloatToStr(IC_eff_surv) + '_' + FloatToStr(IC_eff_rep) + '_' + FloatToStr(IC_eff_kittens);
  if not DirectoryExists(output_dir) then
    MkDir(output_dir);

  SetLength(MalesMap, Mapdimx + 1, Mapdimy + 1, 2);
  SetLength(FemalesMap, Mapdimx + 1, Mapdimy + 1, 2);
  SetLength(ConnectionMap, Mapdimx + 1, Mapdimy + 1, 2);

  SetLength(pop_size, max_years + 1);
  SetLength(pop_IC, max_years + 1);
  SetLength(sum_pop_size, max_years + 1);

  for a := 1 to max_years do sum_pop_size[a] := 0;
  for a := 1 to max_years do n_sim_no_ext[a] := 0;

  SetLength(each_pop_sizes, 6);
  SetLength(each_pop_IC, 6);
  for i := 0 to High(each_pop_sizes) do
  begin
    SetLength(each_pop_sizes[i], max_years+1);
    SetLength(each_pop_IC[i], max_years+1);
  end;

  // Calculate array of step probabilities (here once) to be used in dispersal procedure later
  Step_probabilities;

  MigrationList := TList.Create;
  SettledList := Tlist.Create;

  for current_sim := 1 to n_sim do
  begin
    WriteLn('Starting simulation ', current_sim);

    max_pop_size := 0;
    Startpopulation; {call the procedure to initialize your population}
    Pop_dynamics;    {call the procedure to run the population dynamics}

    {save the results to a text file}
    AssignFile(to_file_out, output_dir + PathDelim + 'PopulationSizes_' + IntToStr(taskID) + '.csv');
    rewrite(to_file_out); {create txt file}
    writeln(to_file_out, 'year,tot_size,tot_IC,size_0,IC_0,size_1,IC_1,size_2,IC_2,size_3,IC_3,size_4,IC_4,size_5,IC_5');

    for b := 1 to max_years do
    begin
      write(to_file_out, b, ',', pop_size[b], ',', pop_IC[b]:0:5, ',', each_pop_sizes[0,b], ',');
      if each_pop_sizes[0,b] > 0 then write(to_file_out, (each_pop_IC[0,b]/each_pop_sizes[0,b]):0:5, ',') else
        write(to_file_out, 'NA', ',');

      write(to_file_out, each_pop_sizes[1,b], ',');
      if each_pop_sizes[1,b]  > 0 then write(to_file_out, (each_pop_IC[1,b]/each_pop_sizes[1,b]):0:5, ',') else
        write(to_file_out, 'NA', ',');

      write(to_file_out, each_pop_sizes[2,b], ',');
      if each_pop_sizes[2,b]  > 0 then write(to_file_out, (each_pop_IC[2,b]/each_pop_sizes[2,b]):0:5, ',') else
        write(to_file_out, 'NA', ',');

      write(to_file_out,each_pop_sizes[3,b], ',');
      if each_pop_sizes[3,b]  > 0 then write(to_file_out, (each_pop_IC[3,b]/each_pop_sizes[3,b]):0:5, ',')  else
        write(to_file_out, 'NA', ',');

      write(to_file_out,each_pop_sizes[4,b], ',');
      if each_pop_sizes[4,b]  > 0 then write(to_file_out, (each_pop_IC[4,b]/each_pop_sizes[4,b]):0:5, ',') else
        write(to_file_out, 'NA', ',');

      write(to_file_out,each_pop_sizes[5,b], ',');
      if each_pop_sizes[5,b]  > 0 then writeln(to_file_out, (each_pop_IC[5,b]/each_pop_sizes[5,b]):0:5) else
        writeln(to_file_out, 'NA');
    end;

    CloseFile(to_file_out);

    {Write Migration list to file}
    AssignFile(mig_file_out, output_dir + PathDelim + 'migration_' + IntToStr(taskID) + '.csv');
    rewrite(mig_file_out); {create txt file}
    writeln(mig_file_out, 'EventID,Simulation,Year,Sex,Age,Natal_pop,Old_pop,New_pop');

    with MigrationList do
    for b := 0 to MigrationList.Count - 1 do
    begin
      MigrationEvent := items[b];

      writeln(mig_file_out, b , ',', MigrationEvent^.simulation, ',',
      MigrationEvent^.year, ',',
      MigrationEvent^.sex, ',',
      MigrationEvent^.age, ',',
      MigrationEvent^.natal_pop, ',',
      MigrationEvent^.old_pop, ',',
      MigrationEvent^.new_pop);
    end;
    CloseFile(mig_file_out);

    {Write Settled migrants to file}
    AssignFile(migS_file_out, output_dir + PathDelim + 'migrationSettled_' + IntToStr(taskID) + '.csv');
    rewrite(migS_file_out); {create txt file}
    writeln(migS_file_out, 'EventID,Simulation,Year,Sex,Age,Natal_pop,Old_pop,New_pop');

    with SettledList do
    for b := 0 to SettledList.Count - 1 do
    begin
      MigrationEvent := items[b];

      writeln(migS_file_out, b , ',', MigrationEvent^.simulation, ',',
      MigrationEvent^.year, ',',
      MigrationEvent^.sex, ',',
      MigrationEvent^.age, ',',
      MigrationEvent^.natal_pop, ',',
      MigrationEvent^.old_pop, ',',
      MigrationEvent^.new_pop);
    end;
    CloseFile(migS_file_out);

  // Loop over the ConnectionMap and write to file for both female and male
  AssignFile(connection_F_out, output_dir + PathDelim + 'ConnectionMapF_' + IntToStr(taskID) + '.csv');
  rewrite(connection_F_out); {create txt file}
  for iy := 1 to MapDimY do
  begin
    for ix := 1 to MapDimX do
    begin
      if ix < MapDimX then
        Write(connection_F_out, ConnectionMap[ix, iy, 0], ',')
      else
        WriteLn(connection_F_out, ConnectionMap[ix, iy, 0]);
    end;
  end;
  CloseFile(connection_F_out);

  // Loop over the ConnectionMap and write to file for both female and male
  AssignFile(connection_M_out, output_dir + PathDelim + 'ConnectionMapM_' + IntToStr(taskID) + '.csv');
  rewrite(connection_M_out); {create txt file}
  for iy := 1 to MapDimY do
  begin
    for ix := 1 to MapDimX do
    begin
      if ix < MapDimX then
        Write(connection_M_out, ConnectionMap[ix, iy, 0], ',')
      else
        WriteLn(connection_M_out, ConnectionMap[ix, iy, 0]);
    end;
  end;
  CloseFile(connection_M_out);

  //WriteFamtreeToCSV(output_dir + PathDelim + 'Famtree_' + IntToStr(taskID) + '.csv');

  WriteLn('Done with simulation ', current_sim);
end;
end;



end.

