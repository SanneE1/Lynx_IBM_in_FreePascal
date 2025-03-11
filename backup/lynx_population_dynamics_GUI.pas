{}
unit lynx_population_dynamics_GUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Math, LCLType,            //Math not used
  lynx_define_units, general_functions, lynx_input_output_functions,
  lynx_vital_rates, lynx_dispersal_assist_functions;

type

{ Tspatial_Form }

Tspatial_Form = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Exit_Button: TButton;
    Abort_Button: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Run_Button: TButton;
    procedure Abort_ButtonClick(Sender: TObject);
    procedure Exit_ButtonClick(Sender: TObject);
    procedure Run_ButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Pop_dynamics_GUI;
  end;

var
  spatial_Form: Tspatial_Form;

implementation

{$R *.lfm}

{ Tspatial_Form }


procedure Startpopulation;
var
  a,b, i, k, Tcheck, xy: integer;
  tmic: real;
begin
  Population := TList.Create;

  //Create/initiate the Famtree (array of array)
  SetLength(Famtree, n_ini);

  //Initialization of UniqueID at 0 (the first ind will have an ID of 0)
  UniqueIDnext:= 0;

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
        Individual^.UniqueID := UniqueIDnext;

      if a < 75 then
      begin
        Individual^.Coor_X := 546;   // Donana for Peninsula
        Individual^.Coor_Y := 1568;
      end
      else if a < 103 then
      begin
        Individual^.Coor_X := 699;   // Matachel for Peninsula
        Individual^.Coor_Y := 1272;
      end
      else if a < 126 then
      begin
        Individual^.Coor_X := 1008;  // Montes de Toledo for Peninsula
        Individual^.Coor_Y := 1057;
      end
      else if a < 465 then
      begin
        Individual^.Coor_X := 1144;   // Sierra Morena for Peninsula
        Individual^.Coor_Y := 1369;
      end
      else
      begin
        Individual^.Coor_X := 364;   // Vale do Guadiana
        Individual^.Coor_Y := 1394;
      end;

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
      Individual^.IC :=0;

      Individual^.DailySteps := 0;
      Individual^.DailyStepsOpen := 0;

      setLength(Individual^.Genome, 25, 2);


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

      SetLength(Famtree, n_ini, 4);
      Famtree[Individual^.UniqueID,0]:=Individual^.UniqueID;  //UniqueID
      Famtree[Individual^.UniqueID,1]:=0;                     //IC
      Famtree[Individual^.UniqueID,2]:= -1;                   //FatherID
      Famtree[Individual^.UniqueID,3]:= -1;                   //MotherID


      UniqueIDnext:= UniqueIDnext+1


    end;

    {Go through some dispersal cycles, to get individuals settled}
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


procedure Tspatial_Form.Pop_dynamics_GUI;
var
  i, a, b, xy, day, Tcheck, current_sim: integer;
  sumIC: array[0..5] of real;
  countInd: array[0..5] of integer;
  avgIC: array[0..5] of real;
  ic_file_out: TextFile;

  begin
    AssignFile(ic_file_out, 'output_data/average_IC.csv');
    Rewrite(ic_file_out);
    writeln(ic_file_out, 'Simulation,Year,Pop0,Pop1,Pop2,Pop3,Pop4,Pop5');


  with population do
  begin
    for current_year := 1 to max_years do
    begin
      day := 0;  // Start the year
      while (day < 366) and (populationsize > 0) do //Let's pretend there's no such thing as leap years
      begin
        day := day + 1;
        populationsize := population.Count;

        if day = 90 then
          if populationsize > 2 then

          reproduction;                 // Reproduction happens at the end of March

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
         //  Aggiorna sumIC e countInd per il calcolo della media
          if (Individual^.Current_pop >= 0) and (Individual^.Current_pop <= 5) then
          begin
            sumIC[Individual^.Current_pop] := sumIC[Individual^.Current_pop] + Famtree[Individual^.UniqueID, 1];
            countInd[Individual^.Current_pop] := countInd[Individual^.Current_pop] + 1;
          end;
        end;
      end;

      //  Calcolo della media IC per popolazione
      for i := 0 to 5 do
      begin
        if countInd[i] > 0 then
          avgIC[i] := sumIC[i] / countInd[i]
        else
          avgIC[i] := 0;
      end;

      // Scrittura della media IC nel file CSV
      Append(ic_file_out);
      Write(ic_file_out, current_sim, ',', current_year, ',');
      for i := 0 to 5 do
      begin
        if i < 5 then
          Write(ic_file_out, avgIC[i]:0:4, ',')
        else
          WriteLn(ic_file_out, avgIC[i]:0:4);
      end;

      Flush(ic_file_out);

      UpdateAbundanceMap;

      {ploting pop size}
      if max_pop_size < populationsize then max_pop_size := populationsize;
      if max_pop_size > Chart1.extent.YMax then
      begin
        Chart1.extent.YMax := max_pop_size;
        application.ProcessMessages;
      end;
      pop_size[current_year] := populationsize;
      sum_pop_size[current_year] := sum_pop_size[current_year] + populationsize;
      if populationsize > 0 then n_sim_no_ext[current_year] := n_sim_no_ext[current_year] + 1;
      {plot trajectory}
      Chart1LineSeries1.addxy(current_year, populationsize);

    if (current_year = 1) or (current_year mod 10 = 0) then
    begin
    WriteMapCSV('output_data/maps/FemalesMap_status_yr_' + IntToStr(current_year) + '.csv', Femalesmap, MapdimX, MapdimY, 0);
    //WriteMapCSV('output_data/maps/FemalesMap_age_yr_' + IntToStr(current_year) + '.csv', Femalesmap, MapdimX, MapdimY, 1);
    WriteMapCSV('output_data/maps/MalesMap_status_yr_' + IntToStr(current_year) + '.csv', Malesmap, MapdimX, MapdimY, 0);
    //WriteMapCSV('output_data/maps/MalesMap_age_yr_' + IntToStr(current_year) + '.csv', Malesmap, MapdimX, MapdimY, 1);
    end;

    WritePopulationToCSV(population, 'output_data/Population_data.csv', current_sim, current_year);

      if (a <= 5) then
      begin
         WritePopulationToCSV(population,'PopulationYear.csv', current_sim, a );
      end;
     CloseFile(ic_file_out);
    end;

   end;
end;

procedure Tspatial_Form.Run_ButtonClick(Sender: TObject);
var
  a, b, c, i: integer;
  t: string;
begin
  randomize; {initialize the pseudorandom number generator}

  paramname := Edit3.Text;
  //ShowMessage('DEBUG: Paramname = ' + paramname);
  ReadParameters(paramname);

  if CheckBox1.Checked then
  begin
  {These values overwrite the values in the file with the input from the GUI}
  val(Edit1.Text, n_ini);
  val(Edit2.Text, max_years);
  val(Edit5.Text, n_sim);
  mapname:= Edit10.Text;
  mapBHname:= Edit4.Text;
end;

  readmap(mapname, mapBHname, mapPops);


  SetLength(MalesMap, Mapdimx + 1, Mapdimy + 1, 2);
  SetLength(FemalesMap, Mapdimx + 1, Mapdimy + 1, 2);

  SetLength(ConnectionMap, Mapdimx + 1, Mapdimy + 1, 2);

  AssignFile(to_file_out, file_name);
  rewrite(to_file_out); {create txt file}
  writeln(to_file_out, 'current_sim,year,pop1, pop2, pop3, pop4, pop5');

  AssignFile(mig_file_out, 'output_data/migration.csv');
  rewrite(mig_file_out); {create txt file}
  writeln(mig_file_out, 'EventID,Simulation,Year,Sex,Age,Natal_pop,Old_pop,New_pop');

  AssignFile(migS_file_out, 'output_data/migration_settled.csv');
  rewrite(migS_file_out); {create txt file}
  writeln(migS_file_out, 'EventID,Simulation,Year,Sex,Age,Natal_pop,Old_pop,New_pop');

  if not DirectoryExists('output_data') then
    MkDir('output_data');

  AssignFile(to_file_out, 'output_data/popsize.txt');
  Rewrite(to_file_out); {create txt file}

  for a := 1 to max_years do sum_pop_size[a] := 0;
  for a := 1 to max_years do n_sim_no_ext[a] := 0;

  SetLength(each_pop_sizes, 6);
  for i := 0 to High(each_pop_sizes) do
    SetLength(each_pop_sizes[i], max_years+1);

  // Calculate array of step probabilities (here once) to be used in dispersal procedure later
  Step_probabilities;

  MigrationList := TList.Create;
  SettledList := Tlist.Create;

  for current_sim := 1 to n_sim do
  begin

    max_pop_size := 0;
    Startpopulation; {call the procedure to initialize your population}
    Pop_dynamics_GUI;    {call the procedure to run the population dynamics}

    {plot population trayectories}
    Chart1LineSeries1.Clear;
    Chart1LineSeries2.Clear;

    for b := 1 to max_years do Chart1LineSeries1.addxy(b, pop_size[b]);
    application.ProcessMessages;
    if (current_sim > 1) then
      if (n_sim > 1) then
        //   for b:=1 to max_years do Chart1LineSeries2.addxy(b,sum_pop_size[b]/n_sim_no_ext[b]);  //now we calculate the avg only when the population is not extinct
        for b := 1 to max_years do
          Chart1LineSeries2.addxy(b, sum_pop_size[b] / current_sim);

    {save the results to a text file}
    append(to_file_out);
    for b := 1 to max_years do
    begin
      writeln(to_file_out, current_sim, ',', b, ',',
      each_pop_sizes[0,b], ',',
      each_pop_sizes[1,b], ',',
      each_pop_sizes[2,b], ',',
      each_pop_sizes[3,b], ',',
      each_pop_sizes[4,b], ',',
      each_pop_sizes[5,b], ',');
    end;

    CloseFile(to_file_out);

    {Write Migration list to file}
    append(mig_file_out);
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


    {Write Settled Migrants list to file}
    append(migS_file_out);
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

    {Write connection map}
    WriteMapCSV('output_data/maps/FemalesMap_traveled_' + IntToStr(current_sim) + '.csv', ConnectionMap, MapdimX, MapdimY, 0);
    WriteMapCSV('output_data/maps/MalesMap_traveled_' + IntToStr(current_sim) + '.csv', ConnectionMap, MapdimX, MapdimY, 1);


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

