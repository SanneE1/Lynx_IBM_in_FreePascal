{}
unit lynx_population_dynamics_GUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Math, LCLType,
  lynx_define_units, general_functions, lynx_input_output_functions,
  lynx_vital_rates;

type

{ Tspatial_Form }

Tspatial_Form = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
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

      Individual^.Coor_X := 550;   // For old_donana 130;  // For Peninsula = 550
      Individual^.Coor_Y := 1550;  // For old_donana 100;  // For Peninsula = 1550

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

      Population.add(Individual);

    end;
  end;
end;


procedure Tspatial_Form.Pop_dynamics_GUI;
var
  b, xy, day, Tcheck: integer;
begin
  with population do
  begin
    for current_year := 1 to max_years do
    begin
      day := 0;  // Start the year
      while (day < 366) and (populationsize > 0) do //Let's pretend there's no such thing as leap years
      begin
        day := day + 1;
        populationsize := population.Count;

        //UpdateAbundanceMap;

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

          if Tcheck = Tsize then Individual^.Status := 3;
          end;

          each_pop_sizes[Individual^.current_pop, current_year] := each_pop_sizes[Individual^.current_pop, current_year] + 1;

        end;
      end;

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

    {if (current_year mod 5 = 0) then
    begin
    WriteMapCSV('output_data/FemalesMap_status_yr_' + IntToStr(current_year) + '.csv', Femalesmap, MapdimX, MapdimY, 0);
    WriteMapCSV('output_data/FemalesMap_age_yr_' + IntToStr(current_year) + '.csv', Femalesmap, MapdimX, MapdimY, 1);
    WriteMapCSV('output_data/MalesMap_status_yr_' + IntToStr(current_year) + '.csv', Malesmap, MapdimX, MapdimY, 0);
    WriteMapCSV('output_data/MalesMap_age_yr_' + IntToStr(current_year) + '.csv', Malesmap, MapdimX, MapdimY, 1);
    end;
    }
    end;

    if populationsize = 0 then N_extint := N_extint + 1;

   end;
end;

procedure Tspatial_Form.Run_ButtonClick(Sender: TObject);
var
  a, b, i: integer;
  t: string;
begin
  randomize; {initialize the pseudorandom number generator}

  paramname := Edit3.Text;
  ReadParameters(paramname);

  {These values should overwrite the values in the file with the input from the GUI}
  val(Edit1.Text, n_ini);  {read parameter values from the form}
  val(Edit2.Text, max_years);

  mapname := Edit10.Text;
  mapBHname := Edit4.Text;

  readmap(mapname, mapBHname, mapiPname, mapPops);

  SetLength(MalesMap, Mapdimx + 1, Mapdimy + 1, 2);
  SetLength(FemalesMap, Mapdimx + 1, Mapdimy + 1, 2);

  N_extint := 0;

  AssignFile(to_file_out, file_name);
  rewrite(to_file_out); {create txt file}
  writeln(to_file_out, 'current_sim,year,pop1, pop2, pop3, pop4, pop5');

  AssignFile(mig_file_out, 'output_data/migration.csv');
  rewrite(mig_file_out); {create txt file}
  writeln(mig_file_out, 'EventID,Simulation,Year,Sex,Age,Natal_pop,Old_pop,New_pop');


  for a := 1 to max_years do sum_pop_size[a] := 0;
  for a := 1 to max_years do n_sim_no_ext[a] := 0;

  SetLength(each_pop_sizes, 6);
  for i := 0 to High(each_pop_sizes) do
    SetLength(each_pop_sizes[i], max_years+1);

  // Calculate array of step probabilities (here once) to be used in dispersal procedure later
  Step_probabilities;

  MigrationList := TList.Create;

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
      if (n_sim > 1) and (n_extint <> n_sim) then
        //   for b:=1 to max_years do Chart1LineSeries2.addxy(b,sum_pop_size[b]/n_sim_no_ext[b]);  //now we calculate the avg only when the population is not extinct
        for b := 1 to max_years do
          Chart1LineSeries2.addxy(b, sum_pop_size[b] / current_sim);

    {save the results to a text file}
    append(to_file_out);
    for b := 1 to max_years do
      writeln(to_file_out, current_sim, ',', b, ',',
      each_pop_sizes[0,b], ',',
      each_pop_sizes[1,b], ',',
      each_pop_sizes[2,b], ',',
      each_pop_sizes[3,b], ',',
      each_pop_sizes[4,b], ',',
      each_pop_sizes[5,b], ',');

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

