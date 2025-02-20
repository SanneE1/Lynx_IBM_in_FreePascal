unit lynx_input_output_functions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  lynx_define_units, general_functions;

procedure ReadMap(mapname: string);
procedure ReadParameters(paramname: string);
procedure UpdateAbundanceMap;
procedure WriteMapCSV(filename: string; var arrayData: Array3Dbyte; dimx, dimy, dimz: integer);
procedure WritePopulationToCSV(population: TList; filename: string; current_sim, year: integer);
Procedure WriteFamtreeToCSV(filename: string);
Procedure DebugLog (msg: string);

implementation

procedure DebugLog(msg: string);
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, 'debug_log.txt');
  if FileExists('debug_log.txt') then
    Append(LogFile)
  else
    Rewrite(LogFile);

  WriteLn(LogFile, msg);
  CloseFile(LogFile);
end;


procedure ReadMap(mapname: string);
var
  ix, iy, Value: integer;

begin

  Assign(filename, mapName);
  reset(filename);
  readln(filename, Mapdimx, Mapdimy);
  SetLength(HabitatMap, Mapdimx + 1, Mapdimy + 1);


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
  par_seq: array[1..32] of string;
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
   par_seq[29]:= 'n_ini';
   par_seq[30]:= 'max_years';
   par_seq[31]:= 'n_sim';
   par_seq[32]:= 'mapname';


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

        if (param = 'mapname') then
          mapname := Trim(Copy(a, spacePos + 1, Length(a)))
          else
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
   n_ini              := Round(val_seq[29]);
   max_years          := Round(val_seq[30]);
   n_sim              := Round(val_seq[31]);


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

procedure WritePopulationToCSV(population: TList; filename: string; current_sim, year: integer);   //capire come funzionano le simulazioni, capire come scrivere il file csv
var
  csvFile: TextFile;
  i, j, l, UniqueID: integer;
  allele1,allele2,homozygosity: integer;
begin

  AssignFile(csvFile, filename);

  if (current_sim = 1) and (year = 1) then
    begin
    Rewrite(csvFile);
    // Write header
    WriteLn(csvFile, 'Simulation,Year,UniqueID,Sex,Age,Status,Coor_X,Coor_Y,Territory_XY,Genome, Homozygosity');
    end
  else append(csvFile);

  // Write data for each individual
  for l := 0 to population.Count - 1 do
  begin
    //reset homozigosity for each ind
    //homozygosity :=0;
    Write(csvFile, current_sim, ',', year, ',');
    individual := PAgent(population[l]);

    // Write individual information
    Write(csvFile, individual^.UniqueID, ',');
    Write(csvFile, individual^.sex, ',');
    Write(csvFile, individual^.Age, ',');
    Write(csvFile, individual^.Status, ',');
    Write(csvFile, individual^.Coor_X, ',');
    Write(csvFile, individual^.Coor_Y, ',');

    // Write territory coordinates
    for j := 0 to length(individual^.TerritoryX) - 1 do
    begin

      Write(csvFile, Individual^.TerritoryX[j], '/');   // X and Y on the same line??
      Write(csvFile, individual^.TerritoryY[j]);

      // Add comma if not last coordinate
      if j < length(individual^.TerritoryX) - 1 then
        Write(csvFile, ';')        //devo aggiungere che dopo le coordinate deve aggiungere una , o magari e sottointeso??
      else
      Write(csvFile, ',');          // or I have to write end and the the ',' thing?
    end;


    // Write genetics
    for i := 1 to 24 do
      begin
      Write(csvFile, Individual^.Genome[i,0], ':', Individual^.Genome[i,1]);    //instead of:  Write(csvFile, Individual^.Genome[i,0], ':', Individual^.Genome[i,1], ',')non dovrebbe essere ;? mica la , fa passare al prossimo elemento
      if i < 24 then
        Write(csvFile, ';')
      else
        Write(csvFile, ',');
      end;


    //alternative with less coding i think MAYBE INCORRECT
    // Write territory coordinates
    //for j := 0 to length(individual^.TerritoryX) - 1 do
    //begin

     // Write(csvFile, Individual^.TerritoryX[j], '/', individual^.TerritoryY[j], ';' );   // X and Y on the same line??
     // end;
     // Write(csvFile, ',');

    // Write genetics
    //for i := 1 to 24 do
    //  begin
    //    Write(csvFile, Individual^.Genome[i,0], ':', Individual^.Genome[i,1], ';');    //instead of:  Write(csvFile, Individual^.Genome[i,0], ':', Individual^.Genome[i,1], ',')non dovrebbe essere ;? mica la , fa passare al prossimo elemento
    //  end;
    //  Write(csvFile, ',');

    //percentage homogeneity x ind
    for i:= 1 to 24 do
      begin
        allele1 := Individual^.Genome[i,0];
        allele2 := Individual^.Genome[i,1];
        if allele1 = allele2 then
          homozygosity := homozygosity + 1;
      end;

    WriteLn(csvFile, ',', Individual^.P_homogeneity:0:4); // End of current individual's data   // devo mettere WriteLn(csvFile, ',', (homozygosity / 24) * 100:0:2);??
  end;

  CloseFile(csvFile);
end;

Procedure WriteFamtreeToCSV(filename: string);
var
  csvFile: TextFile;
  i: integer;
  IC: real; //temporal variable for IC
begin
  // assign and open file CSV
  AssignFile(csvFile, filename);
  Rewrite(csvFile);

  //write name of columns
  WriteLn(csvFile, 'UniqueID,IC,FatherID,MotherID');

  // write into CSV
  for i := 0 to Length(Famtree) - 1 do
  begin
    IC:= Famtree[i,1]; //copy the IC coefficient as a temporal variable
    WriteLn(csvFile,
            Famtree[i, 0], ',',   // UniqueID data in ind i for the first column 0
            IC:0:2, ',',          // IC (Coefficient of Inbreeding), written with  2 decimals
            Famtree[i, 2], ',',   // FatherID
            Famtree[i, 3]);      // MotherID
  end;

  CloseFile(csvFile);
end;

end.

