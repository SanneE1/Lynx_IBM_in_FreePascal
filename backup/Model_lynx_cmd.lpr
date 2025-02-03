program Model_lynx_cmd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  lynx_population_dynamics, lynx_define_units;

begin

  if ParamCount <> 6 then
  {ShowErrorAndExit('Incorrect number of parameters given')
  else}
  WriteLn('A different number of parameters received than expected. Check Lines below for used values');

  begin

  n_ini := StrToIntDef(ParamStr(1), 100);
  max_years := StrToIntDef(ParamStr(2), 100);
  n_sim := StrToIntDef(ParamStr(3), 1);
  paramname := ParamStr(4);
  if paramname = '' then paramname := 'input_data/parameter_values.txt';
  mapname := ParamStr(5);
  if mapname = '' then mapname := 'input_data/old_donana.txt';

  WriteLn('Running ', n_sim, ' lynx population simulation(s) with ',n_ini, ' initial population size, for ', max_years, ' years');
  WriteLn('Parameter file: ', paramname);
  WriteLn('Map file: ', mapname);

  RunPopSim;

  end;
  WriteLn('All simulations finished');
  ReadLn;

end.

