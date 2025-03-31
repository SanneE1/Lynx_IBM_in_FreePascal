program Model_lynx_cmd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  lynx_population_dynamics, lynx_define_units;

begin

  if ParamCount <> 2 then
  WriteLn('A different number of parameters received than expected. Check submission line');

  paramname := ParamStr(1);
  if paramname = '' then paramname := 'input_data' + PathDelim + 'parameter_values_Peninsula_gen_000.txt';

  if ParamStr(2) = '' then taskID := 0 else taskID := StrToInt(ParamStr(2));


  //if taskID := then taskID := 0;

  RunPopSim;

  WriteLn('All simulations finished');
  ReadLn;

end.

