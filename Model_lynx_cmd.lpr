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
  begin
  WriteLn('A different number of parameters received than expected. Check submission line');
  Exit();
  end;

  paramname := ParamStr(1);

  if paramname = '' then paramname := 'input_data' + PathDelim + 'parameter_values_Peninsula_gen_100.txt';
  if ParamStr(2) = '' then taskID := 0 else taskID := StrToInt(ParamStr(2));

  //SetHeapTraceOutput('job_report' + PathDelim + IntToStr(taskID) +  '_heaptrc.log');  // Output file name
  //SetHeapTraceOutput(IntToStr(taskID) +  '_heaptrc.log');  // Output file name

  //if taskID := then taskID := 0;
  WriteLn('Starting program');

  try
  RunPopSim;
  WriteLn('All simulations finished');
  except
    on E: Exception do
    begin
      WriteLn('Program error: ', E.Message);
    end;
  end;

  readLn();

end.

