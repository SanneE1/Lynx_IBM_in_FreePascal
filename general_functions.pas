unit general_functions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  lynx_define_units;

function randomPoisson(mean: integer): integer;
procedure ArrayToNegOne(var arr: array of integer);
procedure ShowErrorAndExit(const errMsg: string);


implementation

function randomPoisson(mean: integer): integer;
  {pseudorandom Poisson distributed number genetrator, Donald Knuth's algorithm}
const
  RESOLUTION = 1000;
var
  k: integer;
  b, l: real;
begin
  //assert(mean > 0, 'mean < 1');
  k := 0;
  b := 1;
  l := exp(-mean);
  while b > l do
  begin
    k := k + 1;
    b := b * random(RESOLUTION) / RESOLUTION;
  end;
  if mean <= 0 then randomPoisson := 0
  else
    randomPoisson := k - 1;
end;

procedure ArrayToNegOne(var arr: array of integer);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    arr[i] := -1;
end;



procedure ShowErrorAndExit(const errMsg: string);
begin
  {$IFDEF LCL}
  // If in GUI mode, use MessageDlg for graphical error display
  MessageDlg('Error: ' + errMsg, mtError, [mbOK], 0);
  {$ELSE}
  // In non-GUI mode, use console output
  Writeln('Error: ', errMsg);
  Readln;
  {$ENDIF}

  Halt(1);  // Exit the program with a non-zero code to indicate error
end;


end.

