unit StreamUtil;

{$MODE Delphi}

interface

uses SysUtils, LCLIntf, LCLType, LMessages, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
     Menus;

function ReadStringFromStream(stream:TStream):string;
procedure WriteStringToStream(stream:TStream; s:string);

implementation

{ ----------------------------------------------------------------- }

function ReadStringFromStream(stream:TStream):string;
var len:integer;
    p:PChar;
begin
  stream.ReadBuffer(len,sizeof(integer));
  SetLength(Result,len);
  p:=PChar(Result);
  stream.ReadBuffer(p^,len);
end;

procedure WriteStringToStream(stream:TStream; s:string);
var len:integer;
    p:PChar;
begin
  len:=length(s);
  stream.WriteBuffer(len,sizeof(integer));
  p:=PChar(s);
  stream.WriteBuffer(p^,len);
end;



end.
