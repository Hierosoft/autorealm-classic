unit XMLUnit;

{$MODE Delphi}

interface

type XML = class
     private
       data:string;
       indent:integer;

       lookingfor:string;
       lasttag:string;
       lastvalue:string;
       function IndentString:string;
       procedure SetData(s:string);
       procedure ReadNextItem;

     public
       // Writing to XML
       procedure OpenTag(name:string);
       procedure CloseTag(name:string);
       procedure Comment(name:string);
       procedure Field(name:string; value:string);

       // Reading from XML
       function CurrentTag:string;
       function GetTag:string;
       function MatchTag(s:string):boolean;
       function MatchOpenTag(s:string):boolean;
       function MatchCloseTag(s:string):boolean;
       function GetValue:string;
       procedure BadTag;

       // General: creation, destruction, and text access
       constructor Create;
       destructor Destroy;  override;
       property AsText:string read data write SetData;
     end;

implementation

uses SysUtils, LocalizedStrings;

constructor XML.Create;
begin
  data:='';
  indent:=0;
end;

destructor XML.Destroy;
begin
end;

function XML.IndentString:string;
var i:integer;
begin
  Result:='';
  for i:=1 to indent do Result := Result + '  ';
end;

procedure XML.ReadNextItem;
label AnotherToken;
var p:integer;

begin
AnotherToken:

  lasttag := '';
  lastvalue := '';

  data := TrimLeft(data);

  // Stop when we've exhausted the buffer.
  if (data='') then exit;

  // Is this a tag, or a value?
  if (length(data) > 1) and (data[1]='<') then begin
    // Find the ending angle bracket
    p:=pos('>',data);
    // Move from data to tag buffer
    lasttag := copy(data,2,p-2);
    delete(data,1,p);

    // If this is a comment block, skip it and read the next tag
    if (copy(lasttag,1,3)='!--') then goto AnotherToken;
    end
  else begin
    // Find the beginning of the next tag
    p:=pos('<', data);

    // Move from data to value buffer
    if (p=0) then begin
      lastvalue := data;
      data := '';
      end
    else begin
      lastvalue := copy(data, 1, p-1);
      delete(data,1,p-1);
      end;
  end;
end;

procedure XML.SetData(s:string);
begin
  data := s;
  ReadNextItem;
end;

procedure XML.OpenTag(name:string);
begin
  data := data + IndentString + '<' + name + '>'#13#10;
  inc(indent);
end;

procedure XML.CloseTag(name:string);
begin
  dec(indent);
  data := data + IndentString + '<\' + name + '>'#13#10;
end;

procedure XML.Comment(name:string);
begin
  data := data + IndentString + '<!-- ' + name + ' -->'#13#10;
end;

procedure XML.Field(name:string; value:string);
begin
  data := data + IndentString + '<' + name + '>' + value + '<\' + name + '>'#13#10;
end;

function XML.CurrentTag:string;
begin
  Result := lasttag;
end;

function XML.GetTag:string;
begin
  Result := lasttag;
  ReadNextItem;
end;

function XML.GetValue:string;
begin
  Result := lastvalue;
  // Only advance to the next token if we weren't reading a tag,
  // but a value.  We don't test on the value, because you're not allowed a null value.
  if (lasttag='') then ReadNextItem;
end;

function XML.MatchTag(s:string):boolean;
begin
  lookingfor := s;

  if (s=lasttag) then begin
    MatchTag := true;
    ReadNextItem;
    end
  else
    MatchTag := false;
end;

function XML.MatchOpenTag(s:string):boolean;
begin
  Result := MatchTag(s);
end;

function XML.MatchCloseTag(s:string):boolean;
begin
  Result := MatchTag('\'+s);
end;

procedure XML.BadTag;
begin
  raise EInOutError.CreateFmt(res_XMLMismatchedTag, [lasttag, lookingfor]);
end;


end.
