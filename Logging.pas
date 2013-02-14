unit Logging;

interface

//
// Unfortunately, Delphi doesn't have preprocessor macros like C++ and we can't
// just "boil away" our logging with an $IFDEF (or at least I can't figure out how to).
// Instead, if logging isn't defined don't define a logging interface, so any LOG
// statements will give an error.
//
// This forces the programmer to comment out any logging in a release build, but at
// least it doesn't negatively impact the finished executeable.  Even if we used an
// empty LOG procedure, the string and/or formatting arguments would need to be
// bundled and the empty routine called for every log, not to mention bloating the
// EXE size with debug logging code.
//
// If anyone has a better idea, please let me know!  AJG 6/16/2001
//

{$IFDEF LOGGING}
procedure LOG(s:string);                            overload;
procedure LOG(s:string; const Args:array of const); overload;
{$ENDIF}

implementation

uses Windows, SysUtils;

{$IFDEF LOGGING}

var LogFilename:string;
    LoggingOn:boolean;
    LogFile:Text;
    FirstMillisecond:longword;

procedure LOG(s:string);
begin
  if not LoggingOn then exit;

  WriteLn(LogFile, Format('[%12.3f] ', [(GetTickCount - FirstMillisecond)*0.001]), s);
  Flush(LogFile);
end;

procedure LOG(s:string; const Args:array of const);
begin
  Log(Format(s,Args));
end;

{$ELSE}

procedure LOG(s:string); overload;
begin
end;

procedure LOG(s:string; const Args:array of const); overload;
begin
end;
{$ENDIF}

initialization
{$IFDEF LOGGING}
  LogFilename := ParamStr(0);
  LogFilename := copy(LogFilename,1, length(LogFilename)-3) + 'log';

  // Use Logging if there is a log file already.
  // User just needs to touch this file to start
  // logging.
  LoggingOn := FileExists(LogFilename);

  if (LoggingOn) then begin
    AssignFile(LogFile, LogFilename);
    Rewrite(LogFile);
    FirstMillisecond := GetTickCount;
    writeln(LogFile,'[Elapsed Time]  Message');
    writeln(LogFile,'-------------- ---------');
    LOG('(Note: Logging is enabled by presence of log file.');
    LOG(' To disable logging, delete '+LogFilename+')');
    LOG('Logging started');
  end;

finalization
  if (LoggingOn) then begin
     LOG('Logging ended');
     CloseFile(LogFile);
  end;
{$ENDIF}
end.
