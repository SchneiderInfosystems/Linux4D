{******************************************************************************}
{                                                                              }
{  Delphi LX4D Library                                                         }
{  Copyright (c) 2025 Christoph Schneider                                      }
{  Schneider Infosystems AG, Switzerland                                       }
{  https://github.com/SchneiderInfosystems/LinuxFB4D                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit LX4D.CmdLine;

interface

uses
  System.Classes, System.SysUtils,
{$IF Defined(VCL)}
  VCL.Forms,
{$ELSEIF Defined(FMX)}
  FMX.Forms,
{$ENDIF}
  Posix.Base,
  Posix.Fcntl;

type
  ELX4DCmdLine = class(Exception);
  TLX4DCmdLine = record
  public type
    TCmdOption = (RunWithRootPrivileges, CatchStdError, TrimLine);
    TCmdOptions = set of TCmdOption;
    TOnNewLine = reference to procedure(InNotOut: boolean; const Line: string);
    TOnCommandTerminated = reference to procedure(Success: boolean; const Error: string);
  public
    class function AppIsTerminated: boolean; static;

    class function ExecuteCommandLineSynch(const Command: string;
      Options: TCmdOptions = []): TStringList; overload; static;
    class function ExecuteCommandLineSynch(const Command: string; OnNewLine: TOnNewLine;
      Options: TCmdOptions = []): boolean; overload; static;

    class procedure ExecuteCommandLineAsynch(const Command: string; OnNewLine: TOnNewLine;
      OnCommandTerminated: TOnCommandTerminated; Options: TCmdOptions = []); static;

    class function ExecuteBashFileSynch(const BashFile: string;
      Options: TCmdOptions = []): TStringList; overload; static;
    class function ExecuteBashFileSynch(const BashFile: string; OnNewLine: TOnNewLine;
      Options: TCmdOptions = []): boolean; overload; static;

    class procedure ExecuteBashFileAsynch(const BashFile: string; OnNewLine: TOnNewLine;
      OnCommandTerminated: TOnCommandTerminated; Options: TCmdOptions = []); static;
  end;

resourcestring
  rsFunctionAvailableInExtendedLinux4DOnly = 'This functionality is only available in the ExtendedLinux4D edition';

implementation

uses
  LX4D.SystemInfo;

{ TLX4DCmdLine }

class function TLX4DCmdLine.AppIsTerminated: boolean;
begin
{$IF Defined(VCL) OR Defined(FMX)}
  result := Application.Terminated;
{$ELSE}
  result := false;
{$ENDIF}
end;

class function TLX4DCmdLine.ExecuteCommandLineSynch(const Command: string; Options: TCmdOptions): TStringList;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class function TLX4DCmdLine.ExecuteCommandLineSynch(const Command: string; OnNewLine: TOnNewLine;
  Options: TCmdOptions): boolean;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class procedure TLX4DCmdLine.ExecuteCommandLineAsynch(const Command: string; OnNewLine: TOnNewLine;
  OnCommandTerminated: TOnCommandTerminated; Options: TCmdOptions);
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class function TLX4DCmdLine.ExecuteBashFileSynch(const BashFile: string; Options: TCmdOptions): TStringList;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class function TLX4DCmdLine.ExecuteBashFileSynch(const BashFile: string; OnNewLine: TOnNewLine;
  Options: TCmdOptions): boolean;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class procedure TLX4DCmdLine.ExecuteBashFileAsynch(const BashFile: string; OnNewLine: TOnNewLine;
  OnCommandTerminated: TOnCommandTerminated; Options: TCmdOptions);
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

end.
