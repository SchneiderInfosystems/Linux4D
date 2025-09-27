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

program LX4DDemoAppWithRootRights;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  LX4D.SystemInfo in '..\Source\LX4D.SystemInfo.pas',
  LX4D.CmdLine in '..\source\LX4D.CmdLine.pas';

begin
  try
    if not TLX4DSystemInfo.RunWithRootRights then
    begin
      writeln('Started without root privileges -> restart');
      TLX4DCmdLine.StartCommand(ParamStr(0), true)
    end else
      writeln('Started with root privileges');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
