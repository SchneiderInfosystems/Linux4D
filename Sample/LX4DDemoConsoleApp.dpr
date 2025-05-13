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


program LX4DDemoConsoleApp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  LX4D.SystemInfo in '..\Source\LX4D.SystemInfo.pas',
  LX4D.CmdLine in '..\source\LX4D.CmdLine.pas',
  LX4D.IOUtils in '..\source\LX4D.IOUtils.pas',
  LX4D.NetInfo in '..\source\LX4D.NetInfo.pas',
  LX4D.PackageManager in '..\source\LX4D.PackageManager.pas';

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  try
    Writeln('LX4DemoConsoleApp Version ======================================== ' + TLX4DSystemInfo.LibVersion + ' ===');
    Writeln('TLX4DSystemInfo ----------------------------------------------------------------');
    if TLX4DSystemInfo.RunWithRootRights then
      Writeln('Application runs with root rights')
    else
      Writeln('Application runs without root rights');
    Writeln('Current User ID ..... : ' + TLX4DSystemInfo.UserID.ToString);
    Writeln('Language ............ : ' + TLX4DSystemInfo.LanguagePretty);
    Writeln('Encoding ............ : ' + TLX4DSystemInfo.EncodingStr);
    Writeln('Current User Name ... : ' + TLX4DSystemInfo.UserName);
    Writeln('Kernel .............. : ' + TLX4DSystemInfo.Kernel.PrettyName);
    Writeln('CPU ................. : ' + TLX4DSystemInfo.CPU.PrettyName);
    {$IFDEF DEBUG}
    for var line in TLX4DSystemInfo.CPU.Details do
      Writeln('  ' + line);
    {$ENDIF}
    Writeln('Distribution ........ : ' + TLX4DSystemInfo.Distribution.PrettyName);
    Writeln('Distribution.ID ..... : ' + TLX4DSystemInfo.Distribution.ID);
    Writeln('Distribution.BaseID . : ' + TLX4DSystemInfo.Distribution.BaseID);
    Writeln('Distribution.CodeName : ' + TLX4DSystemInfo.Distribution.CodeName);
    Writeln('Distribution.Release  : ' + TLX4DSystemInfo.Distribution.Release.ToString);
    Writeln('Distribution.SrcFile  : ' + TLX4DSystemInfo.Distribution.SourceFile);
    {$IFDEF DEBUG}
    for var line in TLX4DSystemInfo.Distribution.Details do
      Writeln('  ' + line);
    {$ENDIF}
    {$IFDEF DEBUG}
    Writeln('Environment Variables : ');
    for var line in TLX4DSystemInfo.EnvironmentVariables do
      Writeln('  ' + line);
    {$ENDIF}
    Writeln('TLX4DPath ----------------------------------------------------------------------');
    Writeln('Desktop path ........ : ' + TLX4DPath.GetDesktopPath);
    Writeln('Downloads path ...... : ' + TLX4DPath.GetDownloadsPath);
    Writeln('Public path ......... : ' + TLX4DPath.GetPublicPath);
    Writeln('Documents path ...... : ' + TLX4DPath.GetDocumentsPath);
    Writeln('Music path .......... : ' + TLX4DPath.GetMusicPath);
    Writeln('Pictures path ....... : ' + TLX4DPath.GetPicturesPath);
    Writeln('Movies path ......... : ' + TLX4DPath.GetMoviesPath);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
