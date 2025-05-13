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

unit LX4D.PackageManager;

interface

uses
  System.Types, System.Classes, System.SysUtils,
  LX4D.CmdLine;

type
  ELX4DPackageManager = class(Exception);
  TLX4DPackageManager = class
  public type
    TOnPackageInstalled = reference to procedure(const PackageName: string; Success: boolean; const Error: string);
  public
    class function GetLinuxInstallerCommand: string;
    class function GetInstalledPackages: TStringList;
    class procedure InstallPackage(const PackageName: string; OnPackageInstalled: TOnPackageInstalled;
      OnLog: TLX4DCmdLine.TOnNewLine = nil);
  end;

implementation

uses
  LX4D.SystemInfo;

class function TLX4DPackageManager.GetLinuxInstallerCommand: string;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class function TLX4DPackageManager.GetInstalledPackages: TStringList;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class procedure TLX4DPackageManager.InstallPackage(const PackageName: string; OnPackageInstalled: TOnPackageInstalled;
  OnLog: TLX4DCmdLine.TOnNewLine);
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

end.
