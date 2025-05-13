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

unit LX4D.NetInfo;

interface

uses
  System.Types, System.Classes, System.SysUtils,
  LX4D.SystemInfo, LX4D.CmdLine;

type
  TLX4DNetInfo = class
  public type
    TOnLog = reference to procedure(const Log: string);
  private
    class function GetIPAddr(const cInet: string; Log: TOnLog = nil): TStringList;
  public
    class function GetIPv4Addr(Log: TOnLog = nil): TStringList;
    class function GetIPv6Addr(Log: TOnLog = nil): TStringList;
  end;

implementation

{ TLX4DNetInfo }

class function TLX4DNetInfo.GetIPAddr(const cInet: string; Log: TOnLog): TStringList;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class function TLX4DNetInfo.GetIPv4Addr(Log: TOnLog): TStringList;
begin
  result := GetIPAddr('inet ', Log);
end;

class function TLX4DNetInfo.GetIPv6Addr(Log: TOnLog): TStringList;
begin
  result := GetIPAddr('inet6 ', Log);
end;

end.
