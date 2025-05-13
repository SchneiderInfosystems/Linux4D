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
  /// <summary>
  /// Provides static helper methods to retrieve network interface information,
  /// primarily focused on IP addresses.
  /// </summary>
  /// <remarks>
  /// This record can be used to list available IPv4 and IPv6 addresses on the host system.
  /// It includes an optional logging callback for diagnostic purposes during information retrieval.
  /// </remarks>
  TLX4DNetInfo = record
  public type
    /// <summary>
    /// Callback procedure for logging diagnostic messages during network information retrieval.
    /// </summary>
    /// <param name="Log">The log message string.</param>
    TOnLog = reference to procedure(const Log: string);
  private
    class function GetIPAddr(const cInet: string; Log: TOnLog = nil): TStringList; static;
  public
    /// <summary>
    /// Retrieves a list of active IPv4 addresses on the system.
    /// </summary>
    /// <param name="Log">
    ///   An optional <see cref="TOnLog"/> callback procedure to receive diagnostic messages
    ///   during the address retrieval process. Defaults to nil if not provided.
    /// </param>
    /// <returns>
    /// A <c>TStringList</c> containing the IPv4 addresses found on active network interfaces.
    /// Each string in the list represents one IPv4 address (e.g., "192.168.1.10").
    /// Returns an empty <c>TStringList</c> if no IPv4 addresses are found or if an error occurs.
    /// The caller is responsible for freeing the returned <c>TStringList</c>.
    /// </returns>
    /// <remarks>
    /// This method iterates through network interfaces and extracts their IPv4 configurations.
    /// Loopback addresses (e.g., "127.0.0.1") are not be included in the list.
    /// </remarks>
    class function GetIPv4Addr(Log: TOnLog = nil): TStringList; static;

    /// <summary>
    /// Retrieves a list of active IPv6 addresses on the system.
    /// </summary>
    /// <param name="Log">
    ///   An optional <see cref="TOnLog"/> callback procedure to receive diagnostic messages
    ///   during the address retrieval process. Defaults to nil if not provided.
    /// </param>
    /// <returns>
    /// A <c>TStringList</c> containing the IPv6 addresses found on active network interfaces.
    /// Each string in the list represents one IPv6 address (e.g., "fe80::a00:27ff:fe34:5678%eth0").
    /// Returns an empty <c>TStringList</c> if no IPv6 addresses are found or if an error occurs.
    /// The caller is responsible for freeing the returned <c>TStringList</c>.
    /// </returns>
    /// <remarks>
    /// This method iterates through network interfaces and extracts their IPv6 configurations.
    /// Loopback addresses are not be included in the list.
    /// </remarks>
    class function GetIPv6Addr(Log: TOnLog = nil): TStringList; static;
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
