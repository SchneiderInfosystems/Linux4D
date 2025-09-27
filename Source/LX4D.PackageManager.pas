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
  /// <summary>
  /// Exception raised for errors occurring during package management operations
  /// within <see cref="TLX4DPackageManager"/>.
  /// </summary>
  ELX4DPackageManager = class(Exception);

  /// <summary>
  /// Provides static helper methods to interact with the Linux system's package manager.
  /// </summary>
  /// <remarks>
  /// This record facilitates tasks such as identifying the system's package installer,
  /// listing installed packages, and initiating package installations.
  /// It aims to abstract common operations across different Linux distributions
  TLX4DPackageManager = record
  public type
    /// <summary>
    /// Callback procedure invoked when a package installation attempt has completed.
    /// </summary>
    /// <param name="PackageName">The name of the package that was attempted to be installed.</param>
    /// <param name="Success">True if the package installation was successful, False otherwise.</param>
    /// <param name="Error">
    ///   Contains an error message if <c>Success</c> is False.
    ///   This could include messages from the package manager itself or errors encountered
    ///   during the process. Empty if successful.
    /// </param>
    TOnPackageInstalled = reference to procedure(const PackageName: string; Success: boolean; const Error: string);
  public
    /// <summary>
    /// Detects and returns the primary command used for installing packages on the current Linux system.
    /// </summary>
    /// <returns>
    /// A string representing the installer command (e.g., "apt-get", "yum" ...).
    /// Returns an empty string if the package installer cannot be determined or is not supported.
    /// </returns>
    /// <remarks>
    /// The implementation typically checks for the presence of known package manager executables.
    /// The returned command is just the base command; arguments like "install" would be appended by other methods.
    /// </remarks>
    class function GetLinuxInstallerCommand: string; static;

    /// <summary>
    /// Retrieves a list of all packages currently installed on the Linux system synchronously.
    /// </summary>
    /// <returns>
    /// A <c>TStringList</c> containing the names of all installed packages.
    /// Returns an empty or nil <c>TStringList</c> if the list cannot be retrieved or if an error occurs.
    /// The caller is responsible for freeing the returned <c>TStringList</c>.
    /// </returns>
    /// <remarks>
    /// This method  queries the system's package database.
    /// The format of package names will correspond to what the native package manager uses.
    /// </remarks>
    /// <exception cref="ELX4DPackageManager">
    /// May be raised if the system's package manager command cannot be determined or
    /// if there's a fundamental issue querying the package database.
    /// </exception>
    class function GetInstalledPackages: TStringList; static;

    /// <summary>
    /// Initiates the installation of a specified package asynchronously.
    /// </summary>
    /// <param name="PackageName">The name of the package to install.</param>
    /// <param name="OnPackageInstalled">
    ///   The <see cref="TOnPackageInstalled"/> callback procedure that will be invoked
    ///   once the installation attempt is complete (either successfully or with an error).
    /// </param>
    /// <param name="OnLog">
    ///   An optional <see cref="TLX4DCmdLine.TOnNewLine"/> callback to receive real-time
    ///   output (stdout/stderr) from the package manager command during the installation process.
    ///   Defaults to nil if not provided.
    /// </param>
    /// <remarks>
    /// This procedure uses the command obtained from <see cref="GetLinuxInstallerCommand"/>
    /// to execute the installation (e.g., "sudo apt-get install [PackageName]").
    /// The installation process is performed asynchronously; this method will return before
    /// the installation is complete. The result is reported via the <c>OnPackageInstalled</c> callback.
    /// Root privileges are generally required for package installation, and the underlying
    /// command execution handle this. That is why the user needs to enter their password.
    /// </remarks>
    /// <exception cref="ELX4DPackageManager">
    /// May be raised if the package installer command cannot be determined,
    /// or if there's an issue initiating the installation process (e.g., failure to start the command).
    /// Errors during the actual installation by the package manager are reported via the
    /// <c>Error</c> parameter of the <c>OnPackageInstalled</c> callback.
    /// </exception>
    class procedure InstallPackage(const PackageName: string; OnPackageInstalled: TOnPackageInstalled;
      OnLog: TLX4DCmdLine.TOnNewLine = nil); static;
	  
    /// <summary>
    /// Initiates the full update of the installer 
    /// </summary>
    /// <param name="OnPackageInstallerUpdated">
    ///   The <see cref="TOnPackageInstalled"/> callback procedure that will be invoked
    ///   once the installation attempt is complete (either successfully or with an error).
    /// </param>
    /// <param name="SingleStep">
    ///   Do the upgrade in one single step (recommended) or start it step by step. When using the single step
	///   option, the user need to enter the password just once.
    /// </param>
    /// <param name="OnLog">
    ///   An optional <see cref="TLX4DCmdLine.TOnNewLine"/> callback to receive real-time
    ///   output (stdout/stderr) from the package manager command during the installation process.
    ///   Defaults to nil if not provided.
    /// </param>
    /// <remarks>
    /// This procedure uses the command obtained from <see cref="GetLinuxInstallerCommand"/>
    /// to execute the installer update (e.g. firstly "sudo apt-get update", secondly "sudo apt-get upgrade" 
	/// and finally "sudo apt-get dist-upgrade").
    /// The installation process is performed asynchronously; this method will return before
    /// the installation is complete. The result is reported via the <c>OnPackageInstallerUpdated</c> callback.
    /// Root privileges are generally required for package installer update, and the underlying
    /// command execution handle this. That is why the user needs to enter their password.
    /// </remarks>
    /// <exception cref="ELX4DPackageManager">
    /// May be raised if the package installer command cannot be determined,
    /// or if there's an issue initiating the installation process (e.g., failure to start the command).
    /// Errors during the actual installer update are reported via the
    /// <c>Error</c> parameter of the <c>OnPackageInstallerUpdated</c> callback.
    /// </exception>
    class procedure UpdatePackageInstaller(OnPackageInstallerUpdated: TOnPackageInstalled;
      SingleStep: boolean = true; OnLog: TLX4DCmdLine.TOnNewLine = nil); static;
  end;

implementation

uses
  LX4D.SystemInfo;

class function TLX4DPackageManager.GetLinuxInstallerCommand: string;
begin
  result := '';
  case TLX4DSystemInfo.Distribution.Kind of
    Ubuntu,
    Mint,
    Debian:
      result := 'apt-get';
    RedHat,
    Fedora:
      result := 'yum';
  end;
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

class procedure TLX4DPackageManager.UpdatePackageInstaller(
  OnPackageInstallerUpdated: TOnPackageInstalled; SingleStep: boolean;
  OnLog: TLX4DCmdLine.TOnNewLine);
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

end.
