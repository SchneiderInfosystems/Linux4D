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
  Posix.Fcntl,
  Posix.Stdlib;

type
  /// <summary>
  /// Exception raised for errors occurring during command line execution within TLX4DCmdLine.
  /// </summary>
  ELX4DCmdLine = class(Exception);

  /// <summary>
  /// Provides utility methods for executing external command lines and bash scripts,
  /// both synchronously and asynchronously.
  /// </summary>
  /// <remarks>
  /// All methods in this record are static and can be called directly using the record type,
  /// e.g., <c>TLX4DCmdLine.ExecuteCommandLineSynch(...)</c>.
  /// </remarks>
  TLX4DCmdLine = record
  public type
    /// <summary>
    /// Defines options for command execution.
    /// </summary>
    TCmdOption = (
      /// <summary>Executes the command with root/administrator privileges (like using 'sudo').</summary>
      RunWithRootPrivileges,
      /// <summary>Redirects the standard error stream to the standard output stream, so both are captured together.</summary>
      CatchStdError,
      /// <summary>Trims leading and trailing whitespace from each line received from the command's output.</summary>
      TrimLine);

    /// <summary>
    /// A set of <see cref="TCmdOption"/> flags to control command execution behavior.
    /// </summary>
    TCmdOptions = set of TCmdOption;

    /// <summary>
    /// Callback procedure invoked when a new line of output (or error, if redirected)
    /// is received from the command.
    /// </summary>
    /// <param name="InNotOut">
    ///   True if the line is from standard output. If <c>CatchStdError</c> is in <c>Options</c>,
    ///   stderr is redirected to stdout, so this will generally be True for all output lines.
    ///   If <c>CatchStdError</c> is not used, this can be False for lines from standard error,
    ///   depending on the underlying process capturing mechanism.
    /// </param>
    /// <param name="Line">The content of the new line received.</param>
    TOnNewLine = reference to procedure(InNotOut: boolean; const Line: string);

    /// <summary>
    /// Callback procedure invoked when an asynchronously executed command has terminated.
    /// </summary>
    /// <param name="Success">True if the command exited with a zero status code, False otherwise.</param>
    /// <param name="Error">
    ///   Contains an error message if <c>Success</c> is False or if an error occurred during
    ///   process management (e.g., command not found). Empty if successful.
    /// </param>
    TOnCommandTerminated = reference to procedure(Success: boolean; const Error: string);
  public
    /// <summary>
    /// Checks if the application is in the process of terminating.
    /// </summary>
    /// <returns>
    /// True if the application termination has been signaled (e.g., via <c>Application.Terminate</c>
    /// or the main form closing), False otherwise. Useful for asynchronous operations to
    /// determine if they should gracefully stop processing.
    /// </returns>
    class function AppIsTerminated: boolean; static;

    /// <summary>
    /// Starts a command line without waiting until the application ended.
    /// </summary>
    /// <param name="Command">The command line string to execute (e.g., "reboot").</param>
    /// <param name="RunWithRootPrivileges">Flag if root rights are required.</param>
    /// <returns>
    /// Returns true if the command has successful started.
    /// </returns>
   class function StartCommand(const Command: string; RunWithRootPrivileges: boolean = false): boolean; static;

    /// <summary>
    /// Executes a command line synchronously and returns all its output lines as a <c>TStringList</c>.
    /// </summary>
    /// <param name="Command">The command line string to execute (e.g., "ls -l /tmp").</param>
    /// <param name="Options">A set of <see cref="TCmdOptions"/> to control execution behavior. Defaults to an empty set.</param>
    /// <returns>
    /// A <c>TStringList</c> containing all lines from the command's standard output
    /// (and standard error if <c>CatchStdError</c> is specified in <c>Options</c>).
    /// The caller is responsible for freeing this <c>TStringList</c> instance.
    /// </returns>
    /// <exception cref="ELX4DCmdLine">
    /// Raised if the command can not be executed.
    /// </exception>
    class function ExecuteCommandLineSynch(const Command: string;
      Options: TCmdOptions = []): TStringList; overload; static;

    /// <summary>
    /// Executes a command line synchronously, invoking a callback for each new line of output.
    /// </summary>
    /// <param name="Command">The command line string to execute.</param>
    /// <param name="OnNewLine">The <see cref="TOnNewLine"/> callback procedure to be invoked for each line of output.</param>
    /// <param name="Options">A set of <see cref="TCmdOptions"/> to control execution behavior. Defaults to an empty set.</param>
    /// <returns>True if the command executed successfully (exit code 0), False otherwise.</returns>
    /// <remarks>
    /// Unlike the overload returning <c>TStringList</c>, this method returns <c>False</c> for non-zero exit codes
    /// instead of raising an <see cref="ELX4DCmdLine"/> for that specific case. However, <see cref="ELX4DCmdLine"/>
    /// may still be raised for other execution issues (e.g., failure to start process).
    /// </remarks>
    class function ExecuteCommandLineSynch(const Command: string; OnNewLine: TOnNewLine;
      Options: TCmdOptions = []): boolean; overload; static;

    /// <summary>
    /// Executes a command line asynchronously. Output and termination are reported via callbacks.
    /// </summary>
    /// <param name="Command">The command line string to execute.</param>
    /// <param name="OnNewLine">The <see cref="TOnNewLine"/> callback procedure invoked for each new line of output.</param>
    /// <param name="OnCommandTerminated">The <see cref="TOnCommandTerminated"/> callback procedure invoked when the command finishes execution.</param>
    /// <param name="Options">A set of <see cref="TCmdOptions"/> to control execution behavior. Defaults to an empty set.</param>
    /// <remarks>
    /// This method returns immediately after attempting to start the process.
    /// Command output and termination status are handled via the provided callback procedures.
    /// Ensure that <see cref="AppIsTerminated"/> is checked within callbacks if the application
    /// might close while the asynchronous command is running.
    /// </remarks>
    /// <exception cref="ELX4DCmdLine">
    /// May be raised if the process cannot be started.
    /// Errors during execution or non-zero exit codes are reported via the <c>OnCommandTerminated</c> callback.
    /// </exception>
    class procedure ExecuteCommandLineAsynch(const Command: string; OnNewLine: TOnNewLine;
      OnCommandTerminated: TOnCommandTerminated; Options: TCmdOptions = []); static;

    /// <summary>
    /// Executes a bash script file synchronously and returns all its output lines as a <c>TStringList</c>.
    /// </summary>
    /// <param name="BashFile">The full path or a relative path to the bash script file to execute.</param>
    /// <param name="Options">A set of <see cref="TCmdOptions"/> to control execution behavior. Defaults to an empty set.</param>
    /// <returns>
    /// A <c>TStringList</c> containing all lines from the script's standard output
    /// (and standard error if <c>CatchStdError</c> is specified in <c>Options</c>).
    /// The caller is responsible for freeing this <c>TStringList</c> instance.
    /// </returns>
    /// <remarks>
    /// This method executes the script using a command like "bash [BashFile]".
    /// Ensure the script file exists and has appropriate permissions if required by the OS.
    /// </remarks>
    /// <exception cref="ELX4DCmdLine">
    /// Raised if the script file is not found, or fails to start.
    /// </exception>
    class function ExecuteBashFileSynch(const BashFile: string;
      Options: TCmdOptions = []): TStringList; overload; static;

    /// <summary>
    /// Executes a bash script file synchronously, invoking a callback for each new line of output.
    /// </summary>
    /// <param name="BashFile">The full path or a relative path to the bash script file to execute.</param>
    /// <param name="OnNewLine">The <see cref="TOnNewLine"/> callback procedure to be invoked for each line of output.</param>
    /// <param name="Options">A set of <see cref="TCmdOptions"/> to control execution behavior. Defaults to an empty set.</param>
    /// <returns>True if the script executed successfully (exit code 0), False otherwise.</returns>
    /// <remarks>
    /// This method executes the script using a command like "bash [BashFile]".
    /// Ensure the script file exists and has appropriate permissions.
    /// Unlike the overload returning <c>TStringList</c>, this method returns <c>False</c> for non-zero exit codes
    /// instead of raising an <see cref="ELX4DCmdLine"/> for that specific case. However, <see cref="ELX4DCmdLine"/>
    /// may still be raised for other execution issues (e.g., script not found, failure to start interpreter).
    /// </remarks>
    class function ExecuteBashFileSynch(const BashFile: string; OnNewLine: TOnNewLine;
      Options: TCmdOptions = []): boolean; overload; static;

    /// <summary>
    /// Executes a bash script file asynchronously. Output and termination are reported via callbacks.
    /// </summary>
    /// <param name="BashFile">The full path to the bash script file to execute.</param>
    /// <param name="OnNewLine">The <see cref="TOnNewLine"/> callback procedure invoked for each new line of output.</param>
    /// <param name="OnCommandTerminated">The <see cref="TOnCommandTerminated"/> callback procedure invoked when the script finishes execution.</param>
    /// <param name="Options">A set of <see cref="TCmdOptions"/> to control execution behavior. Defaults to an empty set.</param>
    /// <remarks>
    /// This method returns immediately after attempting to start the script execution.
    /// Script output and termination status are handled via the provided callback procedures.
    /// Ensure that <see cref="AppIsTerminated"/> is checked within callbacks if the application
    /// might close while the asynchronous script is running.
    /// The script is executed via "bash [BashFile]".
    /// </remarks>
    /// <exception cref="ELX4DCmdLine">
    /// May be raised if the process cannot be started (e.g., script not found).
    /// Errors during execution or non-zero exit codes are reported via the <c>OnCommandTerminated</c> callback.
    /// </exception>
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
{$IF Defined(FMX)}
  result := Application.Terminated;
{$ELSE}
  result := false;
{$ENDIF}
end;

class function TLX4DCmdLine.StartCommand(const Command: string; RunWithRootPrivileges: boolean): boolean;
var
  Cmd: AnsiString;
begin
  if RunWithRootPrivileges then
    Cmd := 'pkexec ' + AnsiString(Command)
  else
    Cmd := AnsiString(Command);
  result := _system(PAnsiChar(Cmd)) = 0;
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
