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

unit LX4D.IOUtils;

interface

uses
  System.Types, System.Classes, System.SysUtils, System.IOUtils;

type
  /// <summary>
  /// Provides static helper methods for retrieving system pathes comparable to System.IOUtils.Path.
  /// </summary>
  /// <remarks>
  /// This record provides the correct path information on non-English Linux installations and also
  /// returns correct information if the function was started with root privileges.
  /// </remarks>
  TLX4DPath = record
  private
    class var FUserDirs: TStringList;
    class function GetXDGPath(const Key, DefaultFolder: string): string; static;
  public
    class destructor ClassDestroy;

    /// <summary>
    /// Gets the full path to the current user's Desktop directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Desktop directory (e.g., "/home/user/Desktop").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_DESKTOP_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetDesktopPath: string; static;

    /// <summary>
    /// Gets the full path to the current user's Downloads directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Downloads directory (e.g., "/home/user/Downloads").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_DOWNLOAD_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetDownloadsPath: string; static;

    /// <summary>
    /// Gets the full path to the current user's Public share directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Public directory (e.g., "/home/user/Public").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_PUBLICSHARE_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetPublicPath: string; static;

    /// <summary>
    /// Gets the full path to the current user's Documents directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Documents directory (e.g., "/home/user/Documents").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_DOCUMENTS_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetDocumentsPath: string; static;

    /// <summary>
    /// Gets the full path to the current user's Music directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Music directory (e.g., "/home/user/Music").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_MUSIC_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetMusicPath: string; static;

    /// <summary>
    /// Gets the full path to the current user's Pictures directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Pictures directory (e.g., "/home/user/Pictures").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_PICTURES_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetPicturesPath: string; static;

    /// <summary>
    /// Gets the full path to the current user's Videos (or Movies) directory on Linux.
    /// </summary>
    /// <returns>
    /// The absolute path to the Videos/Movies directory (e.g., "/home/user/Videos").
    /// Returns an empty string if the path cannot be determined.
    /// </returns>
    /// <remarks>
    /// This corresponds to the XDG_VIDEOS_DIR environment variable.
    /// This function can also be executed when running with root rights.
    /// </remarks>
    class function GetMoviesPath: string; static;
  end;

  /// <summary>
  /// Provides static helper methods for file operations specific to or commonly used on Linux systems.
  /// </summary>
  /// <remarks>
  /// This record facilitates tasks like managing file permissions, ownership,
  /// and interacting with GIO metadata.
  /// </remarks>
  TLX4DFile = record
  public type
    /// <summary>
    /// Represents a basic file permission type (Read, Write, Execute).
    /// </summary>
    TPermission = (Read, Write, Execute);
    /// <summary>
    /// A set of <see cref="TPermission"/> flags, representing a combination of
    /// read, write, and execute permissions.
    /// </summary>
    TPermissions = set of TPermission;
  public
    /// <summary>
    /// Sets the permissions for a file on a Linux system for user, group, and others.
    /// </summary>
    /// <param name="FilePath">The full path to the file whose permissions are to be set.</param>
    /// <param name="User">The set of <see cref="TPermissions"/> for the file owner.</param>
    /// <param name="Group">The set of <see cref="TPermissions"/> for the group owner.</param>
    /// <param name="Other">The set of <see cref="TPermissions"/> for all other users.</param>
    /// <returns>
    /// True if the permissions were successfully set, False otherwise (e.g., file not found,
    /// insufficient privileges to change permissions).
    /// </returns>
    /// <remarks>
    /// This method corresponds to using the <c>chmod</c> command or the <c>chmod()</c> system call.
    /// The current user must have the necessary privileges to change permissions on the specified file.
    /// </remarks>
    class function SetFilePermissions(const FilePath: string; User, Group, Other: TPermissions): boolean; static;

    /// <summary>
    /// Makes a file executable for the owner, group, and others on a Linux system.
    /// </summary>
    /// <param name="FilePath">The full path to the file to be made executable.</param>
    /// <returns>
    /// True if the file was successfully made executable, False otherwise (e.g., file not found,
    /// insufficient privileges).
    /// </returns>
    /// <remarks>
    /// This is a convenience function that sets the execute bit for all
    /// three categories (user, group, other), akin to <c>chmod a+x FilePath</c>.
    /// It effectively calls <see cref="SetFilePermissions"/> with the execute flag added to existing permissions
    /// for user, group, and other, or simply sets permissions like 755 if only execute is considered.
    /// The exact behavior depends on the implementation (e.g., whether it preserves existing read/write bits).
    /// For more granular control, use <see cref="SetFilePermissions"/>.
    /// </remarks>
    class function MakeFileExecutable(const FilePath: string): boolean; static;

    /// <summary>
    /// Sets the ownership of a file to the current effective user and group on a Linux system.
    /// </summary>
    /// <param name="FilePath">The full path to the file whose ownership is to be set.</param>
    /// <returns>
    /// True if the ownership was successfully set (or was already correct), False otherwise
    /// (e.g., file not found, insufficient privileges - typically requires root to change
    /// ownership to a *different* user).
    /// </returns>
    /// <remarks>
    /// This method corresponds to using the <c>chown</c> command or the <c>chown()</c> system call.
    /// Changing ownership to an arbitrary user/group usually requires root privileges.
    /// This specific signature implies setting it to the *current effective user and group*.
    /// If you need to set specific user/group IDs, a different method signature would be required.
    /// </remarks>
    class function SetOwnerShip(const FilePath: string): boolean; static;

    /// <summary>
    /// Retrieves GIO (GNOME I/O) information for a given file or URI.
    /// </summary>
    /// <param name="FilePath">The full path or URI to the file/resource.</param>
    /// <returns>
    /// A <c>TStringList</c> containing lines of information as reported by the <c>gio info</c> command.
    /// Returns an empty or nil <c>TStringList</c> if GIO is not available, the command fails,
    /// or the file is not found. The caller is responsible for freeing the returned <c>TStringList</c>.
    /// </returns>
    /// <remarks>
    /// This method relies on the <c>gio</c> command-line tool being available on the system.
    /// It provides access to metadata like MIME type, GIO attributes (e.g., "metadata::trusted"), etc.
    /// </remarks>
    class function GetGIOInfo(const FilePath: string): TStringList; static;

    /// <summary>
    /// Marks a file as "trusted" using GIO metadata on a Linux system.
    /// </summary>
    /// <param name="FilePath">The full path to the file to be marked as trusted.</param>
    /// <returns>
    /// True if the file was successfully marked as trusted, False otherwise (e.g., GIO not available,
    /// command fails, insufficient privileges, file not found).
    /// </returns>
    /// <remarks>
    /// This sets the <c>metadata::trusted</c> GIO attribute to 'true'.
    /// Marking a desktop entry file (.desktop) as trusted allows it to be launched without warnings
    /// in some desktop environments if it was downloaded or is not executable by default.
    /// Relies on the <c>gio set</c> command-line tool.
    /// </remarks>
    class function MarkFileTrusted(const FilePath: string): boolean; static;
  end;

implementation

uses
  LX4D.SystemInfo, LX4D.CmdLine,
  Posix.Unistd, Posix.Pwd, Posix.SysTypes, Posix.SysStat;

const
  // System Files
  ConfigUserDirs = '/.config/user-dirs.dirs';
  HomeRoot = '/home/';

  // Sytem Environment Variables
  HOME = 'HOME';

{ TLX4DPath }

class destructor TLX4DPath.ClassDestroy;
begin
  FreeAndNil(FUserDirs);
end;

class function TLX4DPath.GetDesktopPath: string;
begin
  result := GetXDGPath('XDG_DESKTOP_DIR', 'Desktop');
end;

class function TLX4DPath.GetDocumentsPath: string;
begin
  result := GetXDGPath('XDG_DOWNLOAD_DIR', 'Downloads');
end;

class function TLX4DPath.GetDownloadsPath: string;
begin
  result := GetXDGPath('XDG_PUBLICSHARE_DIR', 'Public');
end;

class function TLX4DPath.GetMoviesPath: string;
begin
  result := GetXDGPath('XDG_DOCUMENTS_DIR', 'Documents');
end;

class function TLX4DPath.GetMusicPath: string;
begin
  result := GetXDGPath('XDG_MUSIC_DIR', 'Music');
end;

class function TLX4DPath.GetPicturesPath: string;
begin
  result := GetXDGPath('XDG_PICTURES_DIR', 'Pictures');
end;

class function TLX4DPath.GetPublicPath: string;
begin
  result := GetXDGPath('XDG_VIDEOS_DIR', 'Videos');
end;

class function TLX4DPath.GetXDGPath(const Key, DefaultFolder: string): string;
var
  HomeDir, UserDirsFileName: string;
begin
  if TLX4DSystemInfo.RunWithRootRights then
  begin
    HomeDir := HomeRoot + TLX4DSystemInfo.UserName;
    result := TPath.Combine(HomeDir, DefaultFolder); // Fallback solution
    if not assigned(FUserDirs) then
    begin
      FUserDirs := TStringList.Create;
      UserDirsFileName := HomeDir + ConfigUserDirs;
      if FileExists(UserDirsFileName) then
        FUserDirs.LoadFromFile(UserDirsFileName, TLX4DSystemInfo.Encoding);
    end;
  end else begin
    HomeDir := GetEnvironmentVariable(HOME);
    result := TPath.Combine(HomeDir, DefaultFolder); // Fallback solution
    if not assigned(FUserDirs) then
    begin
      FUserDirs := TStringList.Create;
      UserDirsFileName := HomeDir + ConfigUserDirs;
      if FileExists(UserDirsFileName) then
        FUserDirs.LoadFromFile(UserDirsFileName, TLX4DSystemInfo.Encoding);
    end;
  end;
  result := FUserDirs.Values[Key];
  if not result.IsEmpty then
    result := StringReplace(result.DeQuotedString('"'), '$' + HOME, HomeDir, []);
end;

{ TLX4DFile }

class function TLX4DFile.SetFilePermissions(const FilePath: string; User, Group, Other: TPermissions): boolean;
var
  Mode: mode_t;
begin
  if FileExists(FilePath) then
  begin
    Mode := 0;
    if Read in User then
      Mode := Mode + S_IRUSR;
    if Write in User then
      Mode := Mode + S_IWUSR;
    if Execute in User then
      Mode := Mode + S_IXUSR;
    if Read in Group then
      Mode := Mode + S_IRGRP;
    if Write in Group then
      Mode := Mode + S_IWGRP;
    if Execute in Group then
      Mode := Mode + S_IXGRP;
    if Read in Other then
      Mode := Mode + S_IROTH;
    if Write in Other then
      Mode := Mode + S_IWOTH;
    if Execute in Other then
      Mode := Mode + S_IXOTH;
    result := chmod(PAnsiChar(AnsiString(FilePath)), Mode) = 0;
  end else
    result := false;
end;

class function TLX4DFile.MakeFileExecutable(const FilePath: string): boolean;
begin
  result := SetFilePermissions(FilePath, [Read, Write, Execute], [Read, Write, Execute], [Read, Execute]);
end;

class function TLX4DFile.SetOwnerShip(const FilePath: string): boolean;
var
  pwd: PPasswd;
begin
  pwd := getpwnam(PAnsiChar(AnsiString(TLX4DSystemInfo.UserName)));
  if Assigned(pwd) then
    result := chown(PAnsiChar(AnsiString(FilePath)), pwd^.pw_uid, pwd^.pw_gid) = 0
  else
    result := false;
end;

class function TLX4DFile.GetGIOInfo(const FilePath: string): TStringList;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

class function TLX4DFile.MarkFileTrusted(const FilePath: string): boolean;
begin
  raise ELX4DCmdLine.Create(rsFunctionAvailableInExtendedLinux4DOnly);
end;

end.
