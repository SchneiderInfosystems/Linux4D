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
  TLX4DPath = record
  private
    class var FUserDirs: TStringList;
    class function GetXDGPath(const Key, DefaultFolder: string): string; static;
  public
    class destructor ClassDestroy;
    class function GetDesktopPath: string; static;
    class function GetDownloadsPath: string; static;
    class function GetPublicPath: string; static;
    class function GetDocumentsPath: string; static;
    class function GetMusicPath: string; static;
    class function GetPicturesPath: string; static;
    class function GetMoviesPath: string; static;
  end;

  TLX4DFile = record
    class function MakeFileExecutable(const FilePath: string): boolean; static;
    class function SetOwnerShip(const FilePath: string): boolean; static;

    class function GetGIOInfo(const FilePath: string): TStringList; static;
    class function MarkFileTrusted(const FilePath: string): boolean; static;
  end;

implementation

uses
  LX4D.SystemInfo, LX4D.CmdLine,
  Posix.Unistd, Posix.Pwd, Posix.SysStat;

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

class function TLX4DFile.MakeFileExecutable(const FilePath: string): boolean;
begin
  result := chmod(PAnsiChar(AnsiString(FilePath)),
    S_IRUSR or S_IWUSR or S_IXUSR or  // User: rwx
    S_IRGRP or S_IWGRP or S_IXGRP or  // Group: rwx
    S_IROTH or S_IXOTH) = 0;          // Other: r-x
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
