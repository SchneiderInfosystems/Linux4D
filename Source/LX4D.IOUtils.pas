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

implementation

const
  // System Files
  ConfigUserDirs = '/.config/user-dirs.dirs';

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
  HomeDir := GetEnvironmentVariable(HOME);
  result := TPath.Combine(HomeDir, DefaultFolder); // Fallback solution
  if not assigned(FUserDirs) then
  begin
    FUserDirs := TStringList.Create;
    UserDirsFileName := HomeDir + ConfigUserDirs;
    if FileExists(UserDirsFileName) then
      FUserDirs.LoadFromFile(UserDirsFileName, TEncoding.UTF8);
  end;
  result := FUserDirs.Values[Key];
  if not result.IsEmpty then
    result := StringReplace(result.DeQuotedString('"'), '$' + HOME, HomeDir, []);
end;

end.
