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

unit LX4D.SystemInfo;

interface

uses
  System.Types, System.Classes, System.SysUtils, System.RTTI;

type
  TLX4DSystemInfo = record
  public type
    TDistributionKind = (Unknown, Ubuntu, Debian, RedHat, Fedora);
    TDistribution = record
      Kind: TDistributionKind;
      ID: string;
      BaseID: string;
      CodeName: string;
      PrettyName: string;
      Release: double;
      Details: TStringList;
      SourceFile: string;
    end;
    TKernel = record
      SystemName: string;
      NodeName: string;
      Release: string;
      Machine: string;
      function PrettyName: string;
    end;
    TCPU = record
      ModelName: string;
      VendorID: string;
      CPUMHz: Double;
      LogicalCores: Integer;
      FPU: boolean;
      CacheSize: string;
      Details: TStringList;
      function PrettyName: string;
    end;
  private
    class var FUID: cardinal;
    class var FRunWithRootRights: boolean;
    class var FUserName: string;
    class var FDistribution: TDistribution;
    class var FKernel: TKernel;
    class var FCPU: TCPU;
    class var FLanguage: string;
    class var FRegion: string;
    class var FEncoding: string;
    class procedure FetchUser; static;
    class procedure FetchUserWithRootRights; static;
    class procedure FetchLinuxDistribution; static;
    class function GetDistrubutionKindStr: string; static;
    class procedure FetchKernel; static;
    class procedure FetchCPU; static;
    class procedure FetchLanguage; static;
    class function GetLanguagePretty: string; static;
    class function ReadProcFile(const FilePath: string): TStringList; static;
    class function YesNoStrToBool(const BoolStr: string): boolean; static;
  public
    class constructor Create;
    class destructor Destroy;
    class function ToJSON: string; static;
    class property UserID: cardinal read FUID;
    class property UserName: string read FUserName;
    class property RunWithRootRights: boolean read FRunWithRootRights;
    class property Distribution: TDistribution read FDistribution;
    class property DistrubutionKindStr: string read GetDistrubutionKindStr;
    class property Kernel: TKernel read FKernel;
    class property CPU: TCPU read FCPU;
    class property LanguagePretty: string read GetLanguagePretty;
    class property Language: string read FLanguage;
    class property Region: string read FRegion;
    class property Encoding: string read FEncoding;
  end;

implementation

uses
  Posix.Unistd, Posix.Stdlib, Posix.Pwd, Posix.SysUtsname;

const
  // System Files
  OSReleaseFile = '/etc/os-release';
  LSBReleaseFile = '/etc/lsb-release';
  CPUINFO = '/proc/cpuinfo';

  // Sytem Environment Variables
  PKEXEC_UID = 'PKEXEC_UID';
  SUDO_USER = 'SUDO_USER';
  LOGNAME = 'LOGNAME';
  USER = 'USER';
  LANG = 'LANG';

class constructor TLX4DSystemInfo.Create;
begin
  FetchUser;
  FetchLinuxDistribution;
  FetchKernel;
  FetchCPU;
  FetchLanguage;
end;

class destructor TLX4DSystemInfo.Destroy;
begin
  FreeAndNil(FDistribution.Details);
  FreeAndNil(FCPU.Details);
end;

class procedure TLX4DSystemInfo.FetchUser;
var
  pw: PPasswd;
begin
  FUID := geteuid;
  FRunWithRootRights := FUID = 0;
  if FRunWithRootRights then
    FetchUserWithRootRights
  else begin
    pw := getpwuid(FUID);
    if assigned(pw) then
      FUserName := string(pw^.pw_name)
    else
      FUserName := '';
  end;
end;

class procedure TLX4DSystemInfo.FetchUserWithRootRights;
var
  Env: PAnsiChar;
  UID: cardinal;
  pw: PPasswd;
begin
  FUserName := '?'; // Fallback
  Env := getenv(PKEXEC_UID);
  if assigned(Env) then
  begin
    UID := StrToIntDef(string(Env), -1);
    if UID > 0 then
    begin
      pw := getpwuid(UID);
      if assigned(pw) then
        FUserName := string(pw^.pw_name);
    end;
  end;
  Env := getenv(SUDO_USER);
  if assigned(Env) then
    FUserName := string(Env)
  else begin
    Env := getenv(LOGNAME);
    if assigned(Env) then
      FUserName := string(Env)
    else begin
      Env := getenv(USER);
      if assigned(Env) then
        FUserName := string(Env);
    end;
  end;
end;

class procedure TLX4DSystemInfo.FetchLinuxDistribution;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  FDistribution.Kind := Unknown;
  FDistribution.Details := TStringList.Create;
  if FileExists(OSReleaseFile) then
  begin
    FDistribution.SourceFile := OSReleaseFile;
    FDistribution.Details.LoadFromFile(OSReleaseFile);
    FDistribution.ID := FDistribution.Details.Values['ID'].DeQuotedString('"');
    FDistribution.BaseID := FDistribution.Details.Values['ID_LIKE'].DeQuotedString('"');
    FDistribution.Release := StrToFloatDef(FDistribution.Details.Values['VERSION_ID'].DeQuotedString('"'), 0, fs);
    FDistribution.CodeName := FDistribution.Details.Values['VERSION_CODENAME'].DeQuotedString('"');
    if FDistribution.CodeName.IsEmpty then
    begin
      FDistribution.CodeName := FDistribution.Details.Values['VERSION'].DeQuotedString('"');
      if FDistribution.CodeName.Contains(' ') then
      begin
        FDistribution.CodeName := FDistribution.CodeName.SubString(Pos(' ', FDistribution.CodeName));
        if FDistribution.CodeName.StartsWith('(') and FDistribution.CodeName.EndsWith(')') then
          FDistribution.CodeName := FDistribution.CodeName.SubString(1, FDistribution.CodeName.Length - 2);
      end;
    end;
    FDistribution.PrettyName := FDistribution.Details.Values['PRETTY_NAME'].DeQuotedString('"');
  end
  else if FileExists(LSBReleaseFile) then
  begin
    FDistribution.SourceFile := LSBReleaseFile;
    FDistribution.Details.LoadFromFile(LSBReleaseFile);
    FDistribution.ID := FDistribution.Details.Values['DISTRIB_ID'].DeQuotedString('"');
    FDistribution.Release := StrToFloatDef(FDistribution.Details.Values['DISTRIB_RELEASE'].DeQuotedString('"'), 0, fs);
    FDistribution.CodeName := FDistribution.Details.Values['DISTRIB_CODENAME'].DeQuotedString('"');
    FDistribution.PrettyName := FDistribution.Details.Values['DISTRIB_DESCRIPTION'].DeQuotedString('"');
  end;
  if SameText(FDistribution.ID, 'Ubuntu') then
    FDistribution.Kind := Ubuntu
  else if SameText(FDistribution.ID, 'debian') then
    FDistribution.Kind := Debian
  else if FDistribution.ID.Contains('rhel') then
    FDistribution.Kind := RedHat
  else if FDistribution.ID.Contains('fedora') then
    FDistribution.Kind := Fedora;
end;

class function TLX4DSystemInfo.GetDistrubutionKindStr: string;
begin
  result := TRttiEnumerationType.GetName<TDistributionKind>(FDistribution.Kind);
end;

class procedure TLX4DSystemInfo.FetchKernel;
var
  UtsName: TUTSName;
begin
  if uname(UtsName) = 0 then
  begin
    FKernel.SystemName := string(UtsName.sysname);
    FKernel.NodeName := string(UtsName.nodename);
    FKernel.Release := string(UtsName.release);
    FKernel.Machine := string(UtsName.machine);
  end;
end;

class procedure TLX4DSystemInfo.FetchLanguage;
var
  Language: string;
  strs: TArray<string>;
begin
  Language := GetEnvironmentVariable(LANG);
  if Language.Contains('.') then
  begin
    strs := Language.Split(['.']);
    FLanguage := strs[0];
    if length(strs) = 2 then
      FEncoding := strs[1];
    strs := FLanguage.Split(['_']);
    if length(strs) = 2 then
    begin
      FLanguage := strs[0];
      FRegion := strs[1];
    end;
  end
  else if not Language.IsEmpty then
    FLanguage := Language;
end;

class function TLX4DSystemInfo.GetLanguagePretty: string;
begin
  if FRegion.IsEmpty then
    result := FLanguage
  else
    result := FLanguage + ' (' + FRegion + ')';
end;

class function TLX4DSystemInfo.ReadProcFile(const FilePath: string): TStringList;
var
  F: TextFile;
  Line: string;
begin
  result := TStringList.Create;
  AssignFile(F, FilePath);
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      result.Add(Line);
    end;
  finally
    CloseFile(F);
  end;
end;

class function TLX4DSystemInfo.YesNoStrToBool(const BoolStr: string): boolean;
begin
  if SameText(BoolStr, 'yes') or SameText(BoolStr, 'true') then
    result := true
  else
    result := false;
end;

class procedure TLX4DSystemInfo.FetchCPU;
var
  Line, Key, Value: string;
  Details: TStringList;
  strs: TArray<string>;
begin
  FCPU.Details := TStringList.Create;
  if FileExists(CPUINFO) then
  begin
    Details := ReadProcFile(CPUINFO);
    try
      FCPU.LogicalCores := 0;
      for Line in Details do
      begin
        strs := Line.Split([':']);
        if length(Strs) >= 2 then
        begin
          Key := trim(strs[0]);
          Value := trim(strs[1]);
          FCPU.Details.Add(Key + '=' + Value);
          if SameText(Key, 'model name') and (FCPU.ModelName = '') then
            FCPU.ModelName := Value
          else if SameText(Key, 'vendor_id') and (FCPU.VendorID = '') then
            FCPU.VendorID := Value
          else if SameText(Key, 'cpu MHz') and (FCPU.CPUMHz = 0) then
            FCPU.CPUMHz := StrToFloatDef(Value, 0)
          else if SameText(Key, 'cache size') and (FCPU.CacheSize = '') then
            FCPU.CacheSize := Value
          else if SameText(Key, 'processor') then
            inc(FCPU.LogicalCores)
          else if SameText(Key, 'fpu') then
            FCPU.FPU := YesNoStrToBool(Value);
        end else if not trim(Line).IsEmpty then
          FCPU.Details.Add('//Format? ' + Line);
      end;
    finally
      Details.Free;
    end;
  end else
    FCPU.Details.Add('File not found: ' + CPUINFO);
end;

class function TLX4DSystemInfo.ToJSON: string;
begin
  result := Format(
    '{"UserID":%d,"UserName":"%s","Distribution":"%s","Language":"%s","Encoding":"%s","Kernel":"%s","CPU":"%s"}',
    [FUID, FUserName, FDistribution.PrettyName, GetLanguagePretty, FEncoding, FKernel.PrettyName, FCPU.PrettyName]);
end;

{ TLX4DSystemInfo.TKernel }

function TLX4DSystemInfo.TKernel.PrettyName: string;
begin
  result := Format('%s %s (%s), Net: %s', [SystemName, Release, Machine, NodeName]);
end;

{ TLX4DSystemInfo.TCPU }

function TLX4DSystemInfo.TCPU.PrettyName: string;
begin
  result := Format('%s (%s) (%d Cores, %4.2f MHz), CacheSize: %s, FPU: %s',
    [ModelName, VendorID, LogicalCores, CPUMHz, CacheSize, BoolToStr(FPU, true)]);
end;

end.
