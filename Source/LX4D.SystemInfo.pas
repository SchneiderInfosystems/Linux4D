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
  System.Types, System.Classes, System.SysUtils, System.RTTI, System.JSON;

type
  TLX4DSystemInfo = record
  public type
    /// <summary>
    /// Defines a type to differentiate the Linux distribution. This list will be extended in the future.
    /// </summary>
    TDistributionKind = (Unknown, Ubuntu, Mint, Debian, RedHat, Fedora);

    /// <summary>
    /// This record contains detailed information about the installed Linux distribution.
    /// </summary>
    TDistribution = record
      Kind: TDistributionKind;
      ID: string;
      BaseID: string;
      CodeName: string;
      PrettyName: string;
      Release: double;
      Details: TStringList;
      SourceFile: string;
      function KindStr: string;
      function ToJSON: TJSONObject;
    end;

    /// <summary>
    /// This record contains detailed information about the Linux kernel system.
    /// </summary>
    TKernel = record
      SystemName: string;
      NodeName: string;
      Release: string;
      Machine: string;
      function PrettyName: string;
      function ToJSON: TJSONObject;
    end;

    /// <summary>
    /// This record contains detailed information about the CPUs.
    /// </summary>
    TCPU = record
      ModelName: string;
      VendorID: string;
      CPUMHz: Double;
      LogicalCores: Integer;
      FPU: boolean;
      CacheSize: string;
      Details: TStringList;
      function PrettyName: string;
      function ToJSON: TJSONObject;
    end;

    /// <summary>
    /// This record contains detailed information about the system memory.
    /// </summary>
    TMemory = record
      Total: Int64;
      Free: Int64;
      Available: Int64; // This value not only takes into account the completely free memory (Free),
                        // but also the memory that can be reclaimed in the short term by releasing caches and buffers
      Buffers: Int64;
      Cached: Int64;
      Details: TStringList;
      function PrettyFormat: string;
      class operator Finalize(var Dest: TMemory);
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
    class var FEncoding: TEncoding;
    class var FEncodingStr: string;
    class procedure FetchUser; static;
    class procedure FetchUserWithRootRights; static;
    class procedure FetchLinuxDistribution; static;
    class procedure FetchKernel; static;
    class procedure FetchCPU; static;
    class procedure FetchLanguage; static;
    class function GetLanguagePretty: string; static;
    class function ReadProcFile(const FilePath: string): TStringList; static;
    class function YesNoStrToBool(const BoolStr: string): boolean; static;
    class function GetEnvironmentVariables: TStringList; static;
    class function GetMemory: TMemory; static;
    class function GetLibVersion: string; static;
  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>
    /// Returns all collected system information in JSON format as a string.
    /// </summary>
    class function ToJSON: TJSONObject; static;

    /// <summary>
    /// Returns the user ID (UID) under which the process is running.
    /// </summary>
    class property UserID: cardinal read FUID;

    /// <summary>
    /// Returns the name of the user running the current process.
    /// </summary>
    class property UserName: string read FUserName;

    /// <summary>
    /// Indicates whether the application is running with root privileges.
    /// </summary>
    class property RunWithRootRights: boolean read FRunWithRootRights;

    /// <summary>
    /// Returns information about the currently used Linux distribution.
    /// </summary>
    class property Distribution: TDistribution read FDistribution;

    /// <summary>
    /// Returns information about the current kernel, such as system name, version, and architecture.
    /// </summary>
    class property Kernel: TKernel read FKernel;

    /// <summary>
    /// Returns information about the CPU, including model, speed, cache, and core count.
    /// </summary>
    class property CPU: TCPU read FCPU;

    /// <summary>
    /// Returns information about the current Memory state.
    /// </summary>
    class property Memory: TMemory read GetMemory;

    /// <summary>
    /// Returns the current language in a human-readable format (e.g., "English (United States)").
    /// </summary>
    class property LanguagePretty: string read GetLanguagePretty;

    /// <summary>
    /// Returns the configured language code (e.g., "en", "de").
    /// </summary>
    class property Language: string read FLanguage;

    /// <summary>
    /// Returns the configured region code (e.g., "US", "DE").
    /// </summary>
    class property Region: string read FRegion;

    /// <summary>
    /// Returns the character encoding in use (e.g., "UTF-8").
    /// </summary>
    class property EncodingStr: string read FEncodingStr;

    /// <summary>
    /// Returns the encoding class for the character encoding in use (e.g.,TEncoding.UTF8).
    /// </summary>
    class property Encoding: TEncoding read FEncoding;

    /// <summary>
    /// Returns the list of the current environment variables as key-value pairs.
    /// </summary>
    class property EnvironmentVariables: TStringList read GetEnvironmentVariables;

    /// <summary>
    /// Returns the version number of the LX4D library.
    /// </summary>
    class property LibVersion: string read GetLibVersion;
  end;

implementation

uses
  Posix.Unistd, Posix.Stdlib, Posix.Pwd, Posix.SysUtsname, Posix.Base, Posix.Dlfcn;

{$I LX4DVersion.inc}

const
  // System Files
  OSReleaseFile = '/etc/os-release';
  LSBReleaseFile = '/etc/lsb-release';
  CPUINFO = '/proc/cpuinfo';
  MEMINFO = '/proc/meminfo';

  // Sytem Environment Variables
  PKEXEC_UID = 'PKEXEC_UID';
  SUDO_USER = 'SUDO_USER';
  SUDO_UID = 'SUDO_UID';
  LOGNAME = 'LOGNAME';
  USER = 'USER';
  LANG = 'LANG';
  DefaultLocale = '<default locale>';

class constructor TLX4DSystemInfo.Create;
begin
  FetchLanguage;
  FetchUser;
  FetchLinuxDistribution;
  FetchKernel;
  FetchCPU;
end;

class destructor TLX4DSystemInfo.Destroy;
begin
  FreeAndNil(FDistribution.Details);
  FreeAndNil(FCPU.Details);
end;

class function TLX4DSystemInfo.GetLibVersion: string;
begin
  result := cLibEdition + ' V' + cLibMajorVersion.ToString + '.' + cLibMinorVersion.ToString + '.' + cLibReleaseVersion.ToString + '.' +
    cLibBuildVersion.ToString;
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
  pw: PPasswd;
begin
  FUserName := '?'; // Fallback
  Env := getenv(PKEXEC_UID);
  if assigned(Env) then
  begin
    FUID := StrToIntDef(string(Env), -1);
    if FUID > 0 then
    begin
      pw := getpwuid(FUID);
      if assigned(pw) then
      begin
        FUserName := string(pw^.pw_name);
        exit;
      end;
    end;
  end;
  Env := getenv(SUDO_USER);
  if assigned(Env) then
  begin
    FUserName := string(Env);
    Env := getenv(SUDO_UID);
    if assigned(Env) then
    begin
      FUID := StrToIntDef(string(Env), -1);
      exit;
    end;
  end
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
    FDistribution.Details.LoadFromFile(OSReleaseFile, Encoding);
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
    FDistribution.Details.LoadFromFile(LSBReleaseFile, Encoding);
    FDistribution.ID := FDistribution.Details.Values['DISTRIB_ID'].DeQuotedString('"');
    FDistribution.Release := StrToFloatDef(FDistribution.Details.Values['DISTRIB_RELEASE'].DeQuotedString('"'), 0, fs);
    FDistribution.CodeName := FDistribution.Details.Values['DISTRIB_CODENAME'].DeQuotedString('"');
    FDistribution.PrettyName := FDistribution.Details.Values['DISTRIB_DESCRIPTION'].DeQuotedString('"');
  end;
  if SameText(FDistribution.ID, 'Ubuntu') then
    FDistribution.Kind := Ubuntu
  else if FDistribution.ID.Contains('linuxmint') then
    FDistribution.Kind := Mint
  else if FDistribution.ID.Contains('rhel') then
    FDistribution.Kind := RedHat
  else if FDistribution.BaseID.Contains('debian') then
    FDistribution.Kind := Debian
  else if FDistribution.BaseID.Contains('fedora') then
    FDistribution.Kind := Fedora;
end;

{$REGION 'Kernel'}
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
{$ENDREGION}

{$REGION 'Language'}
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
    begin
      FEncoding := TEncoding.ASCII;
      FEncodingStr := strs[1];
      if SameText(FEncodingStr, 'UTF-8') then
        FEncoding := TEncoding.UTF8
      else if SameText(FEncodingStr, 'ISO-8859-1') or SameText(FEncodingStr, 'Latin-1') then
        FEncoding := TEncoding.ANSI;
    end;
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
  result := ''; // Fallback
  if FLanguage = 'C' then
    FLanguage := DefaultLocale
  else if SameText(FLanguage, 'en') then
  begin
    if SameText(FRegion, 'US') then
      result := 'English (United States)'
    else if SameText(FRegion, 'GB') then
      result := 'English (United Kingdom)'
    else if SameText(FRegion, 'CA') then
      result := 'English (Canada)'
    else if SameText(FRegion, 'AU') then
      result := 'English (Australia)'
    else if SameText(FRegion, 'NZ') then
      result := 'English (New Zealand)'
    else if SameText(FRegion, 'IE') then
      result := 'English (Ireland)'
    else if SameText(FRegion, 'ZA') then
      result := 'English (South Africa)'
    else if SameText(FRegion, 'SG') then
      result := 'English (Singapore)'
    else if SameText(FRegion, 'IN') then
      result := 'English (India)'
    else if FRegion.IsEmpty then
      result := 'English'
    else
      result := 'English (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'de') then
  begin
    if SameText(FRegion, 'DE') then
      result := 'German (Germany)'
    else if SameText(FRegion, 'AT') then
      result := 'German (Austria)'
    else if SameText(FRegion, 'CH') then
      result := 'German (Switzerland)'
    else if SameText(FRegion, 'LI') then
      result := 'German (Liechtenstein)'
    else if SameText(FRegion, 'LU') then
      result := 'German (Luxembourg)'
    else if FRegion.IsEmpty then
      result := 'German'
    else
      result := 'German (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'fr') then
  begin
    if SameText(FRegion, 'FR') then
      result := 'French (France)'
    else if SameText(FRegion, 'CA') then
      result := 'French (Canada)'
    else if SameText(FRegion, 'BE') then
      result := 'French (Belgium)'
    else if SameText(FRegion, 'CH') then
      result := 'French (Switzerland)'
    else if SameText(FRegion, 'LU') then
      result := 'French (Luxembourg)'
    else if SameText(FRegion, 'MC') then
      result := 'French (Monaco)'
    else if FRegion.IsEmpty then
      result := 'French'
    else
      result := 'French (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'es') then
  begin
    if SameText(FRegion, 'ES') then
      result := 'Spanish (Spain)'
    else if SameText(FRegion, 'MX') then
      result := 'Spanish (Mexico)'
    else if SameText(FRegion, 'AR') then
      result := 'Spanish (Argentina)'
    else if SameText(FRegion, 'CO') then
      result := 'Spanish (Colombia)'
    else if SameText(FRegion, 'CL') then
      result := 'Spanish (Chile)'
    else if SameText(FRegion, 'PE') then
      result := 'Spanish (Peru)'
    else if SameText(FRegion, 'VE') then
      result := 'Spanish (Venezuela)'
    else if SameText(FRegion, 'US') then
      result := 'Spanish (United States)'
    else if FRegion.IsEmpty then
      result := 'Spanish'
    else
      result := 'Spanish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'it') then
  begin
    if SameText(FRegion, 'IT') then
      result := 'Italian (Italy)'
    else if SameText(FRegion, 'CH') then
      result := 'Italian (Switzerland)'
    else if FRegion.IsEmpty then
      result := 'Italian'
    else
      result := 'Italian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'pt') then
  begin
    if SameText(FRegion, 'PT') then
      result := 'Portuguese (Portugal)'
    else if SameText(FRegion, 'BR') then
      result := 'Portuguese (Brazil)'
    else if FRegion.IsEmpty then
      result := 'Portuguese'
    else
      result := 'Portuguese (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'zh') then
  begin
    if SameText(FRegion, 'CN') then
      result := 'Chinese (Simplified, China)'
    else if SameText(FRegion, 'TW') then
      result := 'Chinese (Traditional, Taiwan)'
    else if SameText(FRegion, 'HK') then
      result := 'Chinese (Traditional, Hong Kong)'
    else if SameText(FRegion, 'SG') then
      result := 'Chinese (Simplified, Singapore)'
    else if SameText(FRegion, 'MO') then
      result := 'Chinese (Traditional, Macau)' // Macau
    else if FRegion.IsEmpty then
      result := 'Chinese'
    else
      result := 'Chinese (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ja') then
  begin
    if SameText(FRegion, 'JP') or FRegion.IsEmpty then
      result := 'Japanese'
    else
      result := 'Japanese (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ko') then
  begin
    if SameText(FRegion, 'KR') or FRegion.IsEmpty then
      result := 'Korean'
    else
      result := 'Korean (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ru') then
  begin
    if SameText(FRegion, 'RU') or FRegion.IsEmpty then
      result := 'Russian'
    else
      result := 'Russian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'nl') then
  begin
    if SameText(FRegion, 'NL') then
      result := 'Dutch (Netherlands)'
    else if SameText(FRegion, 'BE') then
      result := 'Dutch (Belgium)'
    else if SameText(FRegion, 'SR') then
      result := 'Dutch (Suriname)'
    else if FRegion.IsEmpty then
      result := 'Dutch'
    else
      result := 'Dutch (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'pl') then
  begin
    if SameText(FRegion, 'PL') or FRegion.IsEmpty then
      result := 'Polish'
    else
      result := 'Polish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'sv') then
  begin
    if SameText(FRegion, 'SE') then
      result := 'Swedish (Sweden)'
    else if SameText(FRegion, 'FI') then
      result := 'Swedish (Finland)'
    else if FRegion.IsEmpty then
      result := 'Swedish'
    else
      result := 'Swedish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'no') or SameText(FLanguage, 'nb') or SameText(FLanguage, 'nn') then
  begin
    if SameText(FLanguage, 'nb') or (SameText(FLanguage, 'no') and (SameText(FRegion, 'NO') or FRegion.IsEmpty)) then
      result := 'Norwegian Bokmål'
    else if SameText(FLanguage, 'nn') then
      result := 'Norwegian Nynorsk'
    else if SameText(FLanguage, 'no') then
      result := 'Norwegian';
    if not FRegion.IsEmpty and not SameText(FRegion, 'NO') then
      if result <> '' then
        result := result + ' (' + FRegion + ')'
      else
        result := 'Norwegian (' + FRegion + ')' // Fallback
    else if result = '' then
      result := 'Norwegian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'da') then
  begin
    if SameText(FRegion, 'DK') or FRegion.IsEmpty then
      result := 'Danish'
    else if SameText(FRegion, 'GL') then
      result := 'Danish (Greenland)'
    else
      result := 'Danish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'fi') then
  begin
    if SameText(FRegion, 'FI') or FRegion.IsEmpty then
      result := 'Finnish'
    else
      result := 'Finnish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'cs') then
  begin
    if SameText(FRegion, 'CZ') or FRegion.IsEmpty then
      result := 'Czech'
    else
      result := 'Czech (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'sk') then
  begin
    if SameText(FRegion, 'SK') or FRegion.IsEmpty then
      result := 'Slovak'
    else
      result := 'Slovak (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'hu') then
  begin
    if SameText(FRegion, 'HU') or FRegion.IsEmpty then
      result := 'Hungarian'
    else
      result := 'Hungarian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'el') then
  begin
    if SameText(FRegion, 'GR') then
      result := 'Greek (Greece)'
    else if SameText(FRegion, 'CY') then
      result := 'Greek (Cyprus)'
    else if FRegion.IsEmpty then
      result := 'Greek'
    else
      result := 'Greek (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'tr') then
  begin
    if SameText(FRegion, 'TR') or FRegion.IsEmpty then
      result := 'Turkish'
    else if SameText(FRegion, 'CY') then
      result := 'Turkish (Cyprus)'
    else
      result := 'Turkish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ar') then
  begin
    if SameText(FRegion, 'SA') then
      result := 'Arabic (Saudi Arabia)'
    else if SameText(FRegion, 'EG') then
      result := 'Arabic (Egypt)'
    else if SameText(FRegion, 'AE') then
      result := 'Arabic (United Arab Emirates)'
    else if SameText(FRegion, 'DZ') then
      result := 'Arabic (Algeria)'
    else if SameText(FRegion, 'IQ') then
      result := 'Arabic (Iraq)'
    else if SameText(FRegion, 'JO') then
      result := 'Arabic (Jordan)'
    else if SameText(FRegion, 'KW') then
      result := 'Arabic (Kuwait)'
    else if SameText(FRegion, 'LB') then
      result := 'Arabic (Lebanon)'
    else if SameText(FRegion, 'LY') then
      result := 'Arabic (Libya)'
    else if SameText(FRegion, 'MA') then
      result := 'Arabic (Morocco)'
    else if SameText(FRegion, 'OM') then
      result := 'Arabic (Oman)'
    else if SameText(FRegion, 'QA') then
      result := 'Arabic (Qatar)'
    else if SameText(FRegion, 'SY') then
      result := 'Arabic (Syria)'
    else if SameText(FRegion, 'TN') then
      result := 'Arabic (Tunisia)'
    else if SameText(FRegion, 'YE') then
      result := 'Arabic (Yemen)'
    else if FRegion.IsEmpty then
      result := 'Arabic'
    else
      result := 'Arabic (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'he') or SameText(FLanguage, 'iw') then // 'iw' is old code for Hebrew
  begin
    if SameText(FRegion, 'IL') or FRegion.IsEmpty then
      result := 'Hebrew'
    else
      result := 'Hebrew (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'hi') then
  begin
    if SameText(FRegion, 'IN') or FRegion.IsEmpty then
      result := 'Hindi'
    else
      result := 'Hindi (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'th') then
  begin
    if SameText(FRegion, 'TH') or FRegion.IsEmpty then
      result := 'Thai'
    else
      result := 'Thai (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'id') or SameText(FLanguage, 'in') then // 'in' is old code
  begin
    if SameText(FRegion, 'ID') or FRegion.IsEmpty then
      result := 'Indonesian'
    else
      result := 'Indonesian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ms') then
  begin
    if SameText(FRegion, 'MY') then
      result := 'Malay (Malaysia)'
    else if SameText(FRegion, 'BN') then
      result := 'Malay (Brunei)'
    else if SameText(FRegion, 'SG') then
      result := 'Malay (Singapore)'
    else if FRegion.IsEmpty then
      result := 'Malay'
    else
      result := 'Malay (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'vi') then
  begin
    if SameText(FRegion, 'VN') or FRegion.IsEmpty then
      result := 'Vietnamese'
    else
      result := 'Vietnamese (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'uk') then
  begin
    if SameText(FRegion, 'UA') or FRegion.IsEmpty then
      result := 'Ukrainian'
    else
      result := 'Ukrainian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ro') then
  begin
    if SameText(FRegion, 'RO') then
      result := 'Romanian (Romania)'
    else if SameText(FRegion, 'MD') then
      result := 'Romanian (Moldova)'
    else if FRegion.IsEmpty then
      result := 'Romanian'
    else
      result := 'Romanian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'bg') then
  begin
    if SameText(FRegion, 'BG') or FRegion.IsEmpty then
      result := 'Bulgarian'
    else
      result := 'Bulgarian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'hr') then
  begin
    if SameText(FRegion, 'HR') or FRegion.IsEmpty then
      result := 'Croatian'
    else
      result := 'Croatian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'sr') then
  begin
    if SameText(FRegion, 'RS') then
      result := 'Serbian (Serbia)'
    else if SameText(FRegion, 'BA') then
      result := 'Serbian (Bosnia and Herzegovina)'
    else if SameText(FRegion, 'ME') then
      result := 'Serbian (Montenegro)'
    else if FRegion.IsEmpty then
      result := 'Serbian'
    else
      result := 'Serbian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'sl') then
  begin
    if SameText(FRegion, 'SI') or FRegion.IsEmpty then
      result := 'Slovenian'
    else
      result := 'Slovenian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'et') then
  begin
    if SameText(FRegion, 'EE') or FRegion.IsEmpty then
      result := 'Estonian'
    else
      result := 'Estonian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'lv') then
  begin
    if SameText(FRegion, 'LV') or FRegion.IsEmpty then
      result := 'Latvian'
    else
      result := 'Latvian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'lt') then
  begin
    if SameText(FRegion, 'LT') or FRegion.IsEmpty then
      result := 'Lithuanian'
    else
      result := 'Lithuanian (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'is') then
  begin
    if SameText(FRegion, 'IS') or FRegion.IsEmpty then
      result := 'Icelandic'
    else
      result := 'Icelandic (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'ga') then
  begin
    if SameText(FRegion, 'IE') or FRegion.IsEmpty then
      result := 'Irish'
    else
      result := 'Irish (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'cy') then
  begin
    if SameText(FRegion, 'GB') or FRegion.IsEmpty then
      result := 'Welsh' // GB as region, because Welsh is part of GB
    else
      result := 'Welsh (' + FRegion + ')';
  end
  else if SameText(FLanguage, 'gd') then
  begin
    if SameText(FRegion, 'GB') or FRegion.IsEmpty then
      result := 'Scottish Gaelic' // GB as region
    else
      result := 'Scottish Gaelic (' + FRegion + ')';
  end;
  if result.IsEmpty and not FLanguage.IsEmpty then
  begin
    if not FRegion.IsEmpty then
      result := FLanguage + ' (' + FRegion + ')'
    else
      result := FLanguage;
  end;
end;

{$ENDREGION}

{$REGION 'Environment Variables'}

class function TLX4DSystemInfo.GetEnvironmentVariables: TStringList;
var
  p: PMarshaledAString;
begin
  result := TStringList.Create;
  p := environ;
  while (p^ <> nil) do
  begin
    result.Add(string(p^));
    Inc(p);
  end;
end;
{$ENDREGION}

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

class function TLX4DSystemInfo.GetMemory: TMemory;

  function ExtractInBytes(SizeWithUnit: string): Int64;
  begin
    if SizeWithUnit.EndsWith(' kB', true) then
      result := StrToInt64Def(SizeWithUnit.Substring(0, SizeWithUnit.Length - 3), 0) * 1024
    else
      // unexpected unit !
      result := 0;
  end;
var
  Line, Key, Value: string;
  Details: TStringList;
  strs: TArray<string>;
begin
  result.Details := TStringList.Create;
  if FileExists(MEMINFO) then
  begin
    Details := ReadProcFile(MEMINFO);
    for Line in Details do
    begin
      strs := Line.Split([':']);
      if length(Strs) >= 2 then
      begin
        Key := trim(strs[0]);
        Value := trim(strs[1]);
        result.Details.Add(Key + '=' + Value);
        if SameText(Key, 'MemTotal') then
          result.Total := ExtractInBytes(Value)
        else if SameText(Key, 'MemFree') then
          result.Free := ExtractInBytes(Value)
        else if SameText(Key, 'MemAvailable') then
          result.Available := ExtractInBytes(Value)
        else if SameText(Key, 'Buffers') then
          result.Buffers := ExtractInBytes(Value)
        else if SameText(Key, 'Cached') then
          result.Cached := ExtractInBytes(Value);
      end;
    end;
  end;
end;

class function TLX4DSystemInfo.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create(TJSONPair.Create('UserID', FUID)).
    AddPair('UserName', FUserName).
    AddPair('Distribution', FDistribution.ToJSON).
    AddPair('Language', FLanguage).
    AddPair('Region', FRegion).
    AddPair('LanguageAndRegion', GetLanguagePretty).
    AddPair('Encoding', FEncodingStr).
    AddPair('Kernel', FKernel.ToJSON).
    AddPair('CPU', FCPU.ToJSON);
end;

{ TLX4DSystemInfo.TKernel }

function TLX4DSystemInfo.TKernel.PrettyName: string;
begin
  result := Format('%s %s (%s), Net: %s', [SystemName, Release, Machine, NodeName]);
end;

function TLX4DSystemInfo.TKernel.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create(TJSONPair.Create('SystemName', SystemName)).
    AddPair('NodeName', NodeName).
    AddPair('Release', Release).
    AddPair('Machine', Machine);
end;

{ TLX4DSystemInfo.TCPU }

function TLX4DSystemInfo.TCPU.PrettyName: string;
begin
  result := Format('%s (%s) (%d Cores, %4.2f MHz), CacheSize: %s, FPU: %s',
    [ModelName, VendorID, LogicalCores, CPUMHz, CacheSize, BoolToStr(FPU, true)]);
end;

function TLX4DSystemInfo.TCPU.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create(TJSONPair.Create('ModelName', ModelName)).
    AddPair('VendorID', VendorID).
    AddPair('CPUMHz', CPUMHz).
    AddPair('LogicalCores', LogicalCores).
    AddPair('FPU', FPU).
    AddPair('CacheSize', CacheSize).
    AddPair('Details', Details.CommaText);
end;

{ TLX4DSystemInfo.TDistribution }

function TLX4DSystemInfo.TDistribution.KindStr: string;
begin
  result := TRttiEnumerationType.GetName<TDistributionKind>(FDistribution.Kind);
end;

function TLX4DSystemInfo.TDistribution.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create(TJSONPair.Create('Kind', KindStr)).
    AddPair('ID', ID).
    AddPair('BaseID', BaseID).
    AddPair('CodeName', CodeName).
    AddPair('PrettyName', PrettyName).
    AddPair('Release', Release).
    AddPair('Details', Details.CommaText).
    AddPair('SourceFile', SourceFile);
end;

{ TLX4DSystemInfo.TMemory }

class operator TLX4DSystemInfo.TMemory.Finalize(var Dest: TMemory);
begin
  FreeAndNil(Dest.Details);
end;

function TLX4DSystemInfo.TMemory.PrettyFormat: string;

  function GetBytesInBestUnit(Val: Int64): string;
  const
    ByteinKiloBytes: Int64 = 1024;
    ByteinMegaBytes: Int64 = 1024 * 1024;
    ByteinGigaBytes: Int64 = 1024 * 1024 * 1024;
  begin
    if Val < 2 * ByteinKiloBytes then
      result := Format('%d B', [Val])
    else if Val < 2 * ByteinMegaBytes then
      result := Format('%5.1f KB', [Val / ByteinKiloBytes])
    else if Val < 2 * ByteinGigaBytes then
      result := Format('%5.1f MB', [Val / ByteinMegaBytes])
    else
      result := Format('%5.1f GB', [Val / ByteinGigaBytes]);
  end;

begin
  result := Format('Total: %s, Free: %s, Available: %s, Buffers: %s, Cached: %s',
    [GetBytesInBestUnit(Total), GetBytesInBestUnit(Free), GetBytesInBestUnit(Available),
     GetBytesInBestUnit(Buffers), GetBytesInBestUnit(Cached)]);
end;

end.
