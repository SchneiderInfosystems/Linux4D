# Linux4D 

Linux4D is an open-source Delphi library designed to simplify system-oriented programming on Linux. It allows you to easily retrieve crucial information about the host system.

## Key Features (Open-Source Core)

The open-source version of Linux4D helps you:
- Identify the installed system language and region.
- Detect the system's active encoding.
- Determine the specific Linux distribution and its version.
- Retrieve information about the Linux kernel and CPU hardware.

## Getting Started

1. Clone the repository: `git clone https://github.com/SchneiderInfosystems/Linux4D.git`
2. Add the source path to your Delphi project's search path.
3. Check the following example code:

```delphi
uses LX4D.SystemInfo; 

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Distribution: ' + TLX4DSystemInfo.Distribution.PrettyName);
  ShowMessage('Language: ' + TLX4DSystemInfo.LanguagePretty);
  ShowMessage('Kernel: ' + TLX4DSystemInfo.Kernel.PrettyName);
end;
```

## Wiki
For a quick start guide, detailed functional overview, and more examples, please visit our 
[Wiki](https://github.com/SchneiderInfosystems/Linux4D/wiki/Gettings-Started-with-Linux4D).

## License (Open-Source Core)
The open-source Linux4D library is licensed under the Apache-2.0 license. Please see the LICENSE file for more details.

# Extended Linux4D (Commercial Library)

For users requiring advanced system interaction capabilities, we offer Extended Linux4D. This commercial library builds upon the open-source core and provides additional features, including:
* Querying installed system packages.
* Programmatic installation of specific packages.
* A secure mechanism for running parts of your application with root privileges.
* Executing Bash commands from your application, with options for synchronous (blocking) or asynchronous execution.

Interested in Extended Linux4D? 

For inquiries, tailored solutions, and quotes, please contact us at delphi@schneider-infosys.ch.

May-2025, Schneider Infosystems AG, Switzerland
