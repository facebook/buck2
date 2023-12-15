param(
  [parameter(Mandatory=$true)] [String] $FilePath
)

New-Item -ItemType HardLink -Force -Path compile_commands.json -Target $FilePath.Trim() | Out-Null
