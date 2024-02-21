# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

param(
	[parameter(Mandatory=$true)] [String] $InstallDirectory,
	[parameter(Mandatory=$true,ValueFromPipeline=$true)] [String] $Buck2Output
)

begin
{
	$ErrorActionPreference = "Stop"

	function Copy-Directory
	{
		param(
			[parameter(Mandatory=$true)] [String] $SourcePath,
			[parameter(Mandatory=$true)] [String] $TargetPath
		)
		robocopy /MIR "$SourcePath" "$TargetPath" | Out-Null
	}

	function Copy-File
	{
		param(
			[parameter(Mandatory=$true)] [String] $SourcePath,
			[parameter(Mandatory=$true)] [String] $TargetPath
		)
		Copy-Item $SourcePath -Destination $TargetPath
	}
}
process
{
	$OutputSplit = $input.Split(" ")
	$TargetPath = $OutputSplit[0].Trim()
	$ExecutablePath = $OutputSplit[1].Trim()
	$ResourcesPath = $ExecutablePath + ".resources.json"

	$CopySource = $ExecutablePath
	$CopyTarget = Join-Path -Path $InstallDirectory -ChildPath (Split-Path -Leaf $ExecutablePath)
	Copy-File $CopySource $CopyTarget

	try
	{
		$ResourcesContent = Get-Content $ResourcesPath -ErrorAction Stop | ConvertFrom-Json
		$HasResources = $true
	}
	catch [System.Management.Automation.ItemNotFoundException]
	{
		$ResourcesContent = @()
		$HasResources = $false
	}
	if($HasResources)
	{
		foreach($ResourceProperty in $ResourcesContent.PSObject.Properties)
		{
			$ResourceTargetPath = $ResourceProperty.Name
			$ResourceSourcePath = $ResourceProperty.Value
			$CopySource = Join-Path -Path (Split-Path -Parent $ExecutablePath) -ChildPath $ResourceSourcePath
			$CopyTarget = Join-Path -Path $InstallDirectory -ChildPath $ResourceTargetPath

			$GetItemResult = Get-Item $CopySource
			if($GetItemResult.PSIsContainer)
			{
				Copy-Directory $CopySource $CopyTarget
			}
			else
			{
				Copy-File $CopySource $CopyTarget
			}
		}
	}
}
