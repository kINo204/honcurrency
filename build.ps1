# build.ps1 - compile all examples in examples/ directory

# create build directory if missing
New-Item -ItemType Directory -Force -Path build | Out-Null

Get-ChildItem examples\*.hs | ForEach-Object {
    $file = $_.FullName
    $base = $_.BaseName
    Write-Host "Compiling $file..."
    ghc -outputdir build -o "build/$base.exe" $file
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Failed to compile $file" -ForegroundColor Red
        exit $LASTEXITCODE
    }
}
Write-Host "All examples built into build/ directory."