# build.ps1 - compile examples or run tests

param(
    [switch]$Build,
    [switch]$Test
)

# default behavior if no switches provided: only build examples
if (-not $Build -and -not $Test) {
    $Build = $true
}

# ensure build directory (and subdirs) exist
New-Item -ItemType Directory -Force -Path build | Out-Null
New-Item -ItemType Directory -Force -Path build\tests | Out-Null

if ($Build) {
    # compile all examples
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
    Write-Host "All examples built into build/ directory." -ForegroundColor Green
}

if ($Test) {
    # compile and run tests
    Get-ChildItem tests\*.hs | ForEach-Object {
        $file = $_.FullName
        $base = $_.BaseName
        Write-Host "Compiling test $file..."
        ghc -outputdir build\tests -o "build/tests/$base.exe" $file
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Failed to compile test $file" -ForegroundColor Red
            exit $LASTEXITCODE
        }
        Write-Host "Running test $base..."
        & "build/tests/$base.exe True"
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Test $base failed" -ForegroundColor Red
            exit $LASTEXITCODE
        }
    }
    Write-Host "All tests executed successfully." -ForegroundColor Green
}
