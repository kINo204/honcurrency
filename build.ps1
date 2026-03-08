param($example)

if (-not $example) {
    Write-Host "Usage: .\build.ps1 <example>"
    exit 1
}

$hsFile = "examples/$example.hs"

if (Test-Path $hsFile) {
    Write-Host "Compiling $hsFile..."
    # Create build directory if it doesn't exist
    New-Item -ItemType Directory -Force -Path build | Out-Null
    # Compile with output directory set to build
    ghc -outputdir build $hsFile
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Compilation successful. Output in build/ directory."
    } else {
        Write-Host "Compilation failed."
    }
} else {
    Write-Host "Example '$example' not found in examples/ directory."
}