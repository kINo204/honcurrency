# Clean build artifacts
Write-Host "Cleaning build artifacts..."

# Remove the build directory
if (Test-Path build) {
    Remove-Item -Recurse -Force build
    Write-Host "Removed build/ directory."
} else {
    Write-Host "No build/ directory found."
}