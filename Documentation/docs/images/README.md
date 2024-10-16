# Notes on figures
Command for creating small Trixi:
```bash
convert TrixiInside.png -resize 64x64 TrixiInside_small.png
```

Command for turning background transparent:
```bash
convert TrixiInside_small.png -fill 'transparent' -fuzz 5% -draw 'color 0,0 floodfill' TrixiInside_small_transparent.png
```

Command for turning a single color completely transparent:
```bash
convert TrixiInside.png -fuzz 5% -transparent '#52586E' TrixiInside_transparent.png
```
