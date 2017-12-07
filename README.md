# Plague

ASCII roguelike written in F#. Nothing interesting to see here yet.

![screenshot](http://laastine.kapsi.fi/kuvat/plague.png?cache=1)

## TODO

- [x] collision detection
- [x] basic movement
- [x] Houses/walls
- [x] File logger
- [ ] Diagonal movement
- [ ] Dynamic world
- [ ] Critters
- [x] Path finding
- [ ] Line of sight

## Requirements
- Dotnet Core 2.0

### Compile & run app

- Compile sources
`dotnet build`

- Run application
`dotnet run`

- Publish executable in Mac OS
`dotnet publish --framework netcoreapp2.0 --self-contained --runtime osx.10.11-x64 && ./bin/Debug/netcoreapp2.0/osx.10.11-x64/plague`

## Controls

`WASD`
