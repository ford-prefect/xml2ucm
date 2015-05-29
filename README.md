# xml2ucm

This tool helps convert Android mixer_paths.xml audio configuration to the ALSA
UCM format. There is a sample configuration file in ```examples/``` for the
Nexus 4 (mako).

In relatively simple setups, the UCM that is produced should just work, but
some manual tweaking might be required based on the audio subsystem
(PulseAudio, CrAS, ...) and the hardware.

## Building

To build the code, you'll need ```cabal-install```, which your distribution
probably provides. Check out the code and then run:

```sh
$ cabal sandbox init
$ cabal install --only-dependencies xml2ucm.cabal
$ cabal build
```

(If you find the distribution version of ```cabal-install``` to be too old, you
can install a newer version for your user locally using
```cabal install cabal-install```. You'll then need to do something like
```PATH=~/.cabal/bin:$PATH``` to use this version).

## Running

To build with the example, you could do something like:

```sh
$ ./dist/build/xml2ucm/xml2ucm -m examples/mako-mixer_paths.xml -c examples/mako-config.xml -o examples
```

The generated files will be in ```examples/apq8064-tabla-snd-card/```.
