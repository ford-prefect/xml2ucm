# xml2ucm

This tool helps convert Android mixer_paths.xml audio configuration to the ALSA
UCM format. There is a sample configuration file in ```examples/``` for the
Nexus 4 (mako).

In relatively simple setups, the UCM that is produced should just work, but
some manual tweaking might be required based on the audio subsystem
(PulseAudio, CrAS, ...) and the hardware.

## Building

To build the code, you'll need ```stack```, which your [distribution probably
provides][stack]. Check out the code and then run:

[stack]: http://docs.haskellstack.org/en/stable/GUIDE.html#downloading-and-installation

```sh
$ stack setup # if you haven't done this once already
$ stack build
```

## Running

To build with the example, you could do something like:

```sh
$ stack exec -- xml2ucm -m examples/mako-mixer_paths.xml -c examples/mako-config.xml -o examples
```

The generated files will be in ```examples/apq8064-tabla-snd-card/```.
