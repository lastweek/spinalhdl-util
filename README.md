# SpinalHDL

A set of useful SpinalHDL utilites we've used across our academic projects.

## Simulation

Simulation generated files are put under `generated_sim/` project.
All simulation will generate waveforms.
Both configurations are manually passed to `SimConfig` on every test module.

### Waveform

You can use __gtkwave__ to view the generated waveform. For example:
```bash
gtkwave generated_sim/supernic/SuperNICTop/test.vcd
```


## Components

## AXI Stream
We used the Stream+Fragment combo to realize this.
