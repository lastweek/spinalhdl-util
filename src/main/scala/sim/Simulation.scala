/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

object SimContext {
  object SimulationSpinalConfig extends SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(250 MHz)
  )

  val SuperNICSimConfig = SimConfig
    .withConfig(SimulationSpinalConfig)
    //.addSimulatorFlag("")
    .workspacePath("generated_sim/test/")
    .withWave
}
