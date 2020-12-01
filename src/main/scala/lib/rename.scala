/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package lib

import spinal.core._
import spinal.lib._

/*
 * If you want to rename the IO signals your module,
 * you should extends your class with this trait.
 * At the end of your class, call `addPrePopTask(renameIO)`.
 */
trait RenameIO {

  /*
   * Only subclasses of Component can extend this RenameIO trait
   * @this is the calling class, therefore able to use SpinalHDL methods
   * i.e., getAllIo, getName.
   */
  this : Component =>

  def renameIO(): Unit = {

    this.noIoPrefix()

    for (port <- this.getAllIo) {
      val newName = port.getName()
        //.replaceAll("(a?[wrb])_(payload_)?", "$1")

        // For Axi Stream, raw interface
        .replaceAll("_payload$", "_tdata")
        .replaceAll("_ready$", "_tready")
        .replaceAll("_valid$", "_tvalid")

        // For Axi Stream, Fragment Interface
        .replaceAll("_last$", "_tlast")
        .replaceAll("_payload_t", "_t")
        .replaceAll("_payload_fragment_t", "_t")
        .replaceAll("_payload_", "_")

        // Special rules
        //.replaceAll("_desc_tready$", "_desc_ready")
        //.replaceAll("_desc_tvalid$", "_desc_valid")

        // fragments
        //.replaceAll("_fragment$", "_tdata")

      port.setName(newName)
    }
    //println(f"Renamed the IO signal of ${this.getClass().getSimpleName}")
  }
}
