package de.tuberlin.uebb.sl2.modules

import java.io.File

trait Configs {
  case class Config(
    /**
     * where to look for source files
     */
    val sourcepath: File,
    /**
     * which source files at source path to compile
     */
    val sources: List[String],
    /**
     * where to look for compiled versions of imported modules
     * (this should usually be the destination directory.
     * if its not, manual changes to the requirejs-config might
     * be needed.)
     */
    val classpath: File,
    /**
     * the simple name of the main file compiled
     */
    val mainName: String,
    /**
     * the parent directory of the main file compiled
     */
    val mainParent: File,
    /**
     * where to put the compiled files from source.
     */
    val destination: File
 )
}
