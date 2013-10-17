package de.tuberlin.uebb.sl2.modules

import de.tuberlin.uebb.sl2.modules._
import java.io.File
import java.io.IOException
import java.net.URL
import scalax.file.Path

/**
 * Sort the nodes in a given set of edges topologically.
 */
trait ModuleLinearization
	extends Object
	with AbstractFile
	with Configs
	with Errors
	with ModuleResolver
	with Syntax {
  
  sealed case class Module(
    val name: String = "",
    val source: AbstractFile = null,
    val signature: AbstractFile = null,
    val js: AbstractFile = null,
    val compile: Boolean = false) {
    
    override def equals(obj: Any) = {
      (obj != null &&
        obj.isInstanceOf[Module] &&
        obj.asInstanceOf[Module].name == this.name)
    }

    override def hashCode() = { name.hashCode }

    override def toString() = {
      "(Module " + quote(name) + " (compile=" + compile + "))\n"
    }
  }

  /**
   * creates a module compilation unit object from a file name
   * (can either be from /std)
   */
  def moduleFromName(name: String, config: Config): Module = {
    if (name.startsWith(standardLibName+"/")) {
      // load std/ library from resources directory
      val nameEnd = name.replace(standardLibName+"/", "")
      val stdSource = getLibResource(nameEnd + ".sl")
      if (stdSource == null)
        throw new IOException("Could not find source of standard library: "
          + quote(standardLibUrl + name))
      Module(nameEnd,
        createFile(stdSource),
        createFile(new URL(stdSource.toString + ".signature")),
        createFile(new URL(stdSource.toString + ".js")))
    } else {
      // load ordinary files relative to source- and classpath
      Module(name,
        createFile(new URL(config.sourcepath.toURI.toURL, name + ".sl")),
        createFile(new URL(config.classpath.toURI.toURL, name + ".sl.signature")),
        createFile(new URL(config.classpath.toURI.toURL, name + ".sl.js")))
    }
  }

  /**
   * Sorts the modules in the map from modules to sets of their respective
   * required modules topologically and returns the sorted sequence of modules.
   */
  def topoSort(predecessors: scala.collection.Map[Module, Set[Module]]): Either[Error, Iterable[Module]] = {
    topoSort(predecessors, Seq())
  }

  /**
   * Sorts the modules in the map from modules to sets of required modules topologically.
   *
   * Recursively sort topologically: add the modules to the set of done modules
   * that have no predecessors, remove them from the map of predecessors and
   * recurse until no modules are left. Return an error, if modules are left.
   */
  def topoSort(predecessors: scala.collection.Map[Module, Set[Module]],
    done: Iterable[Module]): Either[Error, Iterable[Module]] = {
    val (hasNoPredecessors, hasPredecessors) = predecessors.partition { _._2.isEmpty }
    if (hasNoPredecessors.isEmpty) {
      if (hasPredecessors.isEmpty) {
        Right(done)
      } else {
        Left(CircularDependencyError("Circular dependency between modules " +
          (for (key <- hasPredecessors.keys) yield key.source.path).mkString(", ")))
      }
    } else {
      val found = hasNoPredecessors.map { _._1 }
      topoSort(hasPredecessors.mapValues { _ -- found }, done ++ found)
    }
  }
}
