package de.tuberlin.uebb.sl2.modules

import java.io.File
import java.net.URL
import scalax.file.FileSystem
import scalax.file.Path
import scala.io.Source

trait AbstractFile {
  /**
   * Used to abstract from the differences between files from a .jar file ({@link BottledFile})
   * and files from the file system ({@link PathedFile}).
   */
  abstract class AbstractFile {
    def filename():String
    def parent():File
    def path():String
    def canRead():Boolean
    def lastModified():Long
    def contents():String
  }
  
  class PathedFile(path: Path) extends AbstractFile {
    def filename() = {
      path.name
    }
    
    def parent() = {
      if(path.parent != null)
    	  new File(path.parent.toString)
      else
    	  new File(".")
    }
    
    def path() = {
      path.path
    }
    
    def canRead() = {
      path.canRead
    }
    
    def lastModified() = {
      path.lastModified
    }
    
    def contents() = {
      path.lines(includeTerminator = true).mkString("")
    }
  }
  
  class ErrorFile(url: URL) extends AbstractFile {
    def filename = { "Error" }
    def parent = { new File(".") }
    def path = { throw new UnsupportedOperationException() }
    def canRead() = { false }
    def lastModified() = { 0 }
    def contents() = { throw new UnsupportedOperationException() } 
  }
  
  /**
   * A file inside a .jar file 
   */
  class BottledFile(url: URL) extends AbstractFile {
    
    def filename() = {
      val f = url.getFile
      f.substring(f.lastIndexOf("!")+1)
    }
    
    def parent() = {
      new File(".")
    }
    
    def path() = {
      url.toString
    }
    
    def jarFile() = {
      val f = url.getFile
      f.substring(5, f.lastIndexOf("!"))
    }
    
    def canRead() = {
      /*
       * A bottled file can always be read. Otherwise, its location wouldn't have been found.
       */
      true
    }
    
    def lastModified() = {
      /*
       * The modification date for a bottled does not matter, because all files
       * (source, js and signature) in a JAR file have the same date and cannot
       * be modified.
       */
      0
    }
    
    def contents() = {
      Source.fromURL(url).getLines.mkString("\n")
    }
  }
  
  def createFile(url: URL, path: String):AbstractFile = {
    createFile(new URL(url, path))
  }
  
  def createFile(file: File, path: String):AbstractFile = {
    if(file != null)
    	createFile(new URL(file.toURI.toURL, path))
    else
        new ErrorFile(new File(path).toURI.toURL)
  }
  
	def createFile(url: URL):AbstractFile = {
	  if(url.toString.startsWith("jar:")) {
		  new BottledFile(url)
	  } else {
	      val option = Path(url.toURI)
	      if(option.isEmpty)
	    	  new ErrorFile(url)
	      else
	    	  new PathedFile(option.get)
	  }
	}
}