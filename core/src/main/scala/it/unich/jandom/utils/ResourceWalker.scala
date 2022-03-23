/**
  * Copyright 2018 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.jandom.utils

import java.net.URI
import java.nio.file._

import scala.jdk.CollectionConverters._

/**
  * An utility object containing methods for walking directory resources whether they are
  * packed in a JAR file or not.
  */
object ResourceWalker {

  def list(root: String): Seq[Path] = {
    val uri = getClass.getResource(root).toURI
    val rootPath = if (uri.getScheme == "jar")
      getFileSystem(uri).getPath(root)
    else
      Paths.get(uri)
    Files.list(rootPath).iterator().asScala.toSeq
  }

  private def getFileSystem(uri: URI): FileSystem = {
    try
      FileSystems.getFileSystem(uri)
    catch {
      case e: FileSystemNotFoundException =>
        FileSystems.newFileSystem(uri, java.util.Collections.emptyMap[String, String])
    }
  }
}
