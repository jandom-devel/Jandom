/**
  * Copyright 2015, 2016, 2017 Gianluca Amato <gamato@unich.it>
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


package it.unich.jandom.benchmark

import java.io.File
import java.util.jar.JarFile

import it.unich.jandom.parsers.FastParser
import it.unich.jandom.targets.lts.LTS

import scala.collection.JavaConversions._

/**
  * This trait loads and parser all models in /fast/ resource directory.
  *
  * @author Gianluca Amato <gamato@unich.it>
  */

trait FASTLoader {
  private val path = "fast/"
  private val jarFile = new File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath)

  // determine resource names when the program is packaged as a jar and when not
  private val uris = if (jarFile.isFile) { // Run with JAR file
    val jar = new JarFile(jarFile)
    val entries = jar.entries //gives ALL entries in jar
    val result = (for (element <- entries; name = element.getName
                       if (name startsWith path) && (name.length > path.length)) yield "/"+name).toList
    jar.close()
    result
  }
  else { // Run with IDE
    val resources = getClass.getResource(path).toURI
    val dir = new File(resources)
    dir.list().toList
  }

  /**
    * A sequence of Alice models.
    */
  val ltss: Seq[LTS] = for (uri <- uris) yield {
    val stream = getClass.getResourceAsStream(uri)
    val content = scala.io.Source.fromInputStream(stream).getLines.mkString("\n")
    val result = FastParser().parse(content).get
    result.copy(name = s"${result.name} -- $uri")
  }
}
