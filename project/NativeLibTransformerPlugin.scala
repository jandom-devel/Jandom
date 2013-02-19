import sbt._
import sbt.Keys._
import xml.{ Node, _ }
import xml.transform.RewriteRule
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin
import com.typesafe.sbteclipse.core.Validation

/** 
 * This class is a [[EclipseTransformerFactory[RewriteRule]]] which adds, to the classpath entry
 *  relative to `jar in the `.classpath` file, a native library directory
 * attribute. The native library directory in the parent directory of `jar`.
 * 
 * @param jar the jar of the Java/Scala library which needs native libraries.
 * @author Gianluca Amato
 */
class NativeLibTransformerFactory(val jar: String) extends EclipsePlugin.EclipseTransformerFactory[RewriteRule] {
  import scalaz.Scalaz._

  object NativeLibRewriteRule extends RewriteRule {
    private val CpEntry = "classpathentry"
    private val NativeLibEntry = "org.eclipse.jdt.launching.CLASSPATH_ATTR_LIBRARY_PATH_ENTRY"

    override def transform(node: Node): Seq[Node] = node match {
      case Elem(pf, CpEntry, attrs, scope) if isNativeLibrary(attrs) =>
        val path = new java.io.File(jar)
        // I am assuming sbt-eclipse does not already generate
        // attributes entries.
        Elem(pf, CpEntry, attrs, scope,
          <attributes>
            <attribute name={ NativeLibEntry } value={ path.getParent }/>
          </attributes>)
      case other =>
        other
    }
    private def isNativeLibrary(metaData: MetaData) =
      metaData("kind") == Text("lib") && metaData("path") == Text(jar)
  }

  def createTransformer(ref: ProjectRef, state: State) = NativeLibRewriteRule.success
}

/**
 * This is the companion object for the class [[NativeLibTransformerFactory]].
 */
object NativeLibTransformerFactory {
  def apply(jar: String) = new NativeLibTransformerFactory(jar)
}
    	  
/**
 * This sbt plugin works in combination with sbt-eclipse, and it allows to add
 * native library informations to the classpath of an Eclipse project.
 * If `/path/lib.jar` is a library whose native libraries are in `/path`, just
 * add `classpathTransformerFactories += NativeLibTransformerFactory("/path/lib.jar")`
 * to your project.
 * 
 * @author Gianluca Amato
 */
object NativeLibTransformerPlugin extends Plugin {
  override def settings = Seq( EclipseKeys.classpathTransformerFactories := Seq() )
}
