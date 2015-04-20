import java.io.File
import scala.xml._
import scala.xml.transform._
import sbt.Path

object ClasspathentryRewriteRule extends RewriteRule {
  override def transform(parent: Node): Seq[Node] = {
    parent match {
      case c @ <classpathentry/> if (c \ "@path").toString.endsWith("resources") =>
	val newPath = Path((c \ "@path").toString) / "java"
	c.asInstanceOf[Elem] % Attribute("", "path", newPath.getPath, Null)
      case other => other
    }
  }
}

