import scalaz.Scalaz._
import scala.xml.transform.RewriteRule

import sbt.{ProjectRef, State}

import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseTransformerFactory
import com.typesafe.sbteclipse.core.Validation

object ClasspathentryTransformer extends EclipseTransformerFactory[RewriteRule] {
  override def createTransformer(ref: ProjectRef, state: State): Validation[RewriteRule] = {
    ClasspathentryRewriteRule.success
  }
}

