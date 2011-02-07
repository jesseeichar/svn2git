import sbt._
import com.github.retronym.OneJarProject

class HelloWorldProject(info: ProjectInfo) extends DefaultProject(info) with OneJarProject
{
  override def mainClass = Some("Svn2Git")
}