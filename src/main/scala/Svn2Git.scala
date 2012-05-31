import collection.JavaConverters._
import concurrent.SyncVar
import java.io.File
import sys.process._

object Svn2Git {

  def main(args: Array[String]) {
    val params = new Params(args)
    val action = if (params.isUpdate) {
      new Update(params)
    } else if (params.isClone) {
      new Clone(params)
    } else {
      println("Either -clone or -update is required")
      sys.exit(1)
    }
    
    action.apply()
    action.gc()
  }
}

class Params(args: Seq[String]) {
  val (flags, params) = args.partition { _ startsWith "-" }
  def flag(flagname: String) = {
    val prefix = "-" + flagname + "="
    flags.find(_.startsWith(prefix)).map(_.split("=", 2)(1))
  }
  val git = flag("git").getOrElse("git")
  val url = flag("url")

  val isUpdate = flags.contains("-update")
  val isClone = flags.contains("-clone")

  val repo = new File(params.headOption getOrElse ".")
}
abstract class GitAction(params: Params) {
  import params.git
  
  def apply():Unit
  
  def gc() = exec(git,"gc")(params.repo)

  def exec(cmd: String*)(implicit dir: File) = {
    println(cmd mkString " ")
    val lines = Process(cmd, params.repo).lines_!
    def echo(s:String) = println(">  "+s)
    lines.foreach(println)
    lines
  }

  def lookUpSvnURLFromConfig(repo: File) = {
    val config = new File(new File(repo, ".git"), "config")
    val svnSection = io.Source.fromFile(config).getLines().dropWhile(_.trim != "[svn-remote \"svn\"]")
    val Url = """\s*url\s+=\s*(.+)""".r
    svnSection.collect { case Url(url) => url.trim }.next
  }

  def gitBranches(implicit repo: File) = exec(git, "branch", "-l", "--no-color") map { _.trim.dropWhile(_ == '*').trim }
  def gitTags(implicit repo: File) = exec(git, "tag", "-l") map { _.trim }

  def syncTagsAndBranches(url: String)(implicit repo: File) = {
    clean()
    exec(git, "checkout", "master")
    val svnBranches = exec("svn", "ls", url + "/branches") map { _.trim.dropRight(1) }
    val svnTags = exec("svn", "ls", url + "/tags") map { _.trim.dropRight(1) }

    val gitBranches = this.gitBranches
    val gitTags = this.gitTags

    val notMaster = (branch: String) => branch != "master" && branch != "trunk"
    // add missing branches
    for {
      branch <- svnBranches
      if !(gitBranches contains branch)
      if notMaster(branch)
      remote = "remotes/" + branch
    } {
      exec(git, "branch", branch, remote)
    }

    // delete removed branches
    for {
      branch <- gitBranches
      if notMaster(branch) && !(svnBranches contains branch)
    } {
      exec(git, "branch", "-D", branch)
    }

    // delete git tags so that if there was an update
    // in SVN we will get the updated version of the tag
    // when we rebuild the tags
    for { tag <- gitTags } {
      exec(git, "tag", "-d", tag)
    }

    // add missing branches
    for {
      tag <- svnTags
      remote = "remotes/tags/" + tag
    } {

      val name = if (svnBranches contains tag) tag + "_tag" else tag

      exec(git, "tag", "-m", "SVN tag " + tag, name, remote)
    }
  }

  def clean()(implicit repo: File) = {
    exec(git, "reset", "--hard")
    exec(git, "clean", "-dfqx", "master")
  }
}

class Update(params: Params) extends GitAction(params) {
  import params.git
  def apply() {
    implicit val repo = params.repo
    val url = lookUpSvnURLFromConfig(repo)
    val Branch = """.*\(refs/remotes/(.+?)\).*""".r
    exec(git, "svn", "fetch")

    syncTagsAndBranches(url)

    gitBranches filter { _ != "master" } foreach { b =>
      clean()
      exec(git, "checkout", b)
      exec(git, "rebase", "remotes/" + b)
    }

    clean()
    exec(git, "checkout", "master")
    exec(git, "rebase", "remotes/trunk")
  }
}

class Clone(params: Params) extends GitAction(params) {
  import params.git
  def apply() {
    val url = params.url.getOrElse {
      println("-url=<svn url> required when cloning add -update if you would like to update instead of clone")
      sys.exit()
    }

    implicit val repo = params.repo

    if (!repo.exists) {
      println(exec(git, "svn", "clone", "--stdlayout", url, repo.getName)(repo.getParentFile) mkString "\n")
    }
    syncTagsAndBranches(url)
  }
}
