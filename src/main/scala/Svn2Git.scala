import collection.JavaConverters._
import concurrent.SyncVar
import java.io.File


object Svn2Git {
  def exec(cmd:String*)(implicit dir:File) = {
    println(cmd mkString " ")

    val b = new ProcessBuilder(cmd.asJava)
    b.redirectErrorStream(true);
    b.directory(dir)
    val p = b.start

    val in = new SyncVar[List[String]]()
    val t = new Thread {
      override def run = {
        val s = io.Source.fromInputStream(p.getInputStream)
        in put s.getLines().toList
      }

    }

    t.start

    p.waitFor

    t.join

    in.get(5000) getOrElse(throw new Exception("Failed to execute: "+cmd.mkString(" ")))
  }

  def main (args:Array[String]) {
    val (flags,params) = args.partition{_ startsWith "-"}

    if(flags.contains("-update")) {
      update(flags,params)
    } else {
      clone(flags, params)
    }
  }

  def lookUpSvnURLFromConfig(repo:File) = {
    val config = new File(new File(repo,".git"),"config")
    val svnSection = io.Source.fromFile(config).getLines().dropWhile(_.trim != "[svn-remote \"svn\"]")
    val Url = """\s*url\s+=\s*(.+)""".r
    svnSection.collect{case Url(url) => url.trim}.next
  }

  def update(flags:Seq[String], params:Seq[String]) {
    implicit val repo = new File(params.headOption getOrElse ".")
    val url = lookUpSvnURLFromConfig(repo)
    val Branch = """.*\(refs/remotes/(.+?)\).*""".r
    exec("git","svn","fetch")

    syncTagsAndBranches(url)

    gitBranches filter {_ != "master"} foreach { b =>
      exec("git","co",b)
      exec("git","rebase","remotes/"+b)
    }

    exec("git","co","master")
    exec("git","rebase","remotes/trunk")
  }

  def gitBranches(implicit repo:File) = exec("git","branch","-l", "--no-color") map {_.trim.dropWhile(_ == '*').trim}
  def gitTags(implicit repo:File) = exec("git","tag","-l") map {_.trim}

  def syncTagsAndBranches(url:String)(implicit repo:File) = {
    exec("git","co","master")
    val svnBranches = exec("svn", "ls", url+"/branches") map {_.trim.dropRight(1)}
    val svnTags = exec("svn", "ls", url+"/tags") map {_.trim.dropRight(1)}

    val gitBranches = this.gitBranches
    val gitTags = this.gitTags

    val notMaster = (branch:String) => branch != "master" && branch != "trunk"
    // add missing branches
    for{branch <- svnBranches
        if !(gitBranches contains branch)
        if notMaster(branch)
        remote = "remotes/"+branch} {
      exec("git","co","-b",branch,remote)
    }

    // delete removed branches
    for{branch <- gitBranches
        if notMaster(branch) && !(svnBranches contains branch) } {
      exec("git", "branch", "-D", branch)
    }


    // delete git tags so that if there was an update
    // in SVN we will get the updated version of the tag
    // when we rebuild the tags
    for{tag <- gitTags} {
      exec("git", "tag", "-d", tag)
    }

    // add missing branches
    for{tag <- svnTags
        remote = "remotes/tags/"+tag} {

      val name = if(svnBranches contains tag) tag+"_tag" else tag


      exec("git", "tag","-m","SVN tag "+tag,name,remote)
    }
  }
  def clone(flags:Seq[String], params:Seq[String]) {
    val url = params(0)
    implicit val repo = new File(params.drop(1).headOption getOrElse ".")

    if(!repo.exists) {
      println(exec("git", "svn","clone", "--stdlayout", url, repo.getName)(repo.getParentFile) mkString "\n")
    }
    syncTagsAndBranches(url)
  }


}