import scala.sys.process._

credentials += Credentials(
  "Artifactory Realm",
  "ista.jfrog.io",
  "gunnar.bastkowski@ista.com",
  "pass show ista/artifactory-gunnar".!!.trim
)
