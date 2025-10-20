import scala.sys.process._

credentials += Credentials(
  "Artifactory Realm",
  "ista.jfrog.io",
  "cas-cd-rdm-readonly",
  "pass show ista/artifactory-cas-cd-rdm-readonly".!!.trim
)
