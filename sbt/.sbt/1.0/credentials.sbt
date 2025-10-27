import scala.sys.process._

credentials += Credentials(
  "Artifactory Realm",
  "ista.jfrog.io",
  "cas-ci-rdm-readwrite",
  "pass show ista/artifactory-cas-ci-rdm-readwrite".!!.trim
)

