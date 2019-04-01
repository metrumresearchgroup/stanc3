@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

/* Functions that runs a sh command and returns the stdout */
def runShell(String command){
    def output = sh (returnStdout: true, script: "${command}").trim()
    return "${output}"
}

pipeline {
    agent none
    stages {
        stage("Build & Test") {
            agent {
                dockerfile {
                    filename 'docker/dev-ubuntu/Dockerfile'
                    //Forces image to ignore entrypoint
                    args "--entrypoint=\'\'"
                }
            }
            steps {
                /* runs 'dune build @install' command and then outputs the stdout*/
                runShell("""
                    eval \$(opam env)
                    dune build @install
                """)

                runShell("echo \$(date +'%s') > time.log")
                /* runs 'dune runtest' command and then outputs the stdout*/
                echo runShell("""
                    eval \$(opam env)
                    dune runtest --verbose
                """)
                elapsed = runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")
                runShell("echo \$elapsed >> debian_test_duration")
                echo runShell("cat debian_test_duration")

                echo "Runned tests list @all"
                echo runShell("cd test && find -iname '*.stan' && cd ..")

            }
        }
        stage("Build & Test static linux binary") {
            agent {
                dockerfile {
                    filename 'docker/static/Dockerfile'
                    //Forces image to ignore entrypoint
                    args "--entrypoint=\'\'"
                }
            }
            steps {

                /* runs 'dune build @install' command and then outputs the stdout*/
                runShell("""
                    eval \$(opam env)
                    dune build @install --profile static
                """)

                runShell("echo \$(date +'%s') > time.log")
                /* runs 'dune runtest' command and then outputs the stdout*/
                echo runShell("""
                    eval \$(opam env)
                    dune runtest --profile static --verbose
                """)
                elapsed = runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")
                runShell("echo \$elapsed >> alpine_test_duration")
                echo runShell("cat alpine_test_duration")

                echo "Runned tests list @all"
                echo runShell("cd test && find -iname '*.stan' && cd ..")

            }
        }
    }
    //post {
    //    always {
    //        script {utils.mailBuildResults()}
    //    }
    //}
}
