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

                runShell("start=\$(date +'%s')")
                /* runs 'dune runtest' command and then outputs the stdout*/
                echo runShell("""
                    eval \$(opam env)
                    dune runtest --verbose
                """)
                echo runShell("echo \"It took \$((\$(date +'%s') - \$start)) seconds to run the tests\"")

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

                runShell("start=\$(date +'%s')")
                /* runs 'dune runtest' command and then outputs the stdout*/
                echo runShell("""
                    eval \$(opam env)
                    dune runtest --profile static --verbose
                """)
                echo runShell("echo \"It took \$((\$(date +'%s') - \$start)) seconds to run the tests\"")

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
