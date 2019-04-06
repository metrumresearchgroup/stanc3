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
                /* Sets the UID of opam user to the Jenkins Agent UID to avoid permission issues */
                runShell("""
                    if [ \$(id -u \${USER}) -eq 1000 ]; then usermod -u 9999 opam_ec2; fi
                    usermod -u \$(id -u \${USER}) opam_jenkins
                """)

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
                echo runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")

                //Cleans the workspace
                deleteDir()

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

                /* Sets the UID of opam user to the Jenkins Agent UID to avoid permission issues */
                runShell("""
                    if [ \$(id -u \${USER}) -eq 1000 ]; then usermod -u \$(id -u \${USER}) opam_ec2; elif [ \$(id -u \${USER}) -eq 1004 ]; then usermod -u \$(id -u \${USER}) opam_jenkins; fi
                """)

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
                echo runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")

                //Cleans the workspace
                deleteDir()

            }
        }
    }
}
