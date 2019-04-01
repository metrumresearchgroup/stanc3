@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

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
                    args '-v ${PWD}:/app -w /app'
                }
            }
            steps {
                /* runs dune build install command and then outputs the stdout*/
                echo runShell('''
                    eval \$(opam env)
                    dune build @install
                ''')

                /* runs dune runtest command and then outputs the stdout*/
                echo runShell('''
                    eval \$(opam env)
                    dune runtest
                ''')

                // No idea how the build files from this docker image end
                // up transmitting to the next docker images, so clean here
                // (because the next image can't delete this one's files due
                // to the root user thing)

                //sh "git clean -xffd"
            }
        }
        stage("Build & Test static linux binary") {
            agent {
                dockerfile {
                    filename 'docker/static/Dockerfile'
                    args '-v ${PWD}:/app -w /app'
                }
            }
            steps {

                /* runs dune build install command and then outputs the stdout*/
                echo runShell('''
                    eval \$(opam env)
                    dune build @install --profile static
                ''')

                /* runs dune runtest command and then outputs the stdout*/
                echo runShell('''
                    eval \$(opam env)
                    dune runtest --profile static
                ''')

            }
        }
    }
    post {
        always {
            script {utils.mailBuildResults()}
        }
    }

}
