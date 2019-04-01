@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

pipeline {
    agent none
    stages {
        stage("Build & Test") {
            agent {
                dockerfile {
                    filename 'docker/dev-ubuntu/Dockerfile'
                    args "--entrypoint=\'\'" // TODO: set up a proper user in Dockerfile
                }
            }
            steps {
                /* runs dune build install command and then outputs the stdout*/
                echo sh(returnStdout: true, script: '''
                    eval \$(opam env)
                    dune build @install
                ''').result

                /* runs dune runtest command and then outputs the stdout*/
                echo sh(returnStdout: true, script: '''
                    eval \$(opam env)
                    dune runtest
                ''').result

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
                }
            }
            steps {

                /* runs dune build install command and then outputs the stdout*/
                echo sh(returnStdout: true, script: '''
                    eval \$(opam env)
                    dune build @install --profile static
                ''').result

                /* runs dune runtest command and then outputs the stdout*/
                echo sh(returnStdout: true, script: '''
                    eval \$(opam env)
                    dune runtest --profile static
                ''').result

            }
        }
    }
    post {
        always {
            script {utils.mailBuildResults()}
        }
    }

}
