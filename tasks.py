
from invoke import task
import os
import dotenv

dotenv.load_dotenv()


HERE = os.path.split(__file__)[0]
PWD  = os.environ.get("PWD")


@task
def dev(c):
    c.run(f"""
        cd {HERE}
        npm run watch-css &
        export CSS=$(echo $!)
        elm-app start --no-browser &
        export ELM=$(echo $!)

        function end {{
            kill -TERM $CSS $ELM
            echo killed $CSS $ELM
        }}

        trap end EXIT

        echo Hit Ctrl-C to stop
        while /bin/true ; do
            sleep 10
        done


    """)

@task
def deploy(c):
    c.run(f"""
        cd {HERE}
        export DEPLOY=$(git rev-parse HEAD)
        npm run build-css && \
        PUBLIC_URL=/followme/ elm-app build &&\
        cd build-remote && \
        cp -rvu ../build/* . && \
        git add * && \
        git commit -a -m "built from $DEPLOY"
        git push
    """)
