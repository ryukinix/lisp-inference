FROM commonlispbr/roswell
WORKDIR /app
RUN ros install commonlispbr/quicksys
RUN ros run -s quicksys -e "(qs:install-dist :ultralisp)" -q
RUN ros run -s 40ants-routes -s reblocks -s reblocks-ui -q
COPY roswell roswell
COPY web web
COPY t t
COPY src src
COPY lisp-inference.asd .
RUN ros install ./lisp-inference.asd
RUN ros run -s lisp-inference/web -q
EXPOSE 40000
ENTRYPOINT "/root/.roswell/bin/inference-server"
