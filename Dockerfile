FROM commonlispbr/reblocks:latest
WORKDIR /app
COPY roswell roswell
COPY web web
COPY t t
COPY src src
COPY lisp-inference.asd .
COPY *.lisp .
RUN ros install ./lisp-inference.asd
RUN ros run -s lisp-inference/web -q
EXPOSE 40000
ENTRYPOINT ["/app/roswell/inference-server.ros"]
