FROM ocaml/opam:debian-11-ocaml-5.2

# Set the working directory inside the container
WORKDIR /app

# Install Dune (and optionally other tools)
RUN opam update && opam install dune -y

# Switch to the opam user (already configured in the base image)
USER opam

# Copy project files into the container
COPY --chown=opam:opam . .

# Install project dependencies
RUN opam install . --deps-only -y

# Build the project
RUN dune build

# Run tests (optional)
RUN dune runtest
