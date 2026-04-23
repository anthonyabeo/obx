## Multi-stage Dockerfile for building obx server suitable for ECS Fargate
## - Builder stage installs node/npm to run scripts/vendor_static.sh when needed
## - Produces a statically linked Go binary (no CGO) and copies it into a small runtime image

## Use bullseye base: buster archives are sometimes removed from mirrors causing
## "does not have a Release file" errors. Bullseye is newer and still supported.
FROM golang:1.24-bullseye AS builder
LABEL stage=builder

# Install tools needed for vendoring (curl, git, node/npm)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates git nodejs npm build-essential \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /src

# Download modules early
COPY go.mod go.sum ./
RUN go mod download

# Copy full repo
COPY . .

# Make vendor script executable and run it if present. This is safe if static files
# are already committed; the script is idempotent.
RUN if [ -x ./scripts/vendor_static.sh ]; then ./scripts/vendor_static.sh; fi || true

# Build the server binary. The project places the main package under ./cmd.
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o /out/obx ./cmd/obx.go

## Runtime image
FROM alpine:3.18
RUN apk add --no-cache ca-certificates

# Copy binary from builder
COPY --from=builder /out/obx /usr/local/bin/obx

EXPOSE 8080
ENV PORT=8080

ENTRYPOINT ["/usr/local/bin/obx", "web"]

