#!/usr/bin/env bash

###############################################################################
## Container runtime abstraction layer for Penpot tooling.
##
## Historically `manage.sh` was hardcoded to Docker (and `docker compose`).
## This library lets the same tooling run on top of either:
##
##   - "docker"    : Docker Engine + Docker Compose v2 (the default).
##   - "container" : Apple's `container` CLI (https://github.com/apple/container),
##                   a native runtime for running Linux containers as
##                   lightweight virtual machines on Apple silicon Macs.
##
## The runtime is autodetected, but can be forced with the
## PENPOT_CONTAINER_RUNTIME environment variable:
##
##   PENPOT_CONTAINER_RUNTIME=docker    ./manage.sh start-devenv
##   PENPOT_CONTAINER_RUNTIME=container ./manage.sh start-devenv
##
## Apple's `container` CLI has no `compose` equivalent, so for that runtime the
## devenv is orchestrated natively here, mirroring docker/devenv/docker-compose.yaml.
## Keep both in sync when you touch one of them.
###############################################################################

## Detect which runtime to use. Honors PENPOT_CONTAINER_RUNTIME, otherwise
## prefers docker when available and falls back to Apple's container CLI.
function runtime-detect {
    local requested="${PENPOT_CONTAINER_RUNTIME:-auto}";

    case "$requested" in
        docker)
            if ! command -v docker >/dev/null 2>&1; then
                echo "PENPOT_CONTAINER_RUNTIME=docker but the 'docker' binary was not found." >&2;
                exit 1;
            fi
            echo "docker";
            ;;
        container)
            if ! command -v container >/dev/null 2>&1; then
                echo "PENPOT_CONTAINER_RUNTIME=container but the 'container' binary was not found." >&2;
                echo "Install Apple's container CLI from https://github.com/apple/container" >&2;
                exit 1;
            fi
            echo "container";
            ;;
        auto)
            if command -v docker >/dev/null 2>&1; then
                echo "docker";
            elif command -v container >/dev/null 2>&1; then
                echo "container";
            else
                echo "Neither 'docker' nor Apple's 'container' CLI were found in PATH." >&2;
                echo "Install one of them, or set PENPOT_CONTAINER_RUNTIME explicitly." >&2;
                exit 1;
            fi
            ;;
        *)
            echo "Unknown PENPOT_CONTAINER_RUNTIME='$requested' (expected: docker | container | auto)." >&2;
            exit 1;
            ;;
    esac
}

## Resolve the runtime once and cache it for the rest of the process.
export PENPOT_RUNTIME="${PENPOT_RUNTIME:-$(runtime-detect)}";

## True when running on top of Apple's container CLI.
function runtime-is-container { [ "$PENPOT_RUNTIME" = "container" ]; }

###############################################################################
## Low level wrappers used by the image build / run helpers in manage.sh.
## They paper over the small differences between the two CLIs so the rest of
## the tooling does not need to branch on the runtime.
###############################################################################

## Ensure the runtime is ready to accept commands. Apple's container CLI needs
## its system services running before anything else; Docker needs nothing here.
function oci-ensure-up {
    if runtime-is-container; then
        container system status >/dev/null 2>&1 || container system start;
    fi
}

## Create a named volume if it does not already exist (idempotent).
function oci-volume-create {
    local name="$1";
    if runtime-is-container; then
        container volume inspect "$name" >/dev/null 2>&1 || container volume create "$name" >/dev/null;
    else
        docker volume create "$name" >/dev/null;
    fi
}

## Echo the image id when the given image is present locally, nothing otherwise.
function oci-image-id {
    local image="$1";
    if runtime-is-container; then
        container image inspect "$image" >/dev/null 2>&1 && echo "$image";
    else
        docker images "$image" -q;
    fi
}

## Pull an image from a registry.
function oci-pull {
    local image="$1";
    if runtime-is-container; then
        container image pull "$image";
    else
        docker pull "$image";
    fi
}

## Echo the volume mount flag for the active runtime.
##   $1 volume name, $2 target path inside the container.
function oci-volume-mount-flag {
    echo "--volume" "$1:$2";
}

## Echo the bind mount flag for the active runtime.
##   $1 host path, $2 target path inside the container.
## Docker keeps the ':z' SELinux relabel hint; Apple's container CLI does not
## use SELinux labels so it is omitted there.
function oci-bind-mount-flag {
    if runtime-is-container; then
        echo "--volume" "$1:$2";
    else
        echo "--volume" "$1:$2:z";
    fi
}

## `docker build` / `container build`. Both accept -t/-f/--build-arg and a
## context directory, so the arguments are forwarded as-is.
function oci-build {
    if runtime-is-container; then
        container build "$@";
    else
        docker build "$@";
    fi
}

## `docker run` / `container run`. Apple's container CLI has no --privileged
## flag (each container is already an isolated VM), so it is stripped out.
function oci-run {
    if runtime-is-container; then
        local args=();
        local a;
        for a in "$@"; do
            [ "$a" = "--privileged" ] && continue;
            args+=("$a");
        done
        container run "${args[@]}";
    else
        docker run "$@";
    fi
}

## `docker exec` / `container exec`.
function oci-exec {
    if runtime-is-container; then
        container exec "$@";
    else
        docker exec "$@";
    fi
}

## Return success when a container with the given name is currently running.
function oci-container-running {
    local name="$1";
    if runtime-is-container; then
        container list --format json 2>/dev/null | grep -q "\"$name\"";
    else
        [ -n "$(docker ps -f "name=$name" -q)" ];
    fi
}

###############################################################################
## Devenv orchestration.
##
## For Docker we simply delegate to `docker compose` against the existing
## docker/devenv/docker-compose.yaml. For Apple's container CLI (which has no
## compose) we bring the same set of services up natively below.
###############################################################################

DEVENV_COMPOSE_FILE="docker/devenv/docker-compose.yaml";

## --- Apple container devenv: configuration mirrored from docker-compose.yaml ---

CONTAINER_DEVENV_NETWORK="${DEVENV_PNAME:-penpotdev}";
CONTAINER_DEVENV_SUBNET="172.177.9.0/24";

CONTAINER_DEVENV_VOL_USER="${DEVENV_PNAME:-penpotdev}_user_data";
CONTAINER_DEVENV_VOL_POSTGRES="${DEVENV_PNAME:-penpotdev}_postgres_pg16";
CONTAINER_DEVENV_VOL_MINIO="${DEVENV_PNAME:-penpotdev}_minio";
CONTAINER_DEVENV_VOL_VALKEY="${DEVENV_PNAME:-penpotdev}_valkey";

## Ports published by the main devenv container (mirrors docker-compose.yaml).
CONTAINER_DEVENV_MAIN_PORTS=(
    3447 3448 3449 3450 6006 6060 6061 6062 6063 6064
    9000 9001 9090 9091 4400 4401 4402 4403 4200 4201 4202
);

function __container-network-ensure {
    if ! container network list --format json 2>/dev/null | grep -q "\"$CONTAINER_DEVENV_NETWORK\""; then
        container network create --subnet "$CONTAINER_DEVENV_SUBNET" "$CONTAINER_DEVENV_NETWORK" >/dev/null;
    fi
}

## Run a backing service container only when it is not already running.
##   $1 name, rest: extra `container run` args + image (+ command).
function __container-service-run {
    local name="$1"; shift;
    if oci-container-running "$name"; then
        return 0;
    fi
    # Remove any stopped leftover with the same name before starting.
    container delete "$name" >/dev/null 2>&1 || true;
    container run -d --name "$name" --network "$CONTAINER_DEVENV_NETWORK" "$@";
}

function __container-devenv-up {
    oci-ensure-up;
    __container-network-ensure;

    oci-volume-create "$CONTAINER_DEVENV_VOL_USER";
    oci-volume-create "$CONTAINER_DEVENV_VOL_POSTGRES";
    oci-volume-create "$CONTAINER_DEVENV_VOL_MINIO";
    oci-volume-create "$CONTAINER_DEVENV_VOL_VALKEY";

    ## postgres (DNS name: postgres)
    __container-service-run postgres \
        --volume "$CONTAINER_DEVENV_VOL_POSTGRES:/var/lib/postgresql/data" \
        --volume "${PWD}/docker/devenv/files/postgresql.conf:/etc/postgresql.conf" \
        --volume "${PWD}/docker/devenv/files/postgresql_init.sql:/docker-entrypoint-initdb.d/init.sql" \
        -e POSTGRES_INITDB_ARGS=--data-checksums \
        -e POSTGRES_DB=penpot \
        -e POSTGRES_USER=penpot \
        -e POSTGRES_PASSWORD=penpot \
        postgres:16.8 postgres -c config_file=/etc/postgresql.conf;

    ## valkey / redis (DNS name: redis)
    __container-service-run redis \
        --volume "$CONTAINER_DEVENV_VOL_VALKEY:/data" \
        valkey/valkey:8.1 valkey-server --save 120 1 --loglevel warning;

    ## minio (DNS name: minio)
    __container-service-run minio \
        --volume "$CONTAINER_DEVENV_VOL_MINIO:/mnt/data" \
        -e MINIO_ROOT_USER=minioadmin \
        -e MINIO_ROOT_PASSWORD=minioadmin \
        minio/minio:RELEASE.2025-04-03T14-56-28Z server /mnt/data --console-address ":9001";

    ## mailcatcher (DNS name: mailer)
    __container-service-run mailer \
        -p 1080:1080 \
        sj26/mailcatcher:latest;

    ## test openldap (DNS name: ldap)
    __container-service-run ldap \
        -p 10389:10389 -p 10636:10636 \
        rroemhild/test-openldap:2.1;

    ## main devenv container (DNS name + container name: penpot-devenv-main)
    if ! oci-container-running "penpot-devenv-main"; then
        container delete "penpot-devenv-main" >/dev/null 2>&1 || true;

        local port_flags=();
        local p;
        for p in "${CONTAINER_DEVENV_MAIN_PORTS[@]}"; do
            port_flags+=("-p" "$p:$p");
        done

        container run -d --name penpot-devenv-main \
            --network "$CONTAINER_DEVENV_NETWORK" \
            --volume "$CONTAINER_DEVENV_VOL_USER:/home/penpot/" \
            --volume "${PWD}:/home/penpot/penpot" \
            "${port_flags[@]}" \
            -e EXTERNAL_UID="${CURRENT_USER_ID}" \
            -e PENPOT_SMTP_ENABLED=true \
            -e PENPOT_SMTP_DEFAULT_FROM=no-reply@example.com \
            -e PENPOT_SMTP_DEFAULT_REPLY_TO=no-reply@example.com \
            -e PENPOT_SMTP_HOST=mailer \
            -e PENPOT_SMTP_PORT=1025 \
            -e PENPOT_SMTP_USERNAME= \
            -e PENPOT_SMTP_PASSWORD= \
            -e PENPOT_SMTP_SSL=false \
            -e PENPOT_SMTP_TLS=false \
            -e PENPOT_LDAP_HOST=ldap \
            -e PENPOT_LDAP_PORT=10389 \
            -e PENPOT_LDAP_SSL=false \
            -e PENPOT_LDAP_STARTTLS=false \
            -e PENPOT_LDAP_BASE_DN=ou=people,dc=planetexpress,dc=com \
            -e PENPOT_LDAP_BIND_DN=cn=admin,dc=planetexpress,dc=com \
            -e PENPOT_LDAP_BIND_PASSWORD=GoodNewsEveryone \
            -e PENPOT_LDAP_ATTRS_USERNAME=uid \
            -e PENPOT_LDAP_ATTRS_EMAIL=mail \
            -e PENPOT_LDAP_ATTRS_FULLNAME=cn \
            -e PENPOT_LDAP_ATTRS_PHOTO=jpegPhoto \
            "${DEVENV_IMGNAME}:latest";
    fi
}

function __container-devenv-stop {
    local name;
    for name in penpot-devenv-main ldap mailer minio redis postgres; do
        container stop "$name" >/dev/null 2>&1 || true;
    done
}

function __container-devenv-down {
    local name;
    for name in penpot-devenv-main ldap mailer minio redis postgres; do
        container stop "$name" >/dev/null 2>&1 || true;
        container delete "$name" >/dev/null 2>&1 || true;
    done

    container network delete "$CONTAINER_DEVENV_NETWORK" >/dev/null 2>&1 || true;

    container volume delete "$CONTAINER_DEVENV_VOL_USER" >/dev/null 2>&1 || true;
    container volume delete "$CONTAINER_DEVENV_VOL_POSTGRES" >/dev/null 2>&1 || true;
    container volume delete "$CONTAINER_DEVENV_VOL_MINIO" >/dev/null 2>&1 || true;
    container volume delete "$CONTAINER_DEVENV_VOL_VALKEY" >/dev/null 2>&1 || true;
}

function __container-devenv-logs {
    container logs --follow -n 50 penpot-devenv-main;
}

## --- Public devenv operations dispatched on the active runtime ---

function runtime-devenv-up {
    if runtime-is-container; then
        __container-devenv-up;
    else
        docker compose -p "$DEVENV_PNAME" -f "$DEVENV_COMPOSE_FILE" up -d;
    fi
}

function runtime-devenv-create {
    if runtime-is-container; then
        ## Apple's container CLI has no detached "create" step; bringing the
        ## services up is the closest equivalent and is idempotent.
        __container-devenv-up;
    else
        docker compose -p "$DEVENV_PNAME" -f "$DEVENV_COMPOSE_FILE" create;
    fi
}

function runtime-devenv-stop {
    if runtime-is-container; then
        __container-devenv-stop;
    else
        docker compose -p "$DEVENV_PNAME" -f "$DEVENV_COMPOSE_FILE" stop -t 2;
    fi
}

function runtime-devenv-down {
    if runtime-is-container; then
        __container-devenv-down;
    else
        docker compose -p "$DEVENV_PNAME" -f "$DEVENV_COMPOSE_FILE" down -t 2 -v;
    fi
}

function runtime-devenv-logs {
    if runtime-is-container; then
        __container-devenv-logs;
    else
        docker compose -p "$DEVENV_PNAME" -f "$DEVENV_COMPOSE_FILE" logs -f --tail=50;
    fi
}
