#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "${script_dir}/.." && pwd)"

detect_lang() {
  if [[ -f "${repo_root}/Cargo.toml" ]]; then
    printf '%s\n' rust
  elif [[ -f "${repo_root}/go.mod" ]]; then
    printf '%s\n' go
  elif [[ -f "${repo_root}/build.zig" ]]; then
    printf '%s\n' zig
  else
    printf '%s\n' unknown
  fi
}

lang="${RUNIC_LANG:-$(detect_lang)}"

ensure_runic_bin() {
  if [[ -n "${RUNIC_BIN:-}" ]]; then
    printf '%s\n' "${RUNIC_BIN}"
    return 0
  fi

  if [[ -x "${repo_root}/zig-out/bin/runic" ]]; then
    printf '%s\n' "${repo_root}/zig-out/bin/runic"
    return 0
  fi

  if command -v runic >/dev/null 2>&1; then
    command -v runic
    return 0
  fi

  if [[ "${lang}" == "zig" ]] && command -v zig >/dev/null 2>&1; then
    echo "runic binary not found; building with zig build" >&2
    (cd "${repo_root}" && zig build)
    if [[ -x "${repo_root}/zig-out/bin/runic" ]]; then
      printf '%s\n' "${repo_root}/zig-out/bin/runic"
      return 0
    fi
  fi

  echo "Unable to find a runic binary. Set RUNIC_BIN or build zig-out/bin/runic first." >&2
  return 1
}

run_shell_fallback() {
  echo "Falling back to direct shell stages" >&2
  local -a stages=(
    formatter
    linter
    unit_tests
    cli_smoke
  )

  local stage
  for stage in "${stages[@]}"; do
    echo ""
    echo "==> ${stage}"
    RUNIC_REPO_ROOT="${repo_root}" RUNIC_LANG="${lang}" bash "${repo_root}/scripts/stages/${stage}.sh"
  done
}

run_runic_ci() {
  local runic_bin="$1"
  local combined_log="$2"
  local status=0

  if command -v timeout >/dev/null 2>&1; then
    RUNIC_REPO_ROOT="${repo_root}" RUNIC_LANG="${lang}" \
      timeout 20m "${runic_bin}" "${repo_root}/scripts/run_ci.rn" \
      > >(tee "${combined_log}") \
      2> >(tee -a "${combined_log}" >&2) || status=$?
  else
    RUNIC_REPO_ROOT="${repo_root}" RUNIC_LANG="${lang}" \
      "${runic_bin}" "${repo_root}/scripts/run_ci.rn" \
      > >(tee "${combined_log}") \
      2> >(tee -a "${combined_log}" >&2) || status=$?
  fi

  return "${status}"
}

cleanup_file() {
  local path="${1:-}"
  if [[ -n "${path}" && -e "${path}" ]]; then
    rm -f "${path}"
  fi
}

main() {
  local runic_bin
  runic_bin="$(ensure_runic_bin)"

  local combined_log
  combined_log="$(mktemp)"
  trap "cleanup_file '${combined_log}'" EXIT

  local runic_status=0
  run_runic_ci "${runic_bin}" "${combined_log}" || runic_status=$?

  local should_fallback=0

  if [[ ${runic_status} -ne 0 ]]; then
    echo "Runic CI wrapper failed with status ${runic_status}" >&2
    should_fallback=1
  fi

  if [[ ! -s "${combined_log}" ]]; then
    echo "Runic CI wrapper produced no output" >&2
    should_fallback=1
  fi

  if ! grep -q 'Detected toolchain:' "${combined_log}"; then
    echo "Runic CI wrapper did not emit the expected toolchain marker" >&2
    should_fallback=1
  fi

  if grep -q 'stage skipped: no recognized toolchain' "${combined_log}"; then
    echo "Runic CI wrapper skipped one or more stages because toolchain propagation failed" >&2
    should_fallback=1
  fi

  if ! grep -q 'CLI smoke tests passed' "${combined_log}"; then
    echo "Runic CI wrapper did not reach the CLI smoke success marker" >&2
    should_fallback=1
  fi

  if [[ ${should_fallback} -eq 1 ]]; then
    run_shell_fallback
    return 0
  fi

  return 0
}

main "$@"
