#!/usr/bin/env python3
"""Process facts, conservative recognition, and the fkill/fterm fzf interface."""

from __future__ import annotations

import os
import re
import sys
import json
import shlex
import signal
import argparse
import tempfile
import subprocess
import unicodedata
from pathlib import Path
from dataclasses import field, dataclass

BROWSERS = frozenset(
    "firefox firefox-bin chromium chromium-browse chrome google-chrome brave brave-browser vivaldi vivaldi-bin zen zen-bin".split()
)
CODE = frozenset(("code", "code-insiders"))
SHELLS = frozenset(("zsh", "bash", "fish", "sh", "dash", "nu"))
TERMINALS = frozenset(
    ("kitty", "alacritty", "foot", "foot-server", "wezterm-gui", "konsole", "gnome-terminal-server", "ghostty")
)
FZF_FIELDS = ("--no-sort", "--delimiter=\t", "--with-nth=2..")


def clean(value: str) -> str:
    return "".join(c if not unicodedata.category(c).startswith("C") else f"\\x{ord(c):02x}" for c in value)


def parse_stat(text: str) -> tuple[int, int, int]:
    # comm may contain spaces and closing parentheses; the final ')' ends it.
    fields = text[text.rindex(")") + 2 :].split()
    return int(fields[1]), int(fields[4]), int(fields[19])  # ppid, tty_nr, starttime


def parse_cmdline(data: bytes) -> tuple[tuple[str, ...], bool]:
    args = tuple(x.decode(errors="replace") for x in data.rstrip(b"\0").split(b"\0") if x)
    return args, len(args) == 1 and any(c.isspace() for c in args[0])


@dataclass
class Process:
    pid: int
    ppid: int
    start: int
    comm: str
    cpu: str = "?"
    mem: str = "?"
    argv: tuple[str, ...] = ()
    flat: bool = False
    exe: str = ""
    cwd: str = ""
    tty: str = "?"
    cgroup: str = ""
    app: str = ""
    role: str = "process"
    reason: str = "unrecognized; retained"
    owner: int | None = None
    windows: list[dict] = field(default_factory=list)

    @property
    def token(self) -> str:
        return f"{self.pid}:{self.start}"

    def flag(self, name: str) -> bool:
        if self.flat:
            return bool(re.search(r"(?<!\S)" + re.escape(name) + r"(?=\s|$)", self.argv[0]))
        return name in self.argv

    def prefix(self, name: str) -> bool:
        if self.flat:
            return bool(re.search(r"(?<!\S)" + re.escape(name) + r"[^\s]+", self.argv[0]))
        return any(arg.startswith(name) and len(arg) > len(name) for arg in self.argv)


def collect() -> dict[int, Process]:
    # ps owns CPU/MEM semantics: lifetime average %CPU and per-PID %MEM.
    result = subprocess.run(
        ["ps", "-ww", "-u", str(os.getuid()), "-o", "pid=,ppid=,pcpu=,pmem=,comm="],
        env={**os.environ, "LC_ALL": "C"},
        capture_output=True,
        text=True,
        check=True,
    )
    processes = {}
    for line in result.stdout.splitlines():
        values = line.split(None, 4)
        if len(values) != 5:
            continue
        pid, parent, cpu, mem, comm = values
        base = Path("/proc") / pid
        try:
            ppid, tty, start = parse_stat((base / "stat").read_text())
            if ppid != int(parent):
                continue  # ps and /proc disagree: refresh rather than mix identities.
            args, flat = parse_cmdline((base / "cmdline").read_bytes())
            p = Process(int(pid), ppid, start, comm, cpu, mem, args, flat, tty=str(tty) if tty else "-")
            for name in ("exe", "cwd"):
                try:
                    setattr(p, name, os.readlink(base / name))
                except OSError:
                    pass
            try:
                p.cgroup = (base / "cgroup").read_text().strip()
            except OSError:
                pass
            if parse_stat((base / "stat").read_text())[2] != start:
                continue
            processes[p.pid] = p
        except (OSError, ValueError, IndexError):
            continue  # exit races / inaccessible processes cannot be actionable targets.
    return processes


def window_snapshot() -> tuple[list[dict], str]:
    if not os.environ.get("HYPRLAND_INSTANCE_SIGNATURE"):
        return [], "windows unavailable: no Hyprland instance in environment"
    try:
        result = subprocess.run(["hyprctl", "-j", "clients"], capture_output=True, text=True, timeout=2, check=True)
        windows = json.loads(result.stdout)
        if not isinstance(windows, list) or not all(isinstance(w, dict) for w in windows):
            raise ValueError("invalid clients response")
        return windows, "Hyprland windows attached where PID matches"
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        return [], f"windows unavailable: {type(error).__name__}"


def ancestors(p: Process, processes: dict[int, Process]):
    seen = {p.pid}
    parent = p.ppid
    while parent in processes and parent not in seen:
        seen.add(parent)
        p = processes[parent]
        yield p
        parent = p.ppid


def recognize(processes: dict[int, Process], windows: list[dict]) -> None:
    by_pid: dict[int, list[dict]] = {}
    for window in windows:
        by_pid.setdefault(window.get("pid"), []).append(window)
    for p in processes.values():
        p.app, p.role, p.reason, p.owner = "", "process", "unrecognized; retained", None
        p.windows = by_pid.get(p.pid, [])
        text = "\n".join((p.exe, *p.argv))
        if p.comm.lstrip("-") in SHELLS:
            p.role, p.reason = "shell", "shell executable name"
        elif p.comm in CODE:
            p.app, p.reason = p.comm, "exact Code executable name"
        elif p.comm in BROWSERS:
            p.app, p.reason = p.comm, "exact browser executable name"
        # Explicit package paths, never bare "electron" or a Nix store hash.
        elif (
            (p.comm in ("electron", "obsidian", "cherry-studio") or "crashpad" in p.comm)
            and re.search(r"/(?:cherry-studio[^/]*|obsidian)/", text, re.I)
            and ("app.asar" in text or "/resources/" in text)
        ):
            p.app = "Cherry Studio" if re.search(r"/cherry-studio[^/]*/", text, re.I) else "Obsidian"
            p.reason = "explicit application package path"
        if p.app:
            p.owner = p.pid
            if p.app in CODE and p.flag("--type=renderer") and p.prefix("--vscode-window-config="):
                p.role = "window"
                p.reason += "; Code window renderer flags (internal API)"
            elif p.prefix("--type=") or p.flag("--crashpad-handler") or "crashpad_handler" in p.comm:
                p.role = "helper"
            else:
                p.role = "main"

    # Only direct evidence participates in ownership; inherited labels must not
    # turn a shell's user commands into application helpers.
    explicit = {p.pid for p in processes.values() if p.app}
    for p in processes.values():
        owner = None
        for ancestor in ancestors(p, processes):
            if ancestor.role == "shell":
                break
            if ancestor.pid in explicit:
                owner = ancestor
                break
        if owner and (not p.app or p.app == owner.app):
            p.owner = owner.pid
            if not p.app:
                p.app = owner.app
                p.reason = f"PPID ancestry to {owner.pid} ({owner.app})"
                if p.role != "shell":
                    p.role = "helper"
            elif p.role == "main":
                p.role = "helper"
                p.reason += f"; same application ancestor {owner.pid}"
        if p.windows and p.role not in ("main", "shell"):
            p.role = "window"
            p.reason += "; compositor window PID"


def children_of(processes: dict[int, Process]) -> dict[int, list[int]]:
    children: dict[int, list[int]] = {}
    for p in processes.values():
        children.setdefault(p.ppid, []).append(p.pid)
    for children_list in children.values():
        children_list.sort()
    return children


def render(processes: dict[int, Process], mode: str, expanded: set[int]) -> list[str]:
    children = children_of(processes)
    shown = set()
    for p in processes.values():
        if mode == "all" or p.role != "helper" or not any(a.app == p.app for a in ancestors(p, processes)):
            shown.add(p.pid)
        elif any(a.pid in expanded for a in ancestors(p, processes)):
            shown.add(p.pid)
    hidden_counts: dict[int, int] = {}
    for pid in processes.keys() - shown:
        for ancestor in ancestors(processes[pid], processes):
            hidden_counts[ancestor.pid] = hidden_counts.get(ancestor.pid, 0) + 1
    # Attach visible nodes to their nearest visible real PPID ancestor.
    parents = {}
    for pid in shown:
        parents[pid] = next((a.pid for a in ancestors(processes[pid], processes) if a.pid in shown), None)
    display_children: dict[int | None, list[int]] = {}
    for pid, parent in parents.items():
        display_children.setdefault(parent, []).append(pid)
    lines, visited = [], set()
    stack = [(pid, 0) for pid in sorted(display_children.get(None, ()), reverse=True)]
    # Corrupt/cyclic snapshots must neither loop nor drop targets.
    while len(visited) < len(shown):
        if not stack:
            stack.append((min(shown - visited), 0))
        pid, depth = stack.pop()
        if pid in visited:
            continue
        visited.add(pid)
        p = processes[pid]
        hidden = hidden_counts.get(pid, 0)
        marker = "+" if hidden else "-" if pid in expanded else " "
        label = f"{p.app or p.comm} [{p.role}]"
        if p.role == "shell":
            chain = list(ancestors(p, processes))
            context = next((f"Code terminal via {a.pid}" for a in chain if a.app in CODE), "")
            context = context or next((f"SSH via {a.pid}" for a in chain if a.comm.startswith("sshd")), "")
            context = context or next(
                (f"{a.comm} via {a.pid}" for a in chain if a.comm in TERMINALS), "background / unknown terminal"
            )
            commands = ", ".join(f"{processes[c].comm}({c})" for c in children.get(pid, ()))
            label += f" tty={p.tty} {context}"
            if p.cwd:
                label += f" cwd={p.cwd}"
            if commands:
                label += f" → {commands}"
        elif p.role == "process" and p.argv:
            command = p.argv[0] if p.flat else shlex.join(p.argv)
            label += f" {command[:180]}"
        if p.windows:
            label += " | " + " | ".join(
                f"{w.get('title', '?')} [ws={w.get('workspace', {}).get('name', '?')}, class={w.get('class', '?')}]"
                for w in p.windows
            )
        elif p.role == "window":
            label += " (renderer PID, not a window-close action)"
        if hidden:
            label += f" (+{hidden} hidden; → expand)"
        lines.append(f"{p.token}\t{pid:<8} {p.cpu:>6} {p.mem:>6}  {'  ' * min(depth, 12)}{marker} {clean(label)}")
        stack.extend((child, depth + 1) for child in sorted(display_children.get(pid, ()), reverse=True))
    return lines


def collapse(processes: dict[int, Process], expanded: set[int], pid: int) -> None:
    branch = (
        pid
        if pid in expanded
        else next((a.pid for a in ancestors(processes[pid], processes) if a.pid in expanded), None)
    )
    if branch is not None:
        expanded.difference_update(
            {p for p in expanded if p == branch or any(a.pid == branch for a in ancestors(processes[p], processes))}
        )


def preview_data(p: Process) -> str:
    command = p.argv[0] if p.flat else shlex.join(p.argv)
    lines = [
        f"PID {p.pid}  PPID {p.ppid}  starttime {p.start}",
        f"{p.app or p.comm}: {p.role}",
        f"Recognition: {p.reason}",
        f"CPU {p.cpu}% (lifetime average); MEM {p.mem}% (this PID only)",
        f"TTY device: {p.tty}",
        f"Executable: {p.exe or 'unavailable'}",
        f"CWD: {p.cwd or 'unavailable'}",
        f"Cgroup (fact, not application identity): {p.cgroup}",
        f"Command ({'flattened; original argv boundaries unavailable' if p.flat else 'NUL-separated argv'}): {command}",
    ]
    return "\n".join(clean(line) for line in lines)


def send_targets(processes: dict[int, Process], tokens: list[str], sig: int) -> int:
    failed = False
    targets = {p.token: p for p in processes.values()}
    for token in dict.fromkeys(tokens):
        p = targets.get(token)
        if p is None:
            print(f"Unknown target: {clean(token)}", file=sys.stderr)
            failed = True
            continue
        fd = None
        try:
            # Open first, then check starttime. A later PID reuse cannot redirect pidfd.
            fd = os.pidfd_open(p.pid)
            base = Path("/proc") / str(p.pid)
            if base.stat().st_uid != os.getuid() or parse_stat((base / "stat").read_text())[2] != p.start:
                raise ProcessLookupError("snapshot identity changed; signal not sent")
            signal.pidfd_send_signal(fd, sig)
        except (OSError, ValueError) as error:
            print(f"PID {p.pid}: {error}", file=sys.stderr)
            failed = True
        finally:
            if fd is not None:
                os.close(fd)
    return int(failed)


def parse_signal(value: str) -> int:
    name = value.upper().removeprefix("-").removeprefix("SIG")
    try:
        number = int(name) if name.isdecimal() else int(getattr(signal, "SIG" + name))
    except (ValueError, AttributeError):
        raise ValueError(f"Unknown signal: {value}") from None
    if number not in {int(s) for s in signal.valid_signals()} | {0}:
        raise ValueError(f"Unknown signal: {value}")
    return number


def _focus_position(processes: dict[int, Process], rows: list[str], query: str, focus: str, env: dict[str, str]) -> int:
    if not focus:
        return 1
    visible = {row.split("\t", 1)[0] for row in rows}
    if focus not in visible:
        p = processes.get(int(focus.split(":", 1)[0]))
        # A collapsed helper should focus its real visible ancestor, never an
        # unrelated first row. Do not transfer focus across PID reuse.
        focus = (
            next((a.token for a in ancestors(p, processes) if a.token in visible), "") if p and p.token == focus else ""
        )
        if not focus:
            return 1
    if query:
        # Let the same fzf matcher determine rank; input index is not result rank.
        result = subprocess.run(
            ["fzf", *FZF_FIELDS, "--filter", query],
            input="\n".join(rows) + "\n",
            text=True,
            capture_output=True,
            env=env,
        )
        if result.returncode not in (0, 1):
            raise OSError(f"fzf focus filtering failed: {result.stderr.strip()}")
        rows = result.stdout.splitlines()
    return next((i + 1 for i, row in enumerate(rows) if row.split("\t", 1)[0] == focus), 1)


def picker(sig: int, signal_name: str) -> int:
    processes = collect()
    windows, status = window_snapshot()
    recognize(processes, windows)
    mode, query, expanded, focus = "compact", "", set(), ""
    helper = shlex.quote(str(Path(__file__).resolve()))
    python = shlex.quote(sys.executable)
    env = {**os.environ, "FZF_DEFAULT_OPTS": "", "FZF_DEFAULT_OPTS_FILE": ""}
    with tempfile.TemporaryDirectory(prefix="fprocess-") as directory:
        preview = Path(directory) / "preview.json"
        while True:
            preview.write_text(json.dumps({p.token: preview_data(p) for p in processes.values()}))
            rows = render(processes, mode, expanded)
            position = _focus_position(processes, rows, query, focus, env)
            bindings = ",".join(
                f"{key}:clear-multi+print({key})+accept" for key in ("right", "left", "ctrl-a", "ctrl-r")
            )
            args = [
                "fzf",
                *FZF_FIELDS,
                "--multi",
                "--sync",
                "--layout=reverse",
                "--cycle",
                "--prompt",
                f"{signal_name} [{mode}]> ",
                "--header",
                "Tab: select | Enter: signal PID | →/←: expand/collapse | Ctrl-A: compact/all | Ctrl-R: refresh | Esc: cancel\n"
                "View/refresh changes clear selections. Search hidden processes with Ctrl-A.\n"
                + clean(status)
                + "\nPID        CPU%   MEM%  PROCESS (per-PID metrics)",
                "--preview",
                f"{python} {helper} preview {shlex.quote(str(preview))} {{1}}",
                "--preview-window=down:35%:wrap",
                "--print-query",
                "--query",
                query,
                "--bind",
                f"load:pos({position}),enter:print()+accept,{bindings}",
            ]
            result = subprocess.run(args, input="\n".join(rows) + "\n", text=True, stdout=subprocess.PIPE, env=env)
            if result.returncode == 130:
                return 0
            if result.returncode not in (0, 1):
                return result.returncode
            response = result.stdout.splitlines()
            if len(response) < 2:
                return 0
            query, key = response[:2]
            selected = [row.split("\t", 1)[0] for row in response[2:] if "\t" in row]
            if not key:
                return send_targets(processes, selected, sig)
            focus = selected[-1] if selected else focus
            if key == "ctrl-a":
                mode = "all" if mode == "compact" else "compact"
            elif key == "ctrl-r":
                processes = collect()
                windows, status = window_snapshot()
                recognize(processes, windows)
                expanded.clear()
            elif focus and selected:
                pid = int(focus.split(":")[0])
                if key == "right":
                    expanded.add(pid)
                elif key == "left":
                    collapse(processes, expanded, pid)
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    ui = sub.add_parser("pick")
    ui.add_argument("signal", nargs="?", default="KILL")
    rows = sub.add_parser("rows")
    rows.add_argument("mode", choices=("compact", "all"), nargs="?", default="compact")
    preview = sub.add_parser("preview")
    preview.add_argument("snapshot")
    preview.add_argument("token")
    args = parser.parse_args()
    try:
        if args.command == "preview":
            print(json.loads(Path(args.snapshot).read_text()).get(args.token, "Process absent from snapshot"))
            return 0
        if args.command == "rows":
            processes = collect()
            windows, _ = window_snapshot()
            recognize(processes, windows)
            print("\n".join(render(processes, args.mode, set())))
            return 0
        return picker(parse_signal(args.signal), args.signal)
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"fprocess: {error}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 0


if __name__ == "__main__":
    sys.exit(main())
