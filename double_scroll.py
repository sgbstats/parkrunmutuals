# python
"""
Double-press Scroll Lock every 60 seconds with safe stop controls.

Stop the run with:
 - Ctrl+C
 - kill <pid>   (PID written to ./double_scroll.pid)
 - Type `q` (or quit/exit/stop) and press Enter in the same terminal
"""
import argparse
import re
import time
import logging
import os
import sys
import signal
from threading import Event, Thread
from pynput.keyboard import Controller, Key

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s: %(message)s")


def parse_duration(s: str) -> int:
    m = re.fullmatch(r"(\d+):([0-5][0-9])", s)
    if not m:
        raise argparse.ArgumentTypeError("duration must be in HH:MM with minutes 00-59")
    hours, minutes = int(m.group(1)), int(m.group(2))
    total = hours * 3600 + minutes * 60
    if total <= 0:
        raise argparse.ArgumentTypeError("duration must be > 00:00")
    return total


def double_press_scroll_lock(kb: Controller, gap: float = 0.12) -> None:
    kb.press(Key.scroll_lock)
    kb.release(Key.scroll_lock)
    time.sleep(gap)
    kb.press(Key.scroll_lock)
    kb.release(Key.scroll_lock)


def stdin_watcher(stop_event: Event) -> None:
    """Run in background: read lines from stdin and set stop on recognized commands."""
    while not stop_event.is_set():
        line = sys.stdin.readline()
        if not line:
            # EOF reached (terminal closed input); exit watcher
            break
        cmd = line.strip().lower()
        if cmd in ("q", "quit", "exit", "stop"):
            logging.info("Stop command received from stdin")
            stop_event.set()
            break


def main():
    p = argparse.ArgumentParser(description="Double-press Scroll Lock every 60 seconds.")
    p.add_argument(
        "duration",
        nargs="?",
        help="run time in HH:MM (hours:minutes). Defaults to 08:00 if omitted.",
        default=None,
    )
    args = p.parse_args()

    total_seconds = 8 * 3600 if args.duration is None else parse_duration(args.duration)

    stop = Event()

    # signal handlers for graceful shutdown (SIGINT and SIGTERM)
    def _handle_signal(signum, frame):
        logging.info("Signal %s received, stopping", signal.Signals(signum).name)
        stop.set()

    try:
        signal.signal(signal.SIGINT, _handle_signal)
    except Exception:
        # some environments (rare) may not allow setting SIGINT; ignore if that happens
        pass
    try:
        signal.signal(signal.SIGTERM, _handle_signal)
    except Exception:
        # SIGTERM may not exist on Windows; ignore if unavailable
        pass

    # write PID file so external 'kill' can target this process
    pid_file = os.path.abspath("double_scroll.pid")
    pid = os.getpid()
    try:
        with open(pid_file, "w") as f:
            f.write(str(pid))
    except Exception as e:
        logging.warning("Could not write PID file %s: %s", pid_file, e)
        pid_file = None

    logging.info("Starting for %d hours. PID=%d", round(total_seconds/(60*60), 1), pid)
    #if pid_file:
        #logging.info("PID file: %s", pid_file)
    logging.info("Stop with Ctrl+C, kill %d, or type q + Enter in this terminal", pid)

    # start stdin watcher thread (daemon so it won't block exit)
    t = Thread(target=stdin_watcher, args=(stop,), daemon=True)
    t.start()

    kb = Controller()
    end_time = time.monotonic() + total_seconds

    try:
        # first press immediately, then repeat every 60s until time elapses or stop is set
        while time.monotonic() < end_time and not stop.is_set():
            double_press_scroll_lock(kb)
            remaining = end_time - time.monotonic()
            if remaining <= 0 or stop.is_set():
                break
            sleep_time = min(60.0, remaining)
            # responsive sleep loop
            slept = 0.0
            while slept < sleep_time and not stop.is_set():
                to_sleep = min(1.0, sleep_time - slept)
                time.sleep(to_sleep)
                slept += to_sleep
    except KeyboardInterrupt:
        logging.info("Interrupted by user (KeyboardInterrupt)")
        stop.set()
    finally:
        # clean up pid file
        if pid_file:
            try:
                os.remove(pid_file)
            except Exception:
                pass
        logging.info("Finished")


if __name__ == "__main__":
    main()