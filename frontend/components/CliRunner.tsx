import { FormEvent, useState } from "react";
import useRunCli from "../hooks/useRunCli";

export default function CliRunner() {
  const { run, lastRun } = useRunCli();
  const [command, setCommand] = useState("echo");
  const [args, setArgs] = useState("hello");

  const onSubmit = async (e: FormEvent) => {
    e.preventDefault();
    const argArray = args.trim() ? args.split(" ") : [];
    await run(command, argArray);
  };

  return (
    <div>
      <form onSubmit={onSubmit}>
        <input
          value={command}
          onChange={(e) => setCommand(e.target.value)}
          placeholder="Command"
        />
        <input
          value={args}
          onChange={(e) => setArgs(e.target.value)}
          placeholder="Args"
        />
        <button type="submit">Run</button>
      </form>
      {lastRun && <pre>{lastRun.stdout || lastRun.stderr}</pre>}
    </div>
  );
}
