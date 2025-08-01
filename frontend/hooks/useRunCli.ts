import { useState } from "react";
import { CliRun } from "../lib/cliRunStore";

export default function useRunCli() {
  const [lastRun, setLastRun] = useState<CliRun | null>(null);

  const run = async (command: string, args: string[] = []) => {
    const res = await fetch("/api/run-cli", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ command, args }),
    });
    const data = (await res.json()) as CliRun;
    setLastRun(data);
    return data;
  };

  return { run, lastRun };
}
