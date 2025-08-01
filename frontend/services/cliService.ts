import { CliRun, listRuns, recordRun } from "../lib/cliRunStore";
import { runCommand } from "../lib/runCommand";

export function prepareCommand(command: string, args: string[] = []) {
  return { command, args };
}

export async function executeCommand({
  command,
  args,
}: {
  command: string;
  args: string[];
}) {
  const result = await runCommand(command, args);
  const run = await recordRun({
    command,
    args,
    stdout: result.stdout,
    stderr: result.stderr,
    exitCode: result.code,
  });
  return run;
}

export async function getRuns(): Promise<CliRun[]> {
  return listRuns();
}
